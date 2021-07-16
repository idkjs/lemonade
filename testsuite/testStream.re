/* TestStream -- Test monadic streams

   Lemonade (https://github.com/michipili/lemonade)
   This file is part of Lemonade

   Copyright © 2013–2016 Michael Grünewald

   This file must be used under the terms of the CeCILL-B.
   This source file is licensed as described in the file COPYING, which
   you should have received as part of this distribution. The terms
   are also available at
   http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt */

open Broken;
open Format;

module Success =
  Lemonade_Success.Make({
    type t = string;
  });

module SStream = Lemonade_Stream.Make(Success);

let pp_print_outcome = (f, pp, outcome) =>
  Success.(
    switch (outcome) {
    | Success(x) => fprintf(pp, "Success(%a)", f, x)
    | Error(mesg) => fprintf(pp, "Error(%S)", mesg)
    }
  );

let assert_success_int = (id, ~expected_failure=?, f, a, b) =>
  assert_equal(
    id,
    ~expected_failure?,
    ~printer=pp_print_outcome(pp_print_int),
    ~equal=(==),
    x => Success.run(f(x)),
    a,
    b,
  );

let enumerate = n =>
  SStream.from(
    Success.(
      i =>
        if (i >= 0 && i < n) {
          return(Some(i));
        } else {
          return(None);
        }
    ),
  );

let fail = n =>
  SStream.from(
    Success.(
      i =>
        if (i >= 0 && i < n) {
          return(Some(i));
        } else {
          error("Error");
        }
    ),
  );

let () =
  register_suite(
    "stream",
    "Test monadic streams",
    [
      assert_success_int(
        "enumerate",
        () => SStream.fold((+), enumerate(10), 0),
        (),
        Success.Success(45),
      ),
      assert_success_int(
        "fail",
        () => SStream.fold((+), fail(10), 0),
        (),
        Success.Error("Error"),
      ),
      assert_success_int(
        "map",
        () => SStream.fold((+), SStream.map(x => 2 * x, enumerate(10)), 0),
        (),
        Success.Success(90),
      ),
      assert_success_int(
        "npeek",
        () => Success.map(List.length, SStream.npeek(15, enumerate(10))),
        (),
        Success.Success(10),
      ),
      assert_success_int(
        "concat",
        () => {
          let pyramid = n =>
            SStream.from(
              Success.(
                i =>
                  if (i >= 0 && i < n) {
                    return(Some(enumerate(i)));
                  } else {
                    return(None);
                  }
              ),
            );

          SStream.fold((+), SStream.concat(pyramid(5)), 0);
        },
        (),
        Success.Success(3 + 2 + 1 + 2 + 1 + 1),
      ),
      assert_success_int(
        "filter_map",
        () => {
          let stream =
            SStream.filter_map(
              n =>
                if (n mod 2 == 0) {
                  Some(n);
                } else {
                  None;
                },
              enumerate(10),
            );

          SStream.fold((+), stream, 0);
        },
        (),
        Success.Success(2 + 4 + 6 + 8),
      ),
      assert_success_int(
        "of_list",
        () => SStream.fold((+), SStream.of_list([1, 2, 3, 4, 5]), 0),
        (),
        Success.Success(15),
      ),
      assert_success_int(
        "filter",
        () =>
          SStream.fold(
            (+),
            SStream.filter(
              x => x mod 2 == 1,
              SStream.of_list([1, 2, 3, 4, 5]),
            ),
            0,
          ),
        (),
        Success.Success(9),
      ),
    ],
  );
