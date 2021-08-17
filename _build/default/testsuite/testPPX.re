/** TestPPX -- Test Preprocessor

   Mixture (https://github.com/michipili/mixture)
   This file is part of Mixture

   Copyright © 2013–2015 Michael Grünewald

   This file must be used under the terms of the CeCILL-B.
   This source file is licensed as described in the file COPYING, which
   you should have received as part of this distribution. The terms
   are also available at
   http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt */

open Printf;
open Broken;
open Lemonade;
module Maybe = Lemonade_Maybe;

let pp_print_maybe_bool = (pp, m) =>
  Lemonade_Maybe.pp_print(Format.pp_print_bool, pp, m);

let assert_maybe = (id, ~expected_failure=?, f) =>
  assert_equal(
    id,
    ~expected_failure?,
    ~printer=pp_print_maybe_bool,
    ~equal=(==),
    f,
    (),
    Some(true),
  );

let maybe_assoc = (lst, name) =>
  try(Some(List.assoc(name, lst))) {
  | Not_found => None
  };

let () =
  Maybe.(
    register_suite(
      "ppx",
      "Test Lemonade PPX rewriter",
      [
        assert_maybe(
          "let%lemonade",
          fun
          | () => {
              let%lemonade a = Some(true);
              return(a);
            },
        ),
        assert_maybe(
          "nested let%lemonade",
          fun
          | () => {
              let%lemonade a = Some(1);
              let%lemonade b = Some(1);
              return(a == b);
            },
        ),
        assert_maybe(
          "and let%lemonade",
          fun
          | () => {
              let%lemonade a = Some(1)
              and b = Some(1);
              return(a == b);
            },
        ),
        assert_maybe(
          "match%lemonade",
          fun
          | () => {
              let x = return(Some(0));
              switch%lemonade (x) {
              | Some(x) => return(x + 1 == 1)
              | None => return(false)
              };
            },
        ),
        assert_maybe(
          "match-exn",
          fun
          | () => {
              let x = return(Some(3));
              let%lemonade a =
                switch%lemonade (x) {
                | exception Not_found => return(false)
                | Some(x) => return(x == 3)
                | None => return(false)
                }
              and b =
                switch%lemonade (raise(Not_found)) {
                | exception Not_found => return(true)
                | _ => return(false)
                };

              return(a && b);
            },
        ),
        assert_maybe(
          "if%lemonade",
          fun
          | () => {
              open Maybe.Infix;
              let x = return(true);
              let%lemonade a =
                if%lemonade (x) {
                  return(true);
                } else {
                  return(false);
                };

              let%lemonade b =
                if%lemonade (x >|= (!)) {
                  return(false);
                } else {
                  return(true);
                };

              (
                if%lemonade (x >|= (!)) {
                  return();
                }
              )
              >>= (() => return(a && b));
            },
        ),
        assert_maybe(
          "for%lemonade", /* Test for proper sequencing */
          fun
          | () => {
              let r = ref([]);
              let f = x => return(r := [x, ...r^]);

              let%lemonade () =
                for%lemonade (x in 3 to 5) {
                  f(x);
                };
              return(r^ == [5, 4, 3]);
            },
        ),
        assert_maybe(
          "while%lemonade", /* Test for proper sequencing */
          fun
          | () => {
              let r = ref([]);
              let f = x => return(r := [x, ...r^]);

              let%lemonade () = {
                let c = ref(2);
                while%lemonade (c^ < 5) {
                  incr(c);
                  f(c^);
                };
              };
              return(r^ == [5, 4, 3]);
            },
        ),
        /*    assert_maybe "assert%lemonade"
                begin function () ->
                  let%lemonade () = assert%lemonade true
                  in return true
                end;

              assert_maybe "sequence"
                begin function () ->
                  let lst = ref [] in
                  (lst := 2 :: !lst; return()) >>
                  (lst := 1 :: !lst; return()) >>
                  (return (!lst = [1;2]))
                end;

                assert_maybe "structure let"
                begin function () ->
                  let module M =
                  struct
                    let%lemonade result = return true
                  end
                  in
                  return M.result
                end; */
      ],
    )
  );
