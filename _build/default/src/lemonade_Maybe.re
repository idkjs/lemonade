/* Lemonade_Maybe -- The classic maybe monad

   Lemonade (https://github.com/michipili/lemonade)
   This file is part of Lemonade

   Copyright © 2013–2015 Michael Grünewald

   This file must be used under the terms of the CeCILL-B.
   This source file is licensed as described in the file COPYING, which
   you should have received as part of this distribution. The terms
   are also available at
   http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt */

module Basis = {
  type t(+'a) = option('a);

  let bind = (opt, f) =>
    switch (opt) {
    | Some(x) => f(x)
    | None => None
    };

  let return = x => Some(x);
};

let pp_print = (f, pp, m) =>
  Format.(
    switch (m) {
    | Some(x) => fprintf(pp, "Some(%a)", f, x)
    | None => fprintf(pp, "None")
    }
  );

let is_some =
  fun
  | Some(_) => true
  | None => false;

let is_none =
  fun
  | Some(_) => false
  | None => true;

let find =
  fun
  | Some(b) => b
  | None => raise(Not_found);

let default = a =>
  fun
  | Some(b) => b
  | None => a;

let of_list =
  fun
  | [] => None
  | [a, ..._] => Some(a);

let to_list =
  fun
  | Some(a) => [a]
  | None => [];

let filter_map = (f, lst) =>
  List.rev(
    List.fold_left(
      (acc, x) =>
        switch (f(x)) {
        | Some(a) => [a, ...acc]
        | None => acc
        },
      [],
      lst,
    ),
  );

let filter = lst => filter_map(x => x, lst);

module MethodsMonad = Mixture.Mixture_Monad.Make(Basis);

include Basis;
include MethodsMonad;

module T = (M: Mixture.Mixture_Monad.S) => {
  module Germ = {
    type t('a) = M.t(option('a));

    let bind = (m, f) =>
      M.bind(
        m,
        fun
        | None => M.return(None)
        | Some(x) => f(x),
      );

    let return = x => M.return(Some(x));
  };

  include Mixture.Mixture_Monad.Transformer.Make(Basis, M, Germ);
};
