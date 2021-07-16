/* Lemonade_Ok -- A variant of the success monad

   Lemonade (https://github.com/michipili/lemonade)
   This file is part of Lemonade

   Copyright © 2013–2015 Michael Grünewald

   This file must be used under the terms of the CeCILL-B.
   This source file is licensed as described in the file COPYING, which
   you should have received as part of this distribution. The terms
   are also available at
   http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt */
open Printf;

module Basis: {
  type t('a) = [ | `Ok('a) | `Error(string)];
  let bind: (t('a), 'a => t('b)) => t('b);
  let return: 'a => t('a);
} = {
  type t('a) = [ | `Ok('a) | `Error(string)];

  let bind = (m, f: 'a => t('b)) =>
    switch (m) {
    | `Ok(x) => f(x)
    | `Error(_) as error => error
    };

  let return = x => `Ok(x);
};

module Methods = Mixture_Monad.Make(Basis);

include Basis;
include Methods;

let run =
  fun
  | `Ok(whatever) => whatever
  | `Error(mesg) => ksprintf(failwith, "Error: %s", mesg);

let error = mesg => `Error(mesg);

let errorf = fmt => ksprintf(mesg => `Error(mesg), fmt);

let pp_print = (f, pp) =>
  fun
  | `Ok(x) => Format.fprintf(pp, "`Ok(%a)", f, x)
  | `Error(mesg) => Format.fprintf(pp, "`Error(%S)", mesg);

module T = (M: Mixture_Monad.S) => {
  module Germ: {
    type t('a) = M.t([ | `Ok('a) | `Error(string)]);
    let bind: (t('a), 'a => t('b)) => t('b);
    let return: 'a => t('a);
  } = {
    type t('a) = M.t(Basis.t('a));

    let bind = (m, f) =>
      M.bind(
        m,
        fun
        | `Ok(x) => f(x)
        | `Error(err) => M.return(`Error(err)),
      );

    let return = x => M.return(`Ok(x));
  };

  include Mixture_Monad.Transformer.Make(Basis, M, Germ);
};
