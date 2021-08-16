/* Lemonade_Lazy -- The classic lazy monad

   Lemonade (https://github.com/michipili/lemonade)
   This file is part of Lemonade

   Copyright © 2013–2015 Michael Grünewald

   This file must be used under the terms of the CeCILL-B.
   This source file is licensed as described in the file COPYING, which
   you should have received as part of this distribution. The terms
   are also available at
   http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt */

module Basis = {
  type t(+'a) = Lazy.t('a);

  let bind = (m, f) => lazy(Lazy.force(f(Lazy.force(m))));

  let return = x => lazy(x);
};

module MethodsMonad = Mixture.Mixture_Monad.Make(Basis);

include Basis;
include MethodsMonad;

let exec = m => Lazy.force(m);

let pp_print = (f, pp, m) =>
  Format.(
    if (Lazy.is_val(m)) {
      fprintf(pp, "Lazy(%a)", f, Lazy.force(m));
    } else {
      fprintf(pp, "Lazy(<deferred>)");
    }
  );

module T = (M: Mixture.Mixture_Monad.S) => {
  module Germ = {
    type t('a) = M.t(Lazy.t('a));

    let bind = (m, f) => M.bind(m, x => f(Lazy.force(x)));

    let return = x => M.return(lazy(x));
  };

  include Mixture.Mixture_Monad.Transformer.Make(Basis, M, Germ);
};
