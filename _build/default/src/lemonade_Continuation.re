/* Lemonade_Continuation -- The continuation monad

   Lemonade (https://github.com/michipili/lemonade)
   This file is part of Lemonade

   Copyright © 2013–2015 Michael Grünewald

   This file must be used under the terms of the CeCILL-B.
   This source file is licensed as described in the file COPYING, which
   you should have received as part of this distribution. The terms
   are also available at
   http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt */

module type FinalType = {type t;};

module type S = {
  type final;
  type t('a) = ('a => final) => final;
  include Lemonade_Type.S with type t('a) := t('a);
  let call_cc: (('a => t('b)) => t('a)) => t('a);
};

module Make = (Final: FinalType) => {
  type final = Final.t;
  module Basis = {
    type t('a) = ('a => final) => final;

    let return = (x, cont) => cont(x);

    let bind = (m, f, cont) => m(x => (f(x))(cont));
  };

  module MethodsMonad = Mixture.Mixture_Monad.Make(Basis);

  include Basis;
  include MethodsMonad;

  let call_cc = (kont, cont) => kont((x, _) => cont(x), cont);
};
