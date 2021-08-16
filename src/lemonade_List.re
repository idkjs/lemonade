/* Lemonade_List -- The classic list monad

   Lemonade (https://github.com/michipili/lemonade)
   This file is part of Lemonade

   Copyright © 2013–2015 Michael Grünewald

   This file must be used under the terms of the CeCILL-B.
   This source file is licensed as described in the file COPYING, which
   you should have received as part of this distribution. The terms
   are also available at
   http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt */

module Basis = {
  type t(+'a) = list('a);

  let bind = (lst, f) => List.concat(List.map(f, lst));

  let return = x => [x];
};

module MethodsMonad = Mixture.Mixture_Monad.Make(Basis);

include Basis;
include MethodsMonad;

let pp_print = (f, ff, lst) => {
  open Format;
  let flag = ref(false);
  let loop = item => {
    if (flag^) {
      fprintf(ff, ";@ ");
    };
    flag := true;
    fprintf(ff, "%a", f, item);
  };

  fprintf(ff, "@[<hov 1>[");
  List.iter(loop, lst);
  fprintf(ff, "]@]");
};

module T = (M: Mixture.Mixture_Monad.S) => {
  module Germ = {
    type t('a) = M.t(list('a));

    let bind = (m, f) =>
      M.bind(m, lst => (M.map(List.concat))(M.dist(List.map(f, lst))));
    let return = x => M.return([x]);
  };

  include Mixture.Mixture_Monad.Transformer.Make(Basis, M, Germ);
};
