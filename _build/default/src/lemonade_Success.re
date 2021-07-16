/* Lemonade_Success -- The classic success monad

   Lemonade (https://github.com/michipili/lemonade)
   This file is part of Lemonade

   Copyright © 2013–2015 Michael Grünewald

   This file must be used under the terms of the CeCILL-B.
   This source file is licensed as described in the file COPYING, which
   you should have received as part of this distribution. The terms
   are also available at
   http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt */

module type ErrorType = {type t;};

module type S = {
  include Mixture_Monad.S;
  type error;
  type outcome(+'a) =
    | Success('a)
    | Error(error);
  let error: error => t('a);
  let recover: (t('a), error => t('a)) => t('a);
  let run: t('a) => outcome('a);
};

module Make = (Error: ErrorType) => {
  type error = Error.t;

  type outcome(+'a) =
    | Success('a)
    | Error(error);

  module Basis = {
    type t('a) = outcome('a);

    let bind = (succ, f) =>
      switch (succ) {
      | Success(x) => f(x)
      | Error(s) => Error(s)
      };

    let return = x => Success(x);
  };

  module MethodsMonad = Mixture_Monad.Make(Basis);

  include Basis;
  include MethodsMonad;

  let error = err => Error(err);

  let recover = (m, handler) =>
    switch (m) {
    | Success(x) => Success(x)
    | Error(err) => handler(err)
    };

  let run = m => m;

  module T = (M: Mixture_Monad.S) => {
    module Germ = {
      type t('a) = M.t(Basis.t('a));

      let bind = (m, f) =>
        M.bind(
          m,
          fun
          | Success(x) => f(x)
          | Error(err) => M.return(Error(err)),
        );

      let return = x => M.return(Success(x));
    };

    include Mixture_Monad.Transformer.Make(Basis, M, Germ);
  };
};
