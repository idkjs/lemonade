/* Lemonade_Reader -- The classic reader monad

   Lemonade (https://github.com/michipili/lemonade)
   This file is part of Lemonade

   Copyright © 2013–2015 Michael Grünewald

   This file must be used under the terms of the CeCILL-B.
   This source file is licensed as described in the file COPYING, which
   you should have received as part of this distribution. The terms
   are also available at
   http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt */

module type EnvironmentType = {type t;};

module type S = {
  type environment;
  include Lemonade_Type.S;
  let read: t(environment);
  let run: (environment, t('a)) => 'a;
  let local: (environment => environment, t('a)) => t('a);
  let access: (environment => 'a) => t('a);
};

module Make = (Environment: EnvironmentType) => {
  type environment = Environment.t;
  module Basis = {
    type t('a) = environment => 'a;

    let return = (x, _) => x;

    let bind = (m, f, env) => f(m(env), env);
  };

  module MethodsMonad = Mixture.Mixture_Monad.Make(Basis);

  include Basis;
  include MethodsMonad;

  let read = env => env;

  let run = (env, m) => m(env);

  let local = (f, m, env) => m(f(env));

  let access = (f, env) => f(env);

  module T = (M: Lemonade_Type.S) => {
    type environment = Environment.t;

    module BasisT = {
      type t('a) = environment => M.t('a);
      let return = (x, _) => M.return(x);
      let bind = (m, f, env) => M.bind(m(env), x => f(x, env));
    };

    module MethodsMonadT = Mixture.Mixture_Monad.Make(BasisT);

    include BasisT;
    include MethodsMonadT;

    let read = env => M.return(env);

    let run = (env, m) => m(env);

    let local = (f, m, env) => m(f(env));

    let access = (f, env) => M.return(f(env));

    let lift = (m, _) => m;
  };
};
