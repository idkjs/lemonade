/* Lemonade_Retry -- The retry monad

   Lemonade (https://github.com/michipili/lemonade)
   This file is part of Lemonade

   Copyright © 2013–2015 Michael Grünewald

   This file must be used under the terms of the CeCILL-B.
   This source file is licensed as described in the file COPYING, which
   you should have received as part of this distribution. The terms
   are also available at
   http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt */

module type RetryType = {
  type error;
  type tag;
  type environment;
  let policy: (list((tag, 'a)), environment) => list((tag, 'a));
};

module type S = {
  type error;
  type tag;
  type environment;
  type outcome(+'a) =
    | Success('a)
    | Error(error);

  include Mixture_Monad.S;

  let throw: error => t('a);
  let catch: (t('a), error => t('a)) => t('a);
  let retry: (tag, environment => t('a), t('a)) => t('a);
  let run: (environment, t('a)) => outcome('a);
};

module Make = (Retry: RetryType) => {
  type error = Retry.error;

  type tag = Retry.tag;

  type environment = Retry.environment;

  type outcome(+'a) =
    | Success('a)
    | Error(error);

  module Basis = {
    type t(+'a) =
      | SUCCESS('a)
      | ERROR(list((tag, environment => t('a))), error);

    let return = x => SUCCESS(x);

    let rec bind = (m, f) =>
      switch (m) {
      | SUCCESS(x) => f(x)
      | [@implicit_arity] ERROR(plan, err) =>
        [@implicit_arity] ERROR(List.map(bind_strategy(f), plan), err)
      }
    and bind_strategy = (f, (tag, strategy)) => (
      tag,
      env => bind(strategy(env), f),
    );
  };

  module MethodsMonad = Mixture_Monad.Make(Basis);

  include Basis;
  include MethodsMonad;

  let throw = err => [@implicit_arity] ERROR([], err);

  let catch = (m, handler) =>
    switch (m) {
    | SUCCESS(x) => SUCCESS(x)
    | [@implicit_arity] ERROR(_, err) => handler(err)
    };

  let retry = (tag, f) =>
    fun
    | [@implicit_arity] ERROR(plan, err) =>
      [@implicit_arity] ERROR([(tag, f), ...plan], err)
    | whatever => whatever;

  let rec run = env =>
    fun
    | SUCCESS(x) => Success(x)
    | [@implicit_arity] ERROR([], err) => Error(err)
    | [@implicit_arity] ERROR(plan, err) =>
      _run(env, err, Retry.policy(plan, env))
  and _run = (env, err) =>
    fun
    | [] => Error(err)
    | [(_, f), ...tl] =>
      switch (run(env, f(env))) {
      | Error(_) => run(env, [@implicit_arity] ERROR(tl, err))
      | whatever => whatever
      };
};
