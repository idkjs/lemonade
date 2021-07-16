/* Lemonade_Retry -- The retry monad

   Lemonade (https://github.com/michipili/lemonade)
   This file is part of Lemonade

   Copyright © 2013–2015 Michael Grünewald

   This file must be used under the terms of the CeCILL-B.
   This source file is licensed as described in the file COPYING, which
   you should have received as part of this distribution. The terms
   are also available at
   http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt */

/** Retry monad.

    The retry monad is a monad in which one can run computations
    throwing errors, which can be handled by retry strategies
    according to a policy. This is implemented as a functor
    parametrised by the error type and the policy.

    Note that the [throw] and [catch] operations defined below are
    totally independant of exceptions. */;

/** The input signature of the functor [Lemonade_Retry.Make]. */

module type RetryType = {
  /** The type of error messages. */

  type error;

  /** The type of tags identifying retry strategies. */

  type tag;

  /** The type of computation environment. */

  type environment;

  /** The type of policies, used to select a retry strategy.

      The policy filters applyable retry strategies under a given
      environment. */

  let policy: (list((tag, 'a)), environment) => list((tag, 'a));
};

/** The output signature of the functor [Lemonade_Retry.Make]. */

module type S = {
  /** The type of error messages. */

  type error;

  /** The type of tags identifying retry strategies. */

  type tag;

  /** The type of computation environment. */

  type environment;

  /** The type of computations throwing errors. */

  type outcome(+'a) =
    | Success('a)
    | Error(error);

  include Lemonade_Type.S;

  /** Throw the given error. */

  let throw: error => t('a);

  /** [catch m handler] is a monad containing the same value as [m]
      and thrown errors are interepreted by the [handler]. */

  let catch: (t('a), error => t('a)) => t('a);

  /** [retry tag strategy m] compute the same value as [m] having the
      chance let the retry policy use [strategy] on errors. */

  let retry: (tag, environment => t('a), t('a)) => t('a);

  /** Run the given retryable computation. */

  let run: (environment, t('a)) => outcome('a);
};

/** Functor building an implementation of the [Retry] monad. */

module Make:
  (Retry: RetryType) =>

    S with
      type error = Retry.error and
      type tag = Retry.tag and
      type environment = Retry.environment;
