/* Lemonade_Reader -- The classic reader monad

   Lemonade (https://github.com/michipili/lemonade)
   This file is part of Lemonade

   Copyright © 2013–2015 Michael Grünewald

   This file must be used under the terms of the CeCILL-B.
   This source file is licensed as described in the file COPYING, which
   you should have received as part of this distribution. The terms
   are also available at
   http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt */

/** Reader monad.

    Values in a {i Reader} monad or {i Environment monad} represent a
    computation, which can read values from a shared environment, pass
    values from function to function, and execute sub-computations in
    a modified environment. Using a {i Reader} monad for such
    computations is often clearer and easier than using a {i State}
    monad. */;

/** The input signature of the functor [Lemonade_Reader.Make]. */

module type EnvironmentType = {
  /** The type of data consumed in a reader monad. */

  type t;
};

/** The output signature of the functor [Lemonade_Reader.Make]. */

module type S = {
  /** The type of consumed data. */

  type environment;

  include Lemonade_Type.S;

  /** Access the current environment. */

  let read: t(environment);

  /** Perform a computation in the given environment errors. */

  let run: (environment, t('a)) => 'a;

  /** Execute a computation in a modified environment. */

  let local: (environment => environment, t('a)) => t('a);

  /** Access to a component of the current environment. */

  let access: (environment => 'a) => t('a);
};

/** Functor building an implementation of the [Success] monad. */

module Make:
  (Environment: EnvironmentType) =>
   {
    include S with type environment = Environment.t;

    /** The success monad transformer. */

    module T:
      (M: Lemonade_Type.S) =>
       {
        /** The type of consumed data. */

        type environment = Environment.t;

        include Lemonade_Type.S with type t('a) = t(M.t('a));

        /** Access the current environment. */

        let read: t(environment);

        /** Perform a computation in the given environment errors. */

        let run: (environment, t('a)) => M.t('a);

        /** Execute a computation in a modified environment. */

        let local: (environment => environment, t('a)) => t('a);

        /** Access to a component of the current environment. */

        let access: (environment => 'a) => t('a);

        /** Add an environment to a monad of type ['a M.t]. */

        let lift: M.t('a) => t('a);
      };
  };
