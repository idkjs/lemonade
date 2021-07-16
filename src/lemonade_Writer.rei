/* Lemonade_Writer -- The classic writer monad

   Lemonade (https://github.com/michipili/lemonade)
   This file is part of Lemonade

   Copyright © 2013–2015 Michael Grünewald

   This file must be used under the terms of the CeCILL-B.
   This source file is licensed as described in the file COPYING, which
   you should have received as part of this distribution. The terms
   are also available at
   http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt */

/** Writer monad.

    Values in a {i Writer} monad represent a computation accumulating
    some output. */;

/** The input signature of the functor [Lemonade_Writer.Make]. */

module type OutputType = {
  /** The type of output to accumulate. */

  type t;

  /** The empty output. */

  let empty: unit => t;

  /** Catenate two output. */

  let append: (t, t) => t;
};

/** The output signature of the functor [Lemonade_Writer.Make]. */

module type S = {
  /** The type of output. */

  type output;

  include Lemonade_Type.S;

  /** Embed a simple writer action. */

  let writer: ((output, 'a)) => t('a);

  /** [tell w] is an action that produces the output [w]. */

  let tell: output => t(unit);

  /** [listen m] is an action that executes the action m and adds its
      output to the value of the computation. */

  let listen: t('a) => t((output, 'a));

  /** [pass m] is an action that executes the action [m], which
      returns a value and a function, and returns the value, applying
      the function to the output. */

  let pass: t((output => output, 'a)) => t('a);

  /** [listens f m] is an action that executes the action [m] and adds
      the result of applying [f] to its output to the value of the
      computation. */

  let listens: (output => 'b, t('a)) => t(('a, 'b));

  /** [censor f m] is an action action that executes the computation
      [m] and filters its output with [f], leaving its return values
      unchanged. */

  let censor: (output => output, t('a)) => t('a);

  /** Execute a computation and examine its output and return value. */

  let run: t('a) => (output, 'a);

  /** Execute a computation and examine its return value
      while discarding its output. */

  let eval: t('a) => 'a;

  /** Execute a computation and examine its output while discarding
      its return value. */

  let exec: t('a) => output;
};

/** Functor building an implementation of the [Writer] monad. */

module Make:
  (Output: OutputType) =>
   {
    include S with type output = Output.t;

    /** The writer monad transformer. */

    module T:
      (M: Lemonade_Type.S) =>
       {
        /** The type of consumed data. */

        type output = Output.t;

        include Lemonade_Type.S;

        /** Embed a simple writer action. */

        let writer: ((output, 'a)) => t('a);

        /** [tell w] is an action that produces the output [w]. */

        let tell: output => t(unit);

        /** [listen m] is an action that executes the action m and adds its
        output to the value of the computation. */

        let listen: t('a) => t((output, 'a));

        /** [pass m] is an action that executes the action [m], which
        returns a value and a function, and returns the value, applying
        the function to the output. */

        let pass: t((output => output, 'a)) => t('a);

        /** [listens f m] is an action that executes the action [m] and adds
        the result of applying [f] to its output to the value of the
        computation. */

        let listens: (output => 'b, t('a)) => t(('a, 'b));

        /** [censor f m] is an action action that executes the computation
        [m] and filters its output with [f], leaving its return values
        unchanged. */

        let censor: (output => output, t('a)) => t('a);

        /** Execute a computation and examine its output and return value. */

        let run: t('a) => M.t((output, 'a));

        /** Execute a computation and examine its return value
        while discarding its output. */

        let eval: t('a) => M.t('a);

        /** Execute a computation and examine its output while discarding
        its return value. */

        let exec: t('a) => M.t(output);

        /** Add an environment to a monad of type ['a M.t]. */

        let lift: M.t('a) => t('a);
      };
  };
