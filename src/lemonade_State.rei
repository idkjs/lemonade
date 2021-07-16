/* Lemonade_State -- The classic state monad

   Lemonade (https://github.com/michipili/lemonade)
   This file is part of Lemonade

   Copyright © 2013–2015 Michael Grünewald

   This file must be used under the terms of the CeCILL-B.
   This source file is licensed as described in the file COPYING, which
   you should have received as part of this distribution. The terms
   are also available at
   http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt */

/** State monad. */;

/** The input signature of the functor [Lemonade_State.Make]. */

module type StateType = {
  /** The type of states carried by the monad. */

  type t;
};

/** The output signature of the functor [Lemonade_State.Make]. */

module type S = {
  /** The type of states carried by the monad. */

  type state;

  /** The type of computations of an ['a], carrying a state of type
      [state]. */

  type t(+'a);

  include Lemonade_Type.S with type t('a) := t('a);

  /** Embed a simple state action in the monad. */

  let state: (state => (state, 'a)) => t('a);

  /** Return the state from the internals of the monad. */

  let read: t(state);

  /** Replace the state inside the monad. */

  let write: state => t(unit);

  /** Maps an old state to a new state inside a state monad. The old
      state is discarded. */

  let modify: (state => state) => t(unit);

  /** Unwrap a computation in the state monad as a function. (The
      converse of state.). */

  let run: (t('a), state) => (state, 'a);

  /** Evaluate a state computation with the given initial state and
      return the final value, discarding the final state. */

  let eval: (t('a), state) => 'a;

  /** Evaluate a state computation with the given initial state and
      return the final state, discarding the final value. */

  let exec: (t('a), state) => state;

  /** Map both the return value and final state of a computation using
      the given function.

      {b Note:} The derivation of the notation [maps] is similar to
      the derivation of the notation [mapi] from the standard
      library.*/

  let maps: (((state, 'a)) => (state, 'b), t('a)) => t('b);

  /** [with_state f m] is the monad executing action [m] on a state
      modified by applying [f]. */

  let with_state: (state => state, t('a)) => t('a);
};

/** Functor building an implementation of the [State] monad. */

module Make:
  (State: StateType) =>
   {
    include S with type state = State.t;

    /** The state monad transformer. */

    module T:
      (M: Lemonade_Type.S) =>
       {
        type state = State.t;
        include Lemonade_Type.S;

        /** Embed a simple state action in the monad. */

        let state: (state => (state, 'a)) => t('a);

        /** Return the state from the internals of the monad. */

        let read: t(state);

        /** Replace the state inside the monad. */

        let write: state => t(unit);

        /** Maps an old state to a new state inside a state monad. The old
        state is discarded. */

        let modify: (state => state) => t(unit);

        /** Unwrap a computation in the state monad as a function. (The
        converse of state.). */

        let run: (t('a), state) => M.t((state, 'a));

        /** Evaluate a state computation with the given initial state and
        return the final value, discarding the final state. */

        let eval: (t('a), state) => M.t('a);

        /** Evaluate a state computation with the given initial state and
        return the final state, discarding the final value. */

        let exec: (t('a), state) => M.t(state);

        /** Map both the return value and final state of a computation using
        the given function.

        {b Note:} The derivation of the notation [maps] is similar to
        the derivation of the notation [mapi] from the standard
        library.*/

        let maps: (((state, 'a)) => (state, 'b), t('a)) => t('b);

        /** [with_state f m] is the monad executing action [m] on a state
        modified by applying [f]. */

        let with_state: (state => state, t('a)) => t('a);

        /** Embed the monad [M] in the associated state monad. */

        let lift: M.t('a) => t('a);
      };
  };
