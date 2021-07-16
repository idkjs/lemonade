/* Lemonade_State -- The classic state monad

   Lemonade (https://github.com/michipili/lemonade)
   This file is part of Lemonade

   Copyright © 2013–2015 Michael Grünewald

   This file must be used under the terms of the CeCILL-B.
   This source file is licensed as described in the file COPYING, which
   you should have received as part of this distribution. The terms
   are also available at
   http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt */

module type StateType = {type t;};

module type S = {
  type state;
  type t(+'a);
  include Lemonade_Type.S with type t('a) := t('a);
  let state: (state => (state, 'a)) => t('a);
  let read: t(state);
  let write: state => t(unit);
  let modify: (state => state) => t(unit);
  let run: (t('a), state) => (state, 'a);
  let eval: (t('a), state) => 'a;
  let exec: (t('a), state) => state;
  let maps: (((state, 'a)) => (state, 'b), t('a)) => t('b);
  let with_state: (state => state, t('a)) => t('a);
};

module Make = (State: StateType) => {
  type state = State.t;
  module Basis = {
    type t('a) = state => (state, 'a);

    let return = (x, s) => (s, x);

    let bind = (m, f, state) => {
      let (state', x) = m(state);
      f(x, state');
    };
  };

  module MethodsMonad = Mixture_Monad.Make(Basis);

  include Basis;
  include MethodsMonad;

  let state = m => m;

  let read = state => (state, state);

  let write = (x, _) => (x, ());

  let modify = f => bind(read, s => write(f(s)));

  let run = (m, state) => m(state);

  let eval = (m, state) => snd(m(state));

  let exec = (m, state) => fst(m(state));

  let maps = (f, m, state) => f(m(state));

  let with_state = (f, m, state) => m(f(state));

  module T = (M: Lemonade_Type.S) => {
    type state = State.t;

    module BasisT = {
      type t('a) = state => M.t((state, 'a));

      let return = (x, s) => M.return((s, x));

      let bind = (m, f, state) =>
        M.bind(m(state), ((state', x)) => (f(x))(state'));
    };

    module MethodsMonadT = Mixture_Monad.Make(BasisT);

    include BasisT;
    include MethodsMonadT;

    let state = (m, state) => M.return(m(state));

    let read = state => M.return((state, state));

    let write = (x, state) => M.return((x, ()));

    let modify = f => bind(read, s => write(f(s)));

    let run = (m, state) => m(state);

    let eval = (m, state) => M.map(snd, m(state));

    let exec = (m, state) => M.map(fst, m(state));

    let maps = (f, m, state) => (M.map(f))(m(state));

    let with_state = (f, m, state) => m(f(state));

    let lift = (m, state) => M.map(x => (state, x), m);
  };
};
