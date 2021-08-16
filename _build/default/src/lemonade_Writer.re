/* Lemonade_Writer -- The classic writer monad

   Lemonade (https://github.com/michipili/lemonade)
   This file is part of Lemonade

   Copyright © 2013–2015 Michael Grünewald

   This file must be used under the terms of the CeCILL-B.
   This source file is licensed as described in the file COPYING, which
   you should have received as part of this distribution. The terms
   are also available at
   http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt */

module type OutputType = {
  type t;
  let empty: unit => t;
  let append: (t, t) => t;
};

module type S = {
  type output;
  include Lemonade_Type.S;
  let writer: ((output, 'a)) => t('a);
  let tell: output => t(unit);
  let listen: t('a) => t((output, 'a));
  let pass: t((output => output, 'a)) => t('a);
  let listens: (output => 'b, t('a)) => t(('a, 'b));
  let censor: (output => output, t('a)) => t('a);
  let run: t('a) => (output, 'a);
  let eval: t('a) => 'a;
  let exec: t('a) => output;
};

module Make = (Output: OutputType) => {
  type output = Output.t;
  module Basis = {
    type t('a) = (output, 'a);
    let return = x => (Output.empty(), x);
    let bind = ((out, x), f) => {
      let (more, y) = f(x);
      (Output.append(out, more), y);
    };
  };

  module MethodsMonad = Mixture.Mixture_Monad.Make(Basis);

  include Basis;
  include MethodsMonad;

  let writer = m => m;

  let tell = out => (out, ());

  let listen = ((out, x) as m) => (out, m);

  let pass = ((out, (filter, x))) => (filter(out), x);

  let listens = (f, (out, x)) => (out, (x, f(out)));

  let censor = (f, (out, x)) => (f(out), x);

  let run = m => m;

  let exec = ((out, _)) => out;

  let eval = ((_, x)) => x;

  module T = (M: Lemonade_Type.S) => {
    type output = Output.t;
    module BasisT = {
      type t('a) = M.t(Basis.t('a));
      let return = x => M.return(Basis.return(x));
      let bind = (m, f) =>
        M.bind(m, ((out, x)) =>
          M.bind(f(x), ((more, y)) =>
            M.return((Output.append(out, more), y))
          )
        );
    };

    module MethodsMonadT = Mixture.Mixture_Monad.Make(BasisT);

    include BasisT;
    include MethodsMonadT;

    let writer = m => M.return(m);

    let tell = out => M.return((out, ()));

    let listen = m => M.bind(m, ((out, x) as m) => M.return((out, m)));

    let pass = m =>
      M.bind(m, ((out, (filter, x))) => M.return((filter(out), x)));

    let listens = (f, m) =>
      M.bind(m, ((out, x)) => M.return((out, (x, f(out)))));

    let censor = (f, m) => M.bind(m, ((out, x)) => M.return((f(out), x)));

    let run = m => m;

    let exec = m => M.map(fst, m);

    let eval = m => M.map(snd, m);

    let lift = m => M.bind(m, x => BasisT.return(x));
  };
};
