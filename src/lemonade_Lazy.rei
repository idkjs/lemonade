/* Lemonade_Lazy -- The classic lazy monad

   Lemonade (https://github.com/michipili/lemonade)
   This file is part of Lemonade

   Copyright © 2013–2015 Michael Grünewald

   This file must be used under the terms of the CeCILL-B.
   This source file is licensed as described in the file COPYING, which
   you should have received as part of this distribution. The terms
   are also available at
   http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt */

/** The classic lazy monad. */;

type t(+'a) = Lazy.t('a);

include Lemonade_Type.S with type t('a) := t('a);

/** Execute the computation. */

let exec: t('a) => 'a;

/** A generic printer for lazy values. */

let pp_print:
  ((Format.formatter, 'a) => unit, Format.formatter, Lazy.t('a)) => unit;

/** The lazy monad transformer. */

module T:
  (M: Lemonade_Type.S) =>
   {
    include Lemonade_Type.S with type t('a) = M.t(Lazy.t('a));

    let lift: M.t('a) => t('a);
  };
