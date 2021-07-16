/* Lemonade_List -- The classic list monad

   Lemonade (https://github.com/michipili/lemonade)
   This file is part of Lemonade

   Copyright © 2013–2015 Michael Grünewald

   This file must be used under the terms of the CeCILL-B.
   This source file is licensed as described in the file COPYING, which
   you should have received as part of this distribution. The terms
   are also available at
   http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt */

/** List monad. */;

type t(+'a) = list('a);

include Lemonade_Type.S with type t('a) := t('a);

/** A generic printer for list values. */

let pp_print:
  ((Format.formatter, 'a) => unit, Format.formatter, list('a)) => unit;

/** The list monad transformer. */

module T:
  (M: Lemonade_Type.S) =>
   {
    include Lemonade_Type.S with type t('a) = M.t(list('a));

    let lift: M.t('a) => t('a);
  };
