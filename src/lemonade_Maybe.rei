/* Lemonade_Maybe -- The classic maybe monad

   Lemonade (https://github.com/michipili/lemonade)
   This file is part of Lemonade

   Copyright © 2013–2015 Michael Grünewald

   This file must be used under the terms of the CeCILL-B.
   This source file is licensed as described in the file COPYING, which
   you should have received as part of this distribution. The terms
   are also available at
   http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt */

/** The classic maybe monad. */;

type t(+'a) = option('a);

/** A generic printer for option values. */

let pp_print:
  ((Format.formatter, 'a) => unit, Format.formatter, option('a)) => unit;

/** Predicate recognising options containing a value. */

let is_some: option('a) => bool;

/** Predicate recognising empty options. */

let is_none: option('a) => bool;

/** Return the value held by an option or raise [Not_found] if the
    option is empty. */

let find: option('a) => 'a;

/** [default val opt] return the content of [opt] or [val] if it is empty. */

let default: ('a, option('a)) => 'a;

/** Return an option containing the first element of the list if any,
    or empty if the list is empty. */

let of_list: list('a) => option('a);

/** Return a list containing the element held by the option if any, ot
    the empty list ifthe option is empty. */

let to_list: option('a) => list('a);

/** [filter lst] is the list of values held by the options in [lst]. */

let filter: list(option('a)) => list('a);

/** [filter_map f lst] is the list deduced from [lst] by applying [f]
    to element of the list and retaining the values held by the list. */

let filter_map: ('a => option('b), list('a)) => list('b);

include Lemonade_Type.S with type t('a) := t('a);

/** The maybe monad transformer. */

module T:
  (M: Lemonade_Type.S) =>
   {
    include Lemonade_Type.S with type t('a) = M.t(option('a));

    let lift: M.t('a) => t('a);
  };
