/* Lemonade_Ok -- A variant of the success monad

   Lemonade (https://github.com/michipili/lemonade)
   This file is part of Lemonade

   Copyright © 2013–2015 Michael Grünewald

   This file must be used under the terms of the CeCILL-B.
   This source file is licensed as described in the file COPYING, which
   you should have received as part of this distribution. The terms
   are also available at
   http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt */

/** A widely spread variant of the success monad.

It is mainly useful when working with Yojson. */;

/** The type of monads computing a value of type ['a] or failing with
    an error message. */

type t(+'a) = [ | `Error(string) | `Ok('a)];

include Lemonade_Type.S with type t('a) := t('a);

/** A computation failed with the given error message. */

let error: string => t('a);

/** A computation failed with the given error message, formatted by sprintf. */

let errorf: format4('a, unit, string, t('b)) => 'a;

/** [run m] return the value computed by [m] if [m] succeeded or throw
    a [Failure] exception with the given message otherwise. */

let run: t('a) => 'a;

/** A generic printer for monadic values. */

let pp_print:
  ((Format.formatter, 'a) => unit, Format.formatter, t('a)) => unit;

/** The maybe monad transformer. */

module T:
  (M: Lemonade_Type.S) =>
   {
    include
      Lemonade_Type.S with type t('a) = M.t([ | `Error(string) | `Ok('a)]);

    let lift: M.t('a) => t('a);
  };
