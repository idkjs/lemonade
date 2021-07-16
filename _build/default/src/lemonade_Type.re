/* Lemonade_Type -- The classic type monad

   Lemonade (https://github.com/michipili/lemonade)
   This file is part of Lemonade

   Copyright © 2013–2015 Michael Grünewald

   This file must be used under the terms of the CeCILL-B.
   This source file is licensed as described in the file COPYING, which
   you should have received as part of this distribution. The terms
   are also available at
   http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt */

module type S = {
  type t(+'a);
  let bind: (t('a), 'a => t('b)) => t('b);
  let return: 'a => t('a);
  let apply: (t('a => 'b), t('a)) => t('b);
  let join: t(t('a)) => t('a);
  let map: ('a => 'b, t('a)) => t('b);
  let bind2: (t('a), t('b), ('a, 'b) => t('c)) => t('c);
  let bind3: (t('a), t('b), t('c), ('a, 'b, 'c) => t('d)) => t('d);
  let bind4:
    (t('a), t('b), t('c), t('d), ('a, 'b, 'c, 'd) => t('e)) => t('e);
  let map2: (('a, 'b) => 'c, t('a), t('b)) => t('c);
  let map3: (('a, 'b, 'c) => 'd, t('a), t('b), t('c)) => t('d);
  let map4: (('a, 'b, 'c, 'd) => 'e, t('a), t('b), t('c), t('d)) => t('e);
  let dist: list(t('a)) => t(list('a));
  let ignore: t('a) => t(unit);
  let filter: ('a => t(bool), list(t('a))) => t(list('a));
  let only_if: (bool, t(unit)) => t(unit);
  let unless: (bool, t(unit)) => t(unit);
  let catch: (unit => t('a), exn => t('a)) => t('a);
  module Infix: {
    let (<*>): (t('a => 'b), t('a)) => t('b);
    let (<$>): ('a => 'b, t('a)) => t('b);
    let ( <* ): (t('a), t('b)) => t('a);
    let ( >* ): (t('a), t('b)) => t('b);
    let (>>=): (t('a), 'a => t('b)) => t('b);
    let (>|=): (t('a), 'a => 'b) => t('b);
    let (>>): (t('a), unit => t('b)) => t('b);
    let (>=>): ('a => t('b), 'b => t('c), 'a) => t('c);
    let (<=<): ('b => t('c), 'a => t('b), 'a) => t('c);
  };
};
