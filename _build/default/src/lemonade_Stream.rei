/* Lemonade_Stream -- Monadic streams

   Lemonade (https://github.com/michipili/lemonade)
   This file is part of Lemonade

   Copyright © 2013–2016 Michael Grünewald

   This file must be used under the terms of the CeCILL-B.
   This source file is licensed as described in the file COPYING, which
   you should have received as part of this distribution. The terms
   are also available at
   http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt */

/** Monadic streams. */;

/** Exception raised when trying to read from an empty stream. */

exception Empty;

/** The output signature of the functor [Lemonade_Stream.Make]. */

module type S = {
  /** The type of streams, holding values of type ['a]. */

  type t('a);

  /** The type of monadic computations, yielding a value of type ['a]. */

  type monad(+'a);

  /** [from f] create a stream from the given input function. [f] is
      called each time more input is needed, and the stream ends when [f]
      returns [None]. */

  let from: (int => monad(option('a))) => t('a);

  /** [of_list l] create a stream returning all elements of [l]. */

  let of_list: list('a) => t('a);

  /** [of_array a] create a stream returning all elements of [a]. */

  let of_array: array('a) => t('a);

  /** [of_string str] create a stream returning all characters of [str]. */

  let of_string: string => t(char);

  /** Return the list of elements of the given stream. */

  let to_list: t('a) => monad(list('a));

  /** Return the word composed of all characters of the given
      stream. */

  let to_string: t(char) => monad(string);

  /** [peek st] return the first element of the stream, if any,
      without removing it. */

  let peek: t('a) => monad(option('a));

  /** [npeek n st] return at most the first [n] elements of [st],
      without removing them. */

  let npeek: (int, t('a)) => monad(list('a));

  /** [get st] remove and return the first element of the stream, if
      any. */

  let get: t('a) => monad(option('a));

  /** [nget n st] remove and return at most the first [n] elements of
      [st]. */

  let nget: (int, t('a)) => monad(list('a));

  /** [get_while f st] return the longest prefix of [st] where all
      elements satisfy [f]. */

  let get_while: ('a => bool, t('a)) => monad(list('a));

  /** [next st] remove and return the next element of the stream, of
      fail with {!Empty} if the stream is empty. */

  let next: t('a) => monad('a);

  /** [junk st] remove the first element of [st]. */

  let junk: t('a) => monad(unit);

  /** [njunk n st] removes at most the first [n] elements of the
      stream. */

  let njunk: (int, t('a)) => monad(unit);

  /** [junk_while f st] removes all elements at the beginning of the
      streams which satisfy [f]. */

  let junk_while: ('a => bool, t('a)) => monad(unit);

  /** [is_empty st] return wether the given stream is empty */

  let is_empty: t('a) => monad(bool);

  /** {2 Stream transversal} */;

  /** [map f st] maps the value returned by [st] with [f] */

  let map: ('a => 'b, t('a)) => t('b);

  /** [map_list f st] applies [f] on each element of [st] and flattens
      the lists returned */

  let map_list: ('a => list('b), t('a)) => t('b);

  /** [filter f st] keeps only value [x] such that [f x] is [true] */

  let filter: ('a => bool, t('a)) => t('a);

  /** [filter_map f st] filter and map [st] at the same time */

  let filter_map: ('a => option('b), t('a)) => t('b);

  /** [fold f s x] fold_like function for streams. */

  let fold: (('a, 'b) => 'b, t('a), 'b) => monad('b);

  /** [iter f s] iterates over all elements of the stream */

  let iter: ('a => unit, t('a)) => monad(unit);

  /** [find f s] find an element in a stream. */

  let find: ('a => bool, t('a)) => monad(option('a));

  /** [find f s] find and map at the same time. */

  let find_map: ('a => option('b), t('a)) => monad(option('b));

  /** [combine s1 s2] combine two streams. The stream will ends when
      the first stream ends. */

  let combine: (t('a), t('b)) => t(('a, 'b));

  /** [append s1 s2] return a stream which return all elements of
      [s1], then all elements of [s2] */

  let append: (t('a), t('a)) => t('a);

  /** [concat st] return the concatenation of all streams of [st]. */

  let concat: t(t('a)) => t('a);

  /** [flatten st = map_list (fun l -> l) st] */

  let flatten: t(list('a)) => t('a);
};

/** The functor [Lemonade_Stream.Make]. */

module Make:
  (Monad: Lemonade_Type.S) => S with type monad('a) = Monad.t('a);
