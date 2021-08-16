/* Lemonade_Stream -- Monadic streams

   Lemonade (https://github.com/michipili/lemonade)
   This file is part of Lemonade

   Copyright © 2013–2016 Michael Grünewald

   This file must be used under the terms of the CeCILL-B.
   This source file is licensed as described in the file COPYING, which
   you should have received as part of this distribution. The terms
   are also available at
   http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt */

exception Empty;

module type S = {
  type t('a);
  type monad(+'a);
  let from: (int => monad(option('a))) => t('a);
  let of_list: list('a) => t('a);
  let of_array: array('a) => t('a);
  let of_string: string => t(char);
  let to_list: t('a) => monad(list('a));
  let to_string: t(char) => monad(string);
  let peek: t('a) => monad(option('a));
  let npeek: (int, t('a)) => monad(list('a));
  let get: t('a) => monad(option('a));
  let nget: (int, t('a)) => monad(list('a));
  let get_while: ('a => bool, t('a)) => monad(list('a));
  let next: t('a) => monad('a);
  let junk: t('a) => monad(unit);
  let njunk: (int, t('a)) => monad(unit);
  let junk_while: ('a => bool, t('a)) => monad(unit);
  let is_empty: t('a) => monad(bool);
  let map: ('a => 'b, t('a)) => t('b);
  let map_list: ('a => list('b), t('a)) => t('b);
  let filter: ('a => bool, t('a)) => t('a);
  let filter_map: ('a => option('b), t('a)) => t('b);
  let fold: (('a, 'b) => 'b, t('a), 'b) => monad('b);
  let iter: ('a => unit, t('a)) => monad(unit);
  let find: ('a => bool, t('a)) => monad(option('a));
  let find_map: ('a => option('b), t('a)) => monad(option('b));
  let combine: (t('a), t('b)) => t(('a, 'b));
  let append: (t('a), t('a)) => t('a);
  let concat: t(t('a)) => t('a);
  let flatten: t(list('a)) => t('a);
};

module Make = (Monad: Lemonade_Type.S) => {
  open Monad.Infix;

  type monad('a) = Monad.t('a);

  type t('a) = monad(option(cell('a)))
  and cell('a) = {
    mutable count: int,
    mutable data: data('a),
  }
  and data('a) =
    | Sempty
    | Scons('a, data('a))
    | Sgen(gen('a))
  and gen('a) = {
    mutable curr: option(option('a)),
    func: int => monad(option('a)),
  };

  let _count =
    fun
    | None => 0
    | Some({count}) => count;

  let _data =
    fun
    | None => Sempty
    | Some({data}) => data;

  let from = f =>
    Monad.return(Some({count: 0, data: Sgen({curr: None, func: f})}));

  let of_list = lst =>
    Monad.return(
      Some({
        count: 0,
        data:
          List.fold_right(
            (x, l) => [@implicit_arity] Scons(x, l),
            lst,
            Sempty,
          ),
      }),
    );

  let of_array = a => {
    let count = ref(0);
    from(_ =>{
      let c = count^;
      if (c < Array.length(a)) {
        incr(count);
        Monad.return(Some(a[c]));
      } else {
        Monad.return(None);
      };
    });
  };

  let of_string = a => {
    let count = ref(0);
    from(_ =>{
      let c = count^;
      if (c < String.length(a)) {
        incr(count);
        Monad.return(Some(a.[c]));
      } else {
        Monad.return(None);
      };
   } );
  };

  let rec get_data: type v. (int, data(v)) => monad(data(v)) =
    (count, d) =>
      switch (d) {
      /* Returns either Sempty or Scons(a, _) even when d is a generator
         or a buffer. In those cases, the item a is seen as extracted from
         the generator/buffer.
         The count parameter is used for calling `Sgen-functions'.  */
      | Sempty
      | [@implicit_arity] Scons(_, _) => Monad.return(d)
      | Sgen({curr: Some(None), func: _}) => Monad.return(Sempty)
      | Sgen({curr: Some(Some(x)), func: f} as g) =>
        g.curr = None;
        Monad.return([@implicit_arity] Scons(x, d));
      | Sgen({curr: None, func: f} as g) =>
        Monad.bind(
          g.func(count),
          fun
          | None => {
              g.curr = Some(None);
              Monad.return(Sempty);
            }
          | Some(x) => Monad.return([@implicit_arity] Scons(x, d)),
        )
      };

  let rec peek_data: type v. cell(v) => monad(option(v)) =
    s =>
      /* consult the first item of s */
      switch (s.data) {
      | Sempty => Monad.return(None)
      | [@implicit_arity] Scons(x, _) => Monad.return(Some(x))
      | Sgen({curr: Some(x)}) => Monad.return(x)
      | Sgen(g) =>
        Monad.bind(
          g.func(s.count),
          x => {
            g.curr = Some(x);
            Monad.return(x);
          },
        )
      };

  let peek = m =>
    Monad.bind(
      m,
      fun
      | None => Monad.return(None)
      | Some(s) => peek_data(s),
    );

  let rec junk_data: type v. cell(v) => monad(unit) =
    s =>
      switch (s.data) {
      | [@implicit_arity] Scons(_, d) =>
        Monad.return(
          {
            s.count = succ(s.count);
            s.data = d;
          },
        )
      | Sgen({curr: Some(_)} as g) =>
        Monad.return(
          {
            s.count = succ(s.count);
            g.curr = None;
          },
        )
      | _ =>
        Monad.bind(
          peek_data(s),
          fun
          | None => Monad.return()
          | Some(_) => junk_data(s),
        )
      };

  let junk = m =>
    Monad.bind(
      m,
      fun
      | None => Monad.return()
      | Some(data) => junk_data(data),
    );

  let get = m =>
    Monad.bind(
      peek(m),
      fun
      | Some(a) => junk(m) >>= (() => Monad.return(Some(a)))
      | None => Monad.return(None),
    );

  let rec nget_data = (n, s) =>
    if (n <= 0) {
      Monad.return(([], s.data, 0));
    } else {
      Monad.bind(
        peek_data(s),
        fun
        | None => Monad.return(([], s.data, 0))
        | Some(a) =>
          junk_data(s)
          >>= (
            () =>
              nget_data(pred(n), s)
              >>= (
                ((al, d, k)) =>
                  Monad.return((
                    [a, ...al],
                    [@implicit_arity] Scons(a, d),
                    succ(k),
                  ))
              )
          ),
      );
    };

  let nget = (n, m) =>
    Monad.bind(
      m,
      fun
      | None => Monad.return([])
      | Some(d) =>
        nget_data(n, d)
        >>= (
          ((al, _, len)) =>
            if (len < n) {
              raise(Empty);
            } else {
              Monad.return(al);
            }
        ),
    );

  let npeek_data = (n, s) =>
    nget_data(n, s)
    >>= (
      ((al, d, len)) => {
        s.count = s.count - len;
        s.data = d;
        Monad.return(al);
      }
    );

  let npeek = (n, m) =>
    Monad.bind(
      m,
      fun
      | None => Monad.return([])
      | Some(d) => npeek_data(n, d),
    );

  let next = s =>
    peek(s)
    >>= (
      fun
      | Some(a) => junk(s) >>= (() => Monad.return(a))
      | None => raise(Empty)
    );

  let get_while = (p, m) => {
    let rec loop = ax =>
      Monad.bind(
        peek(m),
        fun
        | Some(a) =>
          junk(m)
          >>= (
            () =>
              if (p(a)) {
                loop([a, ...ax]);
              } else {
                Monad.return(List.rev(ax));
              }
          )
        | None => Monad.return(List.rev(ax)),
      );

    loop([]);
  };

  let junk_while = (p, m) => {
    let rec loop = () =>
      Monad.bind(
        peek(m),
        fun
        | Some(a) =>
          if (p(a)) {
            junk(m) >>= loop;
          } else {
            Monad.return();
          }
        | None => Monad.return(),
      );

    loop();
  };

  let is_empty = m =>
    Monad.bind(
      peek(m),
      fun
      | Some(_) => Monad.return(false)
      | None => Monad.return(true),
    );

  let map = (f, m) => {
    let f = _ =>
      peek(m)
      >>= (
        fun
        | Some(a) => junk(m) >>= (() => Monad.return(Some(f(a))))
        | None => Monad.return(None)
      );

    from(f);
  };

  let map_list = (f, m) => {
    let page = ref([]);
    let rec loop = n =>
      switch (page^) {
      | [] =>
        Monad.bind(
          get(m),
          fun
          | Some(a) => {
              page := f(a);
              loop(n);
            }
          | None => Monad.return(None),
        )
      | [hd, ...tl] =>
        page := tl;
        Monad.return(Some(hd));
      };

    from(loop);
  };

  let filter = (p, m) => {
    let not_p = x => !p(x);

    from(_ => junk_while(not_p, m) >>= (() => get(m)));
  };

  let filter_map = (f, m) => {
    let rec next = serial =>
      Monad.bind(
        get(m),
        fun
        | Some(a) =>
          switch (f(a)) {
          | Some(x) => Monad.return(Some(x))
          | None => next(serial)
          }
        | None => Monad.return(None),
      );

    from(next);
  };

  let flatten = m => map_list(lst => lst, m);

  let append = (m1, m2) => {
    let m = ref(m1);
    let rec loop = n =>
      Monad.bind(
        get(m^),
        fun
        | Some(_) as x => Monad.return(x)
        | None =>
          if (m^ === m2) {
            Monad.return(None);
          } else {
            m := m2;
            loop(n);
          },
      );

    from(loop);
  };

  let concat = m_top => {
    let m = ref(from(_ => Monad.return(None)));
    let rec loop = n =>
      Monad.bind(
        get(m^),
        fun
        | Some(_) as x => Monad.return(x)
        | None =>
          Monad.bind(
            get(m_top),
            fun
            | Some(nextm) => {
                m := nextm;
                loop(n);
              }
            | None => Monad.return(None),
          ),
      );

    from(loop);
  };

  let combine = (m1, m2) => {
    let rec loop = _ =>
      Monad.bind(
        get(m1),
        fun
        | Some(a) =>
          Monad.bind(
            get(m2),
            fun
            | Some(b) => Monad.return(Some((a, b)))
            | None => Monad.return(None),
          )
        | None => Monad.return(None),
      );

    from(loop);
  };

  let fold = (f, m, ax0) => {
    let rec loop = ax =>
      Monad.bind(
        get(m),
        fun
        | Some(x) => loop(f(x, ax))
        | None => Monad.return(ax),
      );

    loop(ax0);
  };

  let iter = (f, m) => {
    let rec loop = () =>
      Monad.bind(
        get(m),
        fun
        | Some(a) => {
            f(a);
            loop();
          }
        | None => Monad.return(),
      );

    loop();
  };

  let find = (p, m) => {
    let rec loop = () =>
      Monad.bind(
        get(m),
        fun
        | Some(a) =>
          if (p(a)) {
            Monad.return(Some(a));
          } else {
            loop();
          }
        | None => Monad.return(None),
      );

    loop();
  };

  let find_map = (f, m) => {
    let rec loop = () =>
      Monad.bind(
        get(m),
        fun
        | Some(a) =>
          switch (f(a)) {
          | Some(_) as x => Monad.return(x)
          | None => loop()
          }
        | None => Monad.return(None),
      );

    loop();
  };

  let rec njunk = (n, m) =>
    if (n <= 0) {
      Monad.return();
    } else {
      Monad.bind(junk(m), () => njunk(pred(n), m));
    };

  let to_list = m => {
    let rec loop = ax =>
      Monad.bind(
        get(m),
        fun
        | Some(x) => loop([x, ...ax])
        | None => Monad.return(List.rev(ax)),
      );

    loop([]);
  };

  let to_string = m => {
    let b = Buffer.create(100);
    let rec loop = () =>
      Monad.bind(
        get(m),
        fun
        | Some(c) => {
            Buffer.add_char(b, c);
            loop();
          }
        | None => Monad.return(Buffer.contents(b)),
      );

    loop();
  };
};
