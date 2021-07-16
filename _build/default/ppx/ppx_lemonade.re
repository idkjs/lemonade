/* Lemonade, the sparkling monad library

   Copyright © 2016 Michael Grünewald
   Copyright © 2014 Gabriel Radanne, Peter Zotov.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License as
   published by the Free Software Foundation, with linking exceptions;
   either version 2.1 of the License, or (at your option) any later
   version. See COPYING file for details.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
   02111-1307, USA. */

open Printf;
open Ast_mapper;
open Ast_helper;
open Asttypes;
open Parsetree;
open Longident;
open Ast_convenience;
open Lemonade;
module Maybe = Lemonade_Maybe;

/* Extend String operations. */
module String = {
  include String;

  let is_prefix = (s, t) =>
    length(t) >= length(s) && sub(t, 0, length(s)) == s;

  let suffix = (offset, t) => sub(t, offset, length(t) - offset);

  let split = (delim, t) => {
    let rec loop = i =>
      try({
        let j = index_from(t, i, delim);
        [sub(t, i, j - i), ...loop(j + 1)];
      }) {
      | Not_found => [sub(t, i, length(t) - i)]
      };

    loop(0);
  };
};

let extension_node_name = "lemonade";

let extension_node_dot = extension_node_name ++ ".";

let extension_node_prefix = "__ppx_" ++ extension_node_name ++ "__";

let newname = i => sprintf("%s%d", extension_node_prefix, i);

let with_loc = (f, {txt, loc}) => [@metaloc loc] f(txt);

let is_catchall = case => {
  let rec is_catchall_pattern = p =>
    switch (p.ppat_desc) {
    | Ppat_any
    | Ppat_var(_) => true
    | [@implicit_arity] Ppat_alias(p, _) => is_catchall_pattern(p)
    | _ => false
    };

  case.pc_guard == None && is_catchall_pattern(case.pc_lhs);
};

let maybe_add_catchall_case = cases =>
  if (!List.exists(is_catchall, cases)) {
    cases @ [Exp.case([%pat? exn], [%expr raise(exn)])];
  } else {
    cases;
  };

let lid_from_extension_id = name => {
  open Longident;
  let names = String.(split('.', name));
  List.fold_left(
    (acc, name) => [@implicit_arity] Ldot(acc, name),
    Lident(List.hd(names)),
    List.tl(names),
  );
};

let lemonade_operators = (~loc=Location.none) =>
  fun
  | Some("") =>
    raise(
      Location.Error(
        Location.errorf("Missing lemonade argument for monadic bind"),
      ),
    )
  | Some(name) => (
      [%expr [%e Exp.ident(lid(name ++ ".bind"))]],
      [%expr [%e Exp.ident(lid(name ++ ".return"))]],
    )
  | None => ([%expr bind], [%expr return]);

let lemonade_extension = (~loc=?, txt) =>
  Maybe.map(
    lemonade_operators(~loc?),
    if (String.is_prefix(extension_node_dot, txt)) {
      Some(Some(String.(suffix(length(extension_node_dot), txt))));
    } else if (String.is_prefix(extension_node_name, txt)) {
      Some(None);
    } else {
      None;
    },
  );

/** [p = x] ≡ [__ppx_lemonade_$i = x] */

let lemonade_bindings = lst => {
  let loop = (i, binding) => {
    ...binding,
    pvb_pat: [@metaloc binding.pvb_expr.pexp_loc] (pvar @@ newname(i)),
  };

  List.mapi(loop, lst);
};

/** [p = x] and e ≡ [bind __ppx_lwt_$i (fun p -> e)] */

let lemonade_binds = ((bind, return), exploc, lst, exp) => {
  let rec loop = (i, bindings) =>
    switch (bindings) {
    | [] => exp
    | [binding, ...t] =>
      let name = [@metaloc binding.pvb_expr.pexp_loc] (evar @@ newname(i));

      let f =
        [@metaloc binding.pvb_loc]
        [%expr (([%p binding.pvb_pat]) => [%e loop(i + 1, t)])];

      let new_exp = [@metaloc exploc] [%expr [%e bind]([%e name], [%e f])];

      {...new_exp, pexp_attributes: binding.pvb_attributes};
    };

  loop(0, lst);
};

let lemonade_expression = (mapper, (bind, return) as monad, exp, attributes) => {
  default_loc := exp.pexp_loc;
  let pexp_attributes = attributes @ exp.pexp_attributes;
  switch (exp.pexp_desc) {
  /*** [let%lemonade $p$ = $e$ in $e'$] ≡ [bind $e$ (fun $p$ -> $e'$)] */
  | [@implicit_arity] Pexp_let(Nonrecursive, vbl, expression) =>
    let new_exp =
      Exp.let_(
        Nonrecursive,
        lemonade_bindings(vbl),
        lemonade_binds(monad, exp.pexp_loc, vbl, expression),
      );

    mapper.expr(mapper, {...new_exp, pexp_attributes});

  /*** [match%lemonade $e$ with $c$] ≡ [bind $e$ (function $c$)]
       [match%lemonade $e$ with exception $x$ | $c$] ≡ [try_bind (fun () -> $e$) (function $c$) (function $x$)] */
  | [@implicit_arity] Pexp_match(e, cases) =>
    let (exns, cases) =
      List.partition(
        fun
        | {pc_lhs: [%pat? exception [%p? _]]} => true
        | _ => false,
        cases,
      );

    let exns =
      List.map(
        fun
        | {pc_lhs: [%pat? exception [%p? pat]]} as case => {
            ...case,
            pc_lhs: pat,
          }
        | _ => assert(false),
        exns,
      );

    let exns = maybe_add_catchall_case(exns);
    let new_exp =
      switch (exns) {
      | [] =>
        %expr
        bind([%e e], [%e Exp.function_(cases)])
      | _ =>
        switch%expr ([%e e]) {
        | exception exn => [%e Exp.function_(exns)](exn)
        | m => [%e bind](m, [%e Exp.function_(cases)])
        }
      };

    mapper.expr(mapper, {...new_exp, pexp_attributes});

  /*** [while%lemonade $cond$ do $body$ done] ≡
          [let rec __ppx_lwt_loop () =
             if $cond$ then Lwt.bind $body$ __ppx_lwt_loop
             else Lwt.return ()
           in __ppx_lwt_loop]
       */
  | [@implicit_arity] Pexp_while(cond, body) =>
    let new_exp = {
      let%expr rec __ppx_lemonade_loop = () =>
        if ([%e cond]) {
          [%e bind]([%e body], __ppx_lemonade_loop);
        } else {
          [%e return]();
        };

      __ppx_lemonade_loop();
    };

    mapper.expr(mapper, {...new_exp, pexp_attributes});

  /*** [for%lemonade $p$ = $start$ (to|downto) $end$ do $body$ done] ≡
       [let __ppx_lwt_bound = $end$ in
       let rec __ppx_lwt_loop $p$ =
         if $p$ COMP __ppx_lwt_bound then Lwt.return ()
         else Lwt.bind $body$ (fun () -> __ppx_lwt_loop ($p$ OP 1))
       in __ppx_lwt_loop $start$] */
  | [@implicit_arity]
    Pexp_for({ppat_desc: Ppat_var(p_var)} as p, start, bound, dir, body) =>
    let (comp, binop) =
      switch (dir) {
      | Upto => (evar(">"), evar("+"))
      | Downto => (evar("<"), evar("-"))
      };

    let q = with_loc(evar, p_var);
    let exp_bound = [@metaloc bound.pexp_loc] [%expr __ppx_lemonade_bound];
    let pat_bound = [@metaloc bound.pexp_loc] [%pat? __ppx_lemonade_bound];
    let new_exp = {
      let%expr [%p pat_bound]: int = [%e bound];
      let rec __ppx_lemonade_loop = ([%p p]) =>
        if ([%e comp]([%e q], [%e exp_bound])) {
          [%e return]();
        } else {
          [%e bind]([%e body], () =>
            __ppx_lemonade_loop([%e binop]([%e q], 1))
          );
        };

      __ppx_lemonade_loop([%e start]);
    };

    mapper.expr(mapper, {...new_exp, pexp_attributes});

  /*** [try%lemonade $e$ with $c$] ≡
           [catch (fun () -> $e$) (function $c$)]
       */
  | [@implicit_arity] Pexp_try(expr, cases) =>
    let cases = maybe_add_catchall_case(cases);
    let new_exp =
      try%expr([%e expr]()) {
      | exn => [%e Exp.function_(cases)](exn)
      };

    mapper.expr(mapper, {...new_exp, pexp_attributes});

  /*** [if%lemonade $c$ then $e1$ else $e2$] ≡
           [match%lemonade $c$ with true -> $e1$ | false -> $e2$]
           [if%lemonade $c$ then $e1$] ≡
           [match%lemonade $c$ with true -> $e1$ | false -> Lwt.return_unit]
       */
  | [@implicit_arity] Pexp_ifthenelse(cond, e1, e2) =>
    let e2 =
      switch (e2) {
      | Some(e) => e
      | None =>
        %expr
        [%e return]()
      };

    let cases = [Exp.case([%pat? true], e1), Exp.case([%pat? false], e2)];

    let new_exp = [%expr [%e bind]([%e cond], [%e Exp.function_(cases)])];
    mapper.expr(mapper, {...new_exp, pexp_attributes});

  | _ => mapper.expr(mapper, exp)
  };
};

let lemonade_mapper = argv => {
  open Ast_mapper;
  let super = default_mapper;
  let expr = (this, e) =>
    switch (e) {
    | {
        pexp_desc:
          [@implicit_arity]
          Pexp_extension(
            {txt: id, loc},
            PStr([{pstr_desc: [@implicit_arity] Pstr_eval(exp, attr)}]),
          ),
      } =>
      switch (lemonade_extension(~loc, id)) {
      | Some(monad) => lemonade_expression(this, monad, exp, attr)
      | None => super.expr(this, e)
      }
    | _ => super.expr(this, e)
    };

  {...default_mapper, expr};
};

let () = Ast_mapper.run_main(lemonade_mapper);
