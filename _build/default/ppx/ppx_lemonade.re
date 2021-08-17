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

// open Printf;
// module Ocaml_ast_mapper = Ast_mapper;
open Ppxlib;

let bind_expand = (~ctxt as _, expr) => {
  let loc = expr.pexp_loc;
  switch (expr.pexp_desc) {
  | [@implicit_arity] Pexp_let(Nonrecursive, value_bindings, body) =>
    /* This is a let%bind expression!  It's of the form
                    let%bind $p1 = $e1 and ... and $pn = $en in $e0
                  and we want it to take the form
                    bind $e1 (fun $p1 -> ... bind $en (fun $pn -> ...) ...)
       */
    let rec bind_wrap = value_bindings' =>
      switch (value_bindings') {
      | [
          {
            pvb_pat: bind_pattern,
            pvb_expr: bind_expr,
            pvb_attributes: [],
            pvb_loc: _bind_loc,
          },
          ...value_bindings'',
        ] =>
        /* Recurse and then wrap the resulting body. */
        let body' = bind_wrap(value_bindings'');
        let cont_function = [%expr (([%p bind_pattern]) => [%e body'])];
        %expr
        bind([%e bind_expr], [%e cont_function]);
      | _ =>
        /* Nothing left to do.  Just return the body. */
        body
      };

    bind_wrap(value_bindings);
  | [@implicit_arity] Pexp_match(expr_match, cases) =>
    let f = Ast_helper.Exp.function_(cases);
    %expr
    bind([%e expr_match], [%e f]);
  | [@implicit_arity] Pexp_ifthenelse(expr_if, expr_then, expr_else) =>
    let expr_else =
      switch (expr_else) {
      | None =>
        %expr
        ()
      | Some(case) => case
      };

    let cases = [
      Ast_helper.Exp.case([%pat? true], expr_then),
      Ast_helper.Exp.case([%pat? false], expr_else),
    ];

    let f = Ast_helper.Exp.function_(cases);
    %expr
    bind([%e expr_if], [%e f]);
  | [@implicit_arity] Pexp_sequence(expr_seq_l, expr_seq_r) =>
    %expr
    bind([%e expr_seq_l], () =>{
      %e
      expr_seq_r
    })
  | _ => expr
  };
};
let orzero_expand = (~ctxt as _, expr) => {
  let loc = expr.pexp_loc;
  switch (expr.pexp_desc) {
  | [@implicit_arity] Pexp_let(Nonrecursive, value_bindings, body) =>
    /* This is a let%orzero expression.  It's of the form
            let%orzero $p1 = $e1 and ... and $pn = $en in $e0
          and we want it to take the form
            match $e1 with
            | $p1 -> (match $e2 with
                      | $p2 -> ...
                               (match $en with
                                | $pn -> $e0
                                | _ -> zero ())
                      | _ -> zero ())
            | _ -> zero ()
       */
    let rec orzero_wrap = value_bindings' =>
      switch (value_bindings') {
      | [
          {
            pvb_pat: orzero_pattern,
            pvb_expr: orzero_expr,
            pvb_attributes: [],
            pvb_loc: _orzero_loc,
          },
          ...value_bindings'',
        ] =>
        /* Recurse and then wrap the resulting body. */
        let body' = orzero_wrap(value_bindings'');
        switch%expr ([%e orzero_expr]) {
        | [%p orzero_pattern] =>
          %e
          body'
        | _ => zero()
        };
      | _ =>
        /* Nothing left to do.  Just return the body. */
        body
      };

    orzero_wrap(value_bindings);
  | _ => expr
  };
};

let if_extension =
  Extension.V3.declare(
    "if",
    Extension.Context.expression,
    Ast_pattern.(single_expr_payload(__)),
    bind_expand,
  );

let for_extension =
  Extension.V3.declare(
    "for",
    Extension.Context.expression,
    Ast_pattern.(single_expr_payload(__)),
    orzero_expand,
  );
let while_extension =
  Extension.V3.declare(
    "while",
    Extension.Context.expression,
    Ast_pattern.(single_expr_payload(__)),
    orzero_expand,
  );


let while_rule = Ppxlib.Context_free.Rule.extension(while_extension);

let for_rule = Ppxlib.Context_free.Rule.extension(for_extension);

let expr_mapper = {
  as _;
  inherit class Ast_traverse.map as super;
  pub! expression = e => {
    let e = super#expression(e);
    switch (e) {
    | [%expr
        {
          %guard
          [%e? guard_expr];
          [%e? body_expr];
        }
      ] =>
      /* This is a sequenced expression with a [%guard ...] extension.  It
            takes the form
              [%guard expr']; expr
            and we want it to take the form
              if expr' then expr else zero ()
         */
      let loc = e.pexp_loc;
      if%expr ([%e guard_expr]) {
        %e
        body_expr;
      } else {
        zero();
      };
    | _ => e
    };
  }
};

let () =
  Driver.register_transformation(
    "lemonade",
    ~rules=[while_rule, for_rule],
    ~impl=expr_mapper#structure,
  );

// open StdLabels;
// open Ast_helper;
// open Asttypes;
// open Parsetree;
// // open Ast_convenience;
// module Ast = Ppxlib.Ast_builder.Default;
// open Lemonade;
// module Maybe = Lemonade_Maybe;

// /* Extend String operations. */
// module String = {
//   include String;

//   let is_prefix = (s, t) =>
//     length(t) >= length(s) && sub(t, ~pos=0, ~len=length(s)) == s;

//   let suffix = (offset, t) => sub(t, ~pos=offset, ~len=length(t) - offset);

//   let split = (delim, t) => {
//     let rec loop = i =>
//       try({
//         let j = index_from(t, i, delim);
//         [sub(t, ~pos=i, ~len=j - i), ...loop(j + 1)];
//       }) {
//       | Not_found => [sub(t, ~pos=i, ~len=length(t) - i)]
//       };

//     loop(0);
//   };
// };

// let extension_node_name = "lemonade";

// let extension_node_dot = extension_node_name ++ ".";

// let extension_node_prefix = "__ppx_" ++ extension_node_name ++ "__";

// let newname = i => sprintf("%s%d", extension_node_prefix, i);

// let with_loc = (f, {txt, loc}) => [@metaloc loc] f(txt);

// let is_catchall = case => {
//   let rec is_catchall_pattern = p =>
//     switch (p.ppat_desc) {
//     | Ppat_any
//     | Ppat_var(_) => true
//     | [@implicit_arity] Ppat_alias(p, _) => is_catchall_pattern(p)
//     | _ => false
//     };

//   case.pc_guard == None && is_catchall_pattern(case.pc_lhs);
// };

// let maybe_add_catchall_case = cases =>
//   if (!List.exists(~f=is_catchall, cases)) {
//     cases @ {
//       let loc = Location.none;
//       [Exp.case([%pat? exn], [%expr raise(exn)])];
//     };
//   } else {
//     cases;
//   };

// let lid_from_extension_id = name => {
//   // open Longident;
//   let names = String.(split('.', name));
//     List.fold_left(
//         ~f=(acc, name) => [@implicit_arity] Longident.Ldot(acc, name),
//         ~init=Longident.Lident(List.hd(names)),
//         List.tl(names),
//       )
//   // List.fold_left(
//   //   (acc, name) => [@implicit_arity] Ldot(acc, name),
//   //   Lident(List.hd(names)),
//   //   List.tl(names),
//   // );
// };

// let nolabel = Nolabel;

// exception Syntax_error(Location.Error.t);

// let make_exception = (~loc, ~sub, str) =>
//   Syntax_error(Location.Error.make(~loc, ~sub, str));

// let raise_errorf = (~loc, fmt) =>
//   Printf.ksprintf(str => make_exception(~loc, ~sub=[], str) |> raise, fmt);

// let unflatten = l =>
//   switch (l) {
//   | [] => None
//   | [hd, ...tl] =>
//     Some(
//       List.fold_left(
//         ~f=(p, s) => [@implicit_arity] Longident.Ldot(p, s),
//         ~init=Longident.Lident(hd),
//         tl,
//       ),
//     )
//   };

// let rec split_at_dots = (s, pos) =>
//   try({
//     let dot = String.index_from(s, pos, '.');
//     [String.sub(s, ~pos, ~len=dot - pos), ...split_at_dots(s, dot + 1)];
//   }) {
//   | Not_found => [String.sub(s, ~pos, ~len=String.length(s) - pos)]
//   };
// let parse_lid = s => {
//   let components = split_at_dots(s, 0);
//   let assert_lid =
//     String.iteri(~f=(i, c) =>
//       switch (i, c) {
//       | (0, 'a' .. 'z' | '_') => ()
//       | (0, _) => assert(false)
//       | (_, 'a' .. 'z' | 'A' .. 'Z' | '_' | '0' .. '9') => ()
//       | _ => assert(false)
//       }
//     );

//   let assert_uid =
//     String.iteri(~f=(i, c) =>
//       switch (i, c) {
//       | (0, 'A' .. 'Z') => ()
//       | (0, _) => assert(false)
//       | (_, 'a' .. 'z' | 'A' .. 'Z' | '_' | '0' .. '9') => ()
//       | _ => assert(false)
//       }
//     );

//   let rec check =
//     fun
//     | [] => assert(false)
//     | ["", ..._] => assert(false)
//     | [s] => assert_lid(s)
//     | [modul, ...rest] => {
//         assert_uid(modul);
//         check(rest);
//       };

//   check(components);
//   switch (unflatten(components)) {
//   | None => assert(false)
//   | Some(v) => v
//   };
// };
// let mkloc = (txt, loc) => {txt, loc};

// let mknoloc = txt => {txt, loc: Location.none};

// let lid = (~loc=default_loc^, s) => mkloc(parse_lid(s), loc);
// let lemonade_operators = (~loc=Location.none) =>
//   fun
//   | Some("") =>
//     raise(
//       Location.Error(
//         raise_errorf(~loc,"Missing lemonade argument for monadic bind"),
//       ),
//     )
//   | Some(name) => (
//       [%expr [%e Ppxlib.Ast_helper.Exp.ident(~loc,lid(name ++ ".bind"))]],
//       [%expr [%e Exp.ident(lid(name ++ ".return"))]],
//     )
//   | None => ([%expr bind], [%expr return]);

// let lemonade_extension = (~loc=?, txt) =>
//   Maybe.map(
//     lemonade_operators(~loc?),
//     if (String.is_prefix(extension_node_dot, txt)) {
//       Some(Some(String.(suffix(length(extension_node_dot), txt))));
//     } else if (String.is_prefix(extension_node_name, txt)) {
//       Some(None);
//     } else {
//       None;
//     },
//   );

// /** [p = x] ≡ [__ppx_lemonade_$i = x] */
// open Ast_builder.Default;
// let lemonade_bindings = lst => {
//   let loop = (i, binding) => {
//     ...binding,
//     pvb_pat: [@metaloc binding.pvb_expr.pexp_loc] (pvar(~loc=i)),
//   };

//   List.mapi(~f=(loop, lst));
// };

// /** [p = x] and e ≡ [bind __ppx_lwt_$i (fun p -> e)] */

// let lemonade_binds = ((bind, return), exploc, lst, exp) => {
//   let rec loop = (i, bindings) =>
//     switch (bindings) {
//     | [] => exp
//     | [binding, ...t] =>
//       let name = [@metaloc binding.pvb_expr.pexp_loc] (Ast.evar(~loc=exploc) @@ newname(i));

//       let f =
//         [@metaloc binding.pvb_loc]
//         [%expr (([%p binding.pvb_pat]) => [%e loop(i + 1, t)])];

//       let new_exp = [@metaloc exploc] [%expr [%e bind]([%e name], [%e f])];

//       {...new_exp, pexp_attributes: binding.pvb_attributes};
//     };

//   loop(0, lst);
// };
// let mapper = {
//   let expr = (_, exp) =>
//     Ppxlib_ast.Selected_ast.of_ocaml(Expression, exp)
//     |> transform#expression
//     |> Ppxlib_ast.Selected_ast.to_ocaml(Expression);

//   {...Ocaml_ast_mapper.default_mapper, expr};
// };
// let lemonade_expression = (mapper, (bind, return) as monad, exp, attributes) => {
//   default_loc := exp.pexp_loc;
//   let pexp_attributes = attributes @ exp.pexp_attributes;
//   switch (exp.pexp_desc) {
//   /*** [let%lemonade $p$ = $e$ in $e'$] ≡ [bind $e$ (fun $p$ -> $e'$)] */
//   | [@implicit_arity] Pexp_let(Nonrecursive, vbl, expression) =>
//     let new_exp =
//       Exp.let_(
//         Nonrecursive,
//         lemonade_bindings(vbl),
//         lemonade_binds(monad, exp.pexp_loc, vbl, expression),
//       );

//     mapper.expr(mapper, {...new_exp, pexp_attributes});

//   /*** [match%lemonade $e$ with $c$] ≡ [bind $e$ (function $c$)]
//        [match%lemonade $e$ with exception $x$ | $c$] ≡ [try_bind (fun () -> $e$) (function $c$) (function $x$)] */
//   | [@implicit_arity] Pexp_match(e, cases) =>
//     let (exns, cases) =
//       List.partition(
//        ~f= fun
//         | {pc_lhs: [%pat? exception [%p? _]]} => true
//         | _ => false,
//         cases,
//       );

//     let exns =
//       List.map(
//       ~f= fun
//         | {pc_lhs: [%pat? exception [%p? pat]]} as case => {
//             ...case,
//             pc_lhs: pat,
//           }
//         | _ => assert(false),
//         exns,
//       );

//     let exns = maybe_add_catchall_case(exns);
//     let new_exp =
//       switch (exns) {
//       | [] =>
//         %expr
//         bind([%e e], [%e Exp.function_(cases)])
//       | _ =>
//         switch%expr ([%e e]) {
//         | exception exn => [%e Exp.function_(exns)](exn)
//         | m => [%e bind](m, [%e Exp.function_(cases)])
//         }
//       };

//     mapper.expr(mapper, {...new_exp, pexp_attributes});

//   /*** [while%lemonade $cond$ do $body$ done] ≡
//           [let rec __ppx_lwt_loop () =
//              if $cond$ then Lwt.bind $body$ __ppx_lwt_loop
//              else Lwt.return ()
//            in __ppx_lwt_loop]
//        */
//   | [@implicit_arity] Pexp_while(cond, body) =>
//     let new_exp = {
//       let%expr rec __ppx_lemonade_loop = () =>
//         if ([%e cond]) {
//           [%e bind]([%e body], __ppx_lemonade_loop);
//         } else {
//           [%e return]();
//         };

//       __ppx_lemonade_loop();
//     };

//     mapper.expr(mapper, {...new_exp, pexp_attributes});

//   /*** [for%lemonade $p$ = $start$ (to|downto) $end$ do $body$ done] ≡
//        [let __ppx_lwt_bound = $end$ in
//        let rec __ppx_lwt_loop $p$ =
//          if $p$ COMP __ppx_lwt_bound then Lwt.return ()
//          else Lwt.bind $body$ (fun () -> __ppx_lwt_loop ($p$ OP 1))
//        in __ppx_lwt_loop $start$] */
//   | [@implicit_arity]
//     Pexp_for({ppat_desc: Ppat_var(p_var)} as p, start, bound, dir, body) =>
//     let (comp, binop) =
//       switch (dir) {
//       | Upto => (evar(">"), evar("+"))
//       | Downto => (evar("<"), evar("-"))
//       };

//     let q = with_loc(evar, p_var);
//     let exp_bound = [@metaloc bound.pexp_loc] [%expr __ppx_lemonade_bound];
//     let pat_bound = [@metaloc bound.pexp_loc] [%pat? __ppx_lemonade_bound];
//     let new_exp = {
//       let%expr [%p pat_bound]: int = [%e bound];
//       let rec __ppx_lemonade_loop = ([%p p]) =>
//         if ([%e comp]([%e q], [%e exp_bound])) {
//           [%e return]();
//         } else {
//           [%e bind]([%e body], () =>
//             __ppx_lemonade_loop([%e binop]([%e q], 1))
//           );
//         };

//       __ppx_lemonade_loop([%e start]);
//     };

//     mapper.expr(mapper, {...new_exp, pexp_attributes});

//   /*** [try%lemonade $e$ with $c$] ≡
//            [catch (fun () -> $e$) (function $c$)]
//        */
//   | [@implicit_arity] Pexp_try(expr, cases) =>
//     let cases = maybe_add_catchall_case(cases);
//     let new_exp =
//       try%expr([%e expr]()) {
//       | exn => [%e Exp.function_(cases)](exn)
//       };

//     mapper.expr(mapper, {...new_exp, pexp_attributes});

//   /*** [if%lemonade $c$ then $e1$ else $e2$] ≡
//            [match%lemonade $c$ with true -> $e1$ | false -> $e2$]
//            [if%lemonade $c$ then $e1$] ≡
//            [match%lemonade $c$ with true -> $e1$ | false -> Lwt.return_unit]
//        */
//   | [@implicit_arity] Pexp_ifthenelse(cond, e1, e2) =>
//     let e2 =
//       switch (e2) {
//       | Some(e) => e
//       | None =>
//         %expr
//         [%e return]()
//       };

//     let cases = [Exp.case([%pat? true], e1), Exp.case([%pat? false], e2)];

//     let new_exp = [%expr [%e bind]([%e cond], [%e Exp.function_(cases)])];
//     mapper.expr(mapper, {...new_exp, pexp_attributes});

//   | _ => mapper.expr(mapper, exp)
//   };
// };

// let lemonade_mapper = argv => {
//   open Ocaml_ast_mapper;
//   let super = default_mapper;
//   let expr = (this, e) =>
//     switch (e) {
//     | {
//         pexp_desc:
//           [@implicit_arity]
//           Pexp_extension(
//             {txt: id, loc},
//             PStr([{pstr_desc: [@implicit_arity] Pstr_eval(exp, attr)}]),
//           ),
//       } =>
//       switch (lemonade_extension(~loc, id)) {
//       | Some(monad) => lemonade_expression(this, monad, exp, attr)
//       | None => super.expr(this, e)
//       }
//     | _ => super.expr(this, e)
//     };

//   {...default_mapper, expr};
// };

// let () = Ocaml_ast_mapper.run_main(lemonade_mapper);
