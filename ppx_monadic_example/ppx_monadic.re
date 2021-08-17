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

let bind_extension =
  Extension.V3.declare(
    "bind",
    Extension.Context.expression,
    Ast_pattern.(single_expr_payload(__)),
    bind_expand,
  );

let orzero_extension =
  Extension.V3.declare(
    "orzero",
    Extension.Context.expression,
    Ast_pattern.(single_expr_payload(__)),
    orzero_expand,
  );

let orzero_rule = Ppxlib.Context_free.Rule.extension(orzero_extension);

let bind_rule = Ppxlib.Context_free.Rule.extension(bind_extension);

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
    "ocaml-monadic",
    ~rules=[bind_rule, orzero_rule],
    ~impl=expr_mapper#structure,
  );
