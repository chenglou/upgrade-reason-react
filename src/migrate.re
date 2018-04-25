open Refmt_api.Migrate_parsetree;

open Ast_404;

/* module To_current = Convert(OCaml_404, OCaml_402); */
open Ast_helper;

open Ast_mapper;

open Asttypes;

open Parsetree;

open Longident;

let isUnit = a =>
  switch (a) {
  | {
      pexp_desc: Pexp_construct({txt: Lident("()")}, None),
      pexp_loc,
      pexp_attributes,
    } =>
    true
  | _ => false
  };

let unitExpr = Exp.construct({loc: Location.none, txt: Lident("()")}, None);

let rec changeInnerMostExpr = (body, rewrite) =>
  switch (body.pexp_desc) {
  | Pexp_let(recFlag, binding, letBody) =>
    switch (changeInnerMostExpr(letBody, rewrite)) {
    | expr when isUnit(expr) => unitExpr
    | expr => {...body, pexp_desc: Pexp_let(recFlag, binding, expr)}
    }
  | Pexp_sequence(first, second) =>
    switch (changeInnerMostExpr(second, rewrite)) {
    | expr when isUnit(expr) => first
    | expr => {...body, pexp_desc: Pexp_sequence(first, expr)}
    }
  | Pexp_open(overrideFlag, ident, inner) =>
    switch (changeInnerMostExpr(inner, rewrite)) {
    | expr when isUnit(expr) => unitExpr
    | expr => {...body, pexp_desc: Pexp_open(overrideFlag, ident, expr)}
    }
  | Pexp_ifthenelse(cond, branch1, None) =>
    switch (changeInnerMostExpr(branch1, rewrite)) {
    | expr when isUnit(expr) => cond
    | expr => {...body, pexp_desc: Pexp_ifthenelse(cond, expr, None)}
    }
  | Pexp_ifthenelse(cond, branch1, Some(branch2)) =>
    let b1 =
      switch (changeInnerMostExpr(branch1, rewrite)) {
      | expr when isUnit(expr) => unitExpr
      | expr => expr
      };
    let b2 =
      switch (changeInnerMostExpr(branch2, rewrite)) {
      | expr when isUnit(expr) => None
      | expr => Some(expr)
      };
    Exp.ifthenelse(cond, b1, b2);
  | Pexp_match(exp, cases) => {
      ...body,
      pexp_desc:
        Pexp_match(
          exp,
          cases
          |> List.map(case =>
               switch (changeInnerMostExpr(case.pc_rhs, rewrite)) {
               | expr when isUnit(expr) => {...case, pc_rhs: unitExpr}
               | expr => {...case, pc_rhs: expr}
               }
             ),
        ),
    }
  | anythingElse => rewrite(body)
  };

let removeNoUpdate = expr =>
  switch (expr) {
  | {
      pexp_desc:
        Pexp_construct(
          {
            txt:
              Lident("NoUpdate") | Ldot(Lident("ReasonReact"), "NoUpdate"),
          },
          _,
        ),
    } => unitExpr
  | {
      pexp_desc:
        Pexp_construct(
          {
            txt:
              Lident("Update") | Lident("UpdateWithSideEffects") |
              Ldot(Lident("ReasonReact"), "Update") |
              Ldot(Lident("ReasonReact"), "UpdateWithSideEffects"),
          },
          _,
        ),
    } =>
    Exp.apply(
      Exp.ident({loc: Location.none, txt: Ldot(Lident("self"), "send")}),
      [
        (
          Nolabel,
          Exp.apply(
            Exp.ident({
              loc: Location.none,
              txt: Lident("pleaseTurnMeIntoAnActionConstructorForTheReducer"),
            }),
            [(Nolabel, expr)],
          ),
        ),
      ],
    )
  | expr => expr
  };

let refactorMapper = {
  ...default_mapper,
  expr: (mapper, expression) =>
    switch (expression) {
    /* remove NoUpdate from didMount return */
    /* change Update & UpdateWithSideEffect to something else */
    | {
        pexp_desc:
          Pexp_record(fields, Some({pexp_desc: Pexp_ident(_)}) as spread),
        pexp_loc,
        pexp_attributes,
      }
        when
          List.exists(
            ((field, _value)) =>
              switch (field) {
              | {txt: Lident("didMount")} => true
              | _ => false
              },
            fields,
          ) =>
      let newFields =
        fields
        |> List.map(((field, value)) =>
             switch (field, value) {
             | (
                 {txt: Lident("didMount")},
                 {pexp_desc: Pexp_fun(Nolabel, None, arg, body)} as value,
               ) => (
                 field,
                 {
                   ...value,
                   pexp_desc:
                     Pexp_fun(
                       Nolabel,
                       None,
                       arg,
                       changeInnerMostExpr(body, removeNoUpdate),
                     ),
                 },
               )
             | fv => (field, default_mapper.expr(mapper, value))
             }
           );
      {pexp_loc, pexp_attributes, pexp_desc: Pexp_record(newFields, spread)};
    /* stringToElement -> string */
    /* arrayToElement -> array */
    /* nullToElement -> null */
    | {pexp_desc: Pexp_ident({txt: Ldot(Lident("ReasonReact"), ident)})} as expr =>
      switch (ident) {
      | "stringToElement" =>
        Exp.ident({
          loc: Location.none,
          txt: Ldot(Lident("ReasonReact"), "string"),
        })
      | "arrayToElement" =>
        Exp.ident({
          loc: Location.none,
          txt: Ldot(Lident("ReasonReact"), "array"),
        })
      | "nullElement" =>
        Exp.ident({
          loc: Location.none,
          txt: Ldot(Lident("ReasonReact"), "null"),
        })
      | _ => default_mapper.expr(mapper, expr)
      }
    | anythingElse => default_mapper.expr(mapper, anythingElse)
    },
};

switch (Sys.argv) {
| [||]
| [|_|]
| [|_, "help" | "-help" | "--help"|] =>
  print_endline("Usage: pass a list of .re files you'd like to convert.")
| arguments =>
  let files = Array.sub(arguments, 1, Array.length(arguments) - 1);
  files
  |> Array.iter(file => {
       let isReason = Filename.check_suffix(file, ".re");
       /* || Filename.check_suffix(file, ".rei"); */
       /* let isOCaml =
          Filename.check_suffix(file, ".ml")
          || Filename.check_suffix(file, ".mli"); */
       if (isReason) {
         if (Sys.file_exists(file)) {
           let ic = open_in_bin(file);
           let lexbuf = Lexing.from_channel(ic);
           let (ast, comments) =
             Refmt_api.Reason_toolchain.RE.implementation_with_comments(
               lexbuf,
             );
           let newAst = refactorMapper.structure(refactorMapper, ast);
           let target = file;
           let oc = open_out_bin(target);
           let formatter = Format.formatter_of_out_channel(oc);
           Refmt_api.Reason_toolchain.RE.print_implementation_with_comments(
             formatter,
             (newAst, comments),
           );
           Format.print_flush();
           close_out(oc);
         } else {
           print_endline(file ++ " doesn't exist. Skipped.");
         };
       };
     });
  print_endline("\n===\n");
  print_endline(
    "Done! Please build your project again. It's possible that it fails; if so, it's expected. Check the changes this script made. You might need to re-run this script.",
  );
};
