open Refmt_api.Migrate_parsetree;
open Ast_404;
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
             )
        )
    }
  | anythingElse => rewrite(body)
  };

let hasWeirdHyphens = ({txt}) => {
  switch (txt) {
  | Lident(name) when String.length(name) > 5
    && name.[0] === 'd'
    && name.[1] === 'a'
    && name.[2] === 't'
    && name.[3] === 'a'
    && name.[4] === '-'
    => true /* aria-foo is fine. Transformed later to ariaFoo */
  | _ => false
  }
};

let processLabelIfAria = (label) => {
  let len = String.length(label);
  if (len > 5
    && label.[0] === 'a'
    && label.[1] === 'r'
    && label.[2] === 'i'
    && label.[3] === 'a'
    && label.[4] === '-') {
    "aria" ++ String.capitalize(String.sub(label, 5, len - 5))
  } else {
    label
  }
};

let refactorMapper = {
  ...default_mapper,
  module_expr: (mapper, item) => {
    switch (item) {
    /* ReactEventRe.UI => ReactEvent.UI */
    | {pmod_desc: Pmod_ident({loc, txt: Ldot(Lident("ReactEventRe"), eventModuleName)})} =>
      {...item, pmod_desc: Pmod_ident({loc, txt: Ldot(Lident("ReactEvent"), eventModuleName)})}
    | _ => default_mapper.module_expr(mapper, item)
    }
  },
  structure_item: (mapper, item) => {
    switch (item) {
    /* open ReactEventRe => open ReactEvent */
    | {pstr_desc: Pstr_open({popen_lid: {loc, txt: Lident("ReactEventRe")}} as o)} =>
      {...item, pstr_desc: Pstr_open({...o, popen_lid: {loc, txt: Lident("ReactEvent")}})}
    | _ => default_mapper.structure_item(mapper, item)
    }
  },
  expr: (mapper, item) =>
    switch (item) {
    /* ReactEventRe.(...) => ReactEvent.(...) */
    | {pexp_desc: Pexp_open(overrideFlag, {loc, txt: Lident("ReactEventRe")}, e)} =>
      {...item, pexp_desc: Pexp_open(overrideFlag, {loc, txt: Lident("ReactEvent")}, mapper.expr(mapper, e))}
    /* ReactEventRe.*._type => ReactEvent.*.type_ */
    | {pexp_desc: Pexp_ident({loc, txt: Ldot(Ldot(Lident("ReactEventRe"), eventModuleName), "_type")})} =>
      {...item, pexp_desc: Pexp_ident({loc, txt: Ldot(Ldot(Lident("ReactEvent"), eventModuleName), "type_")})}
    /* e |> ReactEventRe.Mouse.preventDefault => e->ReactEvent.Mouse.preventDefault */
    | {pexp_desc: Pexp_apply(
        {pexp_desc: Pexp_ident({loc, txt: Lident("|>")})} as f,
        [
          (Nolabel, e),
          (Nolabel, {pexp_desc: Pexp_ident({
            loc: loc2,
            txt: Ldot(
              Ldot(Lident("ReactEventRe"), _eventModuleName),
              _callName
            )
          })} as call)
        ]
      )} =>
      {...item, pexp_desc: Pexp_apply(
        {...f, pexp_desc: Pexp_ident({loc, txt: Lident("|.")})},
        [(Nolabel, mapper.expr(mapper, e)), (Nolabel, mapper.expr(mapper, call))]
      )}
    /* e |> ReactEventRe.Form.target |> ReactDOMRe.domElementToObj => e->ReactEvent.Form.target */
    /* |--- or anything similar ---| */
    | {pexp_desc: Pexp_apply(
        {pexp_desc: Pexp_ident({loc, txt: Lident("|>" | "|.")})} as f,
        [
          (Nolabel, e),
          (Nolabel, {pexp_desc: Pexp_ident({
            txt: Ldot(Lident("ReactDOMRe"), "domElementToObj"),
          })})
        ]
      )}
    /* ReactDOMRe.domElementToObj(e |> ReactEventRe.Form.target) => e->ReactEvent.Form.target */
    /*                            |--- or anything similar ---| */
    | {pexp_desc: Pexp_apply(
        {pexp_desc: Pexp_ident({loc, txt: Ldot(Lident("ReactDOMRe"), "domElementToObj")})} as f,
        [(Nolabel, e)]
      )} =>
        switch (e) {
        | {pexp_desc: Pexp_apply(
            {pexp_desc: Pexp_ident({loc, txt: Lident("|>" | "|.")})} as f,
            [
              (Nolabel, e2),
              (Nolabel, {pexp_desc: Pexp_ident({
                loc: loc2,
                txt: Ldot(Ldot(Lident("ReactEventRe"), eventModuleName), callName),
              })} as call)
            ]
          )} =>
          {...e, pexp_desc: Pexp_apply(
            {...f, pexp_desc: Pexp_ident({loc, txt: Lident("|.")})},
            [
              (Nolabel, mapper.expr(mapper, e2)),
              (Nolabel, {...call, pexp_desc: Pexp_ident({
                loc: loc2,
                txt: Ldot(Ldot(Lident("ReactEvent"), eventModuleName), callName),
              })})
            ]
          )}
        | {pexp_desc: Pexp_apply(
            {pexp_desc: Pexp_ident({loc, txt: Ldot(Ldot(Lident("ReactEventRe"), eventModuleName), callName)})} as f,
            [(Nolabel, e2)]
          )} =>
          {...e, pexp_desc: Pexp_apply(
            {...f, pexp_desc: Pexp_ident({loc, txt: Ldot(Ldot(Lident("ReactEvent"), eventModuleName), callName)})},
            [(Nolabel, mapper.expr(mapper, e2))]
          )}
        | _ => default_mapper.expr(mapper, item)
        };
    /* ReactEventRe => ReactEvent */
    /* this should be at the end of ReactEventRe codemods, since pattern matching order matters */
    | {pexp_desc: Pexp_ident({loc, txt: Lident("ReactEventRe")})} =>
      {...item, pexp_desc: Pexp_ident({loc, txt: Lident("ReactEvent")})}
    | {pexp_desc: Pexp_ident({loc, txt: Ldot(Ldot(Lident("ReactEventRe"), eventModuleName), call)})} =>
      {...item, pexp_desc: Pexp_ident({loc, txt: Ldot(Ldot(Lident("ReactEvent"), eventModuleName), call)})}

    /* ReasonReact.createDomElement("div", {"a": b}, bar) => <div a=(ReactDOMRe.props(~a="b", ())> ...bar </div> */
    | {pexp_desc: Pexp_apply(
        {pexp_desc: Pexp_ident({loc, txt: Ldot(Lident("ReasonReact"), "createDomElement")})},
        [
          (Nolabel, {pexp_desc: Pexp_constant(Pconst_string(domTag, None))}),
          (Nolabel, props),
          (Nolabel, children),
        ]
      )} =>
      let newProps = switch (props) {
      /* {"a": b} => ~a=b */
      | {pexp_desc: Pexp_extension((
          {txt: "bs.obj"},
          PStr([{pstr_desc: Pstr_eval({pexp_desc: Pexp_record(fields, None)}, attrs)}])
        ))}
        when fields |> List.for_all(((name, value)) => !hasWeirdHyphens(name)) =>
        fields |> List.map((({txt}, value)) => {
          let label = Longident.last(txt) |> processLabelIfAria;
          (Labelled(label), value)
        });
      /* Js.Obj.empty() => nothing */
      | {pexp_desc: Pexp_apply(
          {pexp_desc: Pexp_ident({txt: Ldot(Ldot(Lident("Js"), "Obj"), "empty")})},
          [(Nolabel, {pexp_desc: Pexp_construct({txt: Lident("()")}, None)})]
        )} => []
      | e =>
        /* TODO: bail, help user here by wrapping with ReactDOMRe.objToDOMProps */
        [(Nolabel, mapper.expr(mapper, e))]
      };
      let newProps = newProps @ [(Labelled("children"), children), (Nolabel, unitExpr)];
      {
        ...item,
        pexp_attributes: [({loc: Location.none, txt: "JSX"}, PStr([])), ...item.pexp_attributes],
        pexp_desc: Pexp_apply(
          Exp.ident({loc: Location.none, txt: Lident(domTag)}),
          newProps
        )
      }
    | anythingElse => default_mapper.expr(mapper, anythingElse)
    }
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
               lexbuf
             );
           let newAst = refactorMapper.structure(refactorMapper, ast);
           let target = file;
           let oc = open_out_bin(target);
           let formatter = Format.formatter_of_out_channel(oc);
           Refmt_api.Reason_toolchain.RE.print_implementation_with_comments(
             formatter,
             (newAst, comments)
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
    "Done! Please build your project again. It's possible that it fails; if so, it's expected. Check the changes this script made."
  );
};
