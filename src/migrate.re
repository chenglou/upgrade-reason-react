open Belt;

open Refmt_api.Migrate_parsetree;
open Ast_404;
open Ast_helper;
open Ast_mapper;
open Asttypes;
open Parsetree;
open Longident;

let unitExpr = Exp.construct({loc: Location.none, txt: Lident("()")}, None);

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

let camelCaseAriaIfExists = (label) => {
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

let prospectiveComponents: ref(list(string)) = ref([]);
let prospectiveComponentMakePairs = ref([]);
let outerStructure = ref(true);

let findFirstComponentLikeReturnValue = (expr) => {
  /* [arg_label] */
  let propsList = ref([]);
  let rec findFirstComponentLikeReturnValueRec = (expr) => {
    switch (expr) {
    | {
      pexp_desc: Pexp_fun(argLabel, _, _, innerExpression)
    } =>
      switch (argLabel) {
      | Labelled(foo) | Optional(foo) => propsList := [argLabel, ...propsList^]
      | Nolabel => ()
      };
      findFirstComponentLikeReturnValueRec(innerExpression)
    | {
      pexp_desc: Pexp_let(_, _, innerExpression)
    } => findFirstComponentLikeReturnValueRec(innerExpression)
    | {
      pexp_desc: Pexp_record(_, Some({
        pexp_desc: Pexp_ident({txt: Lident(txt)})
      }))
    } => List.getBy(prospectiveComponents^, (el) => el == txt)
    | {
      pexp_desc: Pexp_apply({
      pexp_desc: Pexp_ident({txt: Lident("wrapJsForReason") | Ldot(_, "wrapJsForReason")})
    }, _)
    } => Some("")
    | _ => None
    }
  };
  let maybeReturnValue = findFirstComponentLikeReturnValueRec(expr);
  (maybeReturnValue, propsList^);
};

let lidentLoc = lident => {loc: Location.none, txt: lident};

let safeTypeFromValue = valueStr => switch (String.sub(valueStr, 0, 1)) {
| "_" => "T" ++ valueStr
| _ => valueStr
};

let rec recursivelyMakeNamedArgsForExternal = (list, args) =>
  switch (list) {
  | [label, ...tl] =>
    recursivelyMakeNamedArgsForExternal(
      tl,
      Typ.arrow(
        label,
        Typ.var(safeTypeFromValue(
          switch (label) {
          | Labelled(str)
          | Optional(str) => str
          | _ => raise(Invalid_argument("This should never happen."))
          },
        )),
        args,
      ),
    )
  | [] => args
  };

let refactorMapper = {
  ...default_mapper,
  value_binding: (mapper, item) =>  {
    switch (item) {
    | {
      pvb_pat: {
        ppat_desc: Ppat_var({txt})
      },
      pvb_expr: {
        pexp_desc: Pexp_apply({
          pexp_desc: Pexp_ident({txt:
            Lident("basicComponent") | Lident("statelessComponent") | Lident("statelessComponentWithRetainedProps") | Lident("reducerComponent") | Lident("reducerComponentWithRetainedProps") | 
            Ldot(_, "basicComponent") | Ldot(_, "statelessComponent") | Ldot(_, "statelessComponentWithRetainedProps") | Ldot(_, "reducerComponent") | Ldot(_, "reducerComponentWithRetainedProps")
          })
        }, _)
      }} => prospectiveComponents := [txt, ...prospectiveComponents^]
    | {
        pvb_pat: {
          ppat_desc: Ppat_var({txt})
        },
        pvb_expr: {
        pexp_desc: Pexp_fun(_, _, _, _)
      } as expression} => 
      let (makeForComponent, args) = findFirstComponentLikeReturnValue(expression);
      switch (makeForComponent) {
      | Some(component) =>
        let pairs = List.head(prospectiveComponentMakePairs^);
        switch (pairs) {
        | Some(pairs) =>
          pairs := [(component, txt, args), ...pairs^];
        | None => ()
        };
      | None => ()
      }
    | _ => ();
    };
    default_mapper.value_binding(mapper, item)
  },
  structure: (mapper, item) => {
    let isOuterStructure = outerStructure^;
    outerStructure := false;
    let pairs = ref([]);
    prospectiveComponentMakePairs := [pairs, ...prospectiveComponentMakePairs^];
    let mapped = default_mapper.structure(mapper, item);
    let newStructureItems = List.reduce(List.reverse(pairs^), [], (acc, (component, make, args)) => {
      List.concat(acc, [
      Str.value(Nonrecursive, [
        Vb.mk(
          ~attrs=[(
            lidentLoc("ocaml.doc"),
            PStr([Str.eval(Exp.constant(Const.string(
              "\n * This is a wrapper created to let this component be used from the new React api.\n * Please convert this component to a [@react.component] function and then remove this wrapping code.\n "
            )))])
          )],
          Pat.var(lidentLoc(make)),
          Exp.apply(
            Exp.ident(lidentLoc(Ldot(Lident("ReasonReactCompat"), "wrapReasonReactForReact"))),
            [
              (Labelled("component"), switch (component) {
              | "" => Exp.apply(
                  Exp.ident(lidentLoc(Ldot(Lident("ReasonReact"), "statelessComponent"))),
                  [(Nolabel, Exp.constant(Const.string("TemporaryRefactorComponent")))]
                )
                | component => Exp.ident(lidentLoc(Lident(component)))
              }),
              (Nolabel, Exp.fun_(Nolabel, None, Pat.var(lidentLoc("reactProps")), Exp.apply(
                Exp.ident(lidentLoc(Lident(make))),
                List.concat(
                  List.map(args, (arg) => switch arg {
                  | Labelled(name) | Optional(name) => (arg, Exp.apply(
                    Exp.ident(lidentLoc(Lident("##"))),
                    [
                      (Nolabel, Exp.ident(lidentLoc(Lident("reactProps")))),
                      (Nolabel, Exp.ident(lidentLoc(Lident(name))))
                    ]))
                  | Nolabel => raise(Invalid_argument("This should not ever happen."))
                  }),
                  [
                    (Nolabel, Exp.apply(
                    Exp.ident(lidentLoc(Lident("##"))),
                    [
                      (Nolabel, Exp.ident(lidentLoc(Lident("reactProps")))),
                      (Nolabel, Exp.ident(lidentLoc(Lident("children"))))
                    ]))
                  ]
                )
              )))
            ]
          )
        )
      ]),
      Str.primitive(
        Val.mk(
          ~attrs=[(lidentLoc("bs.obj"), PStr([]))],
          ~prim=[""],
          lidentLoc(make ++ "Props"),
          recursivelyMakeNamedArgsForExternal(
            args,
            Typ.arrow(Nolabel, Typ.constr(lidentLoc(Lident("unit")), []), Typ.constr(
              lidentLoc(Ldot(Lident("Js"), "t")),
              [Typ.object_(List.map(args, (arg) => switch (arg) {
              | Labelled(name) => (name, [], Typ.var(safeTypeFromValue(name)))
              | Optional(name) => (name, [], Typ.constr(lidentLoc(Lident("option")), [Typ.var(safeTypeFromValue(name))]))
              | Nolabel => raise(Invalid_argument("This should never happen"))
              }), Closed)]
            ))
          )
        )
      )
      ])
    });
    let [_, ...pairs] = prospectiveComponentMakePairs^;
    prospectiveComponentMakePairs := pairs;
    let structureItems = List.concat(mapped, newStructureItems);
    if (isOuterStructure) {
      List.add(structureItems, Str.attribute((
        lidentLoc("bs.config"),
        PStr([Str.eval(Exp.record(
          [(lidentLoc(Lident("jsx")), Exp.constant(Const.int(3)))],
          None
        ))])
      )));
    } else {
      structureItems
    }
  },
  expr: (mapper, item) =>
    switch (item) {
    /* ReasonReact.string|array|null => React.string|array|null */
    | {pexp_desc: Pexp_ident({loc, txt: Ldot(Lident("ReasonReact"), ("string" | "array" | "null") as name)})} =>
      {...item, pexp_desc: Pexp_ident({loc, txt: Ldot(Lident("React"), name)})}
    | anythingElse => default_mapper.expr(mapper, anythingElse)
    },
  value_description: (mapper, valueDescription) => {
    let isComponent = ref(false);
    let rec transformType = (coreType) => switch (coreType) {
    | {ptyp_desc: Ptyp_arrow((Labelled(_) | Optional(_)) as label, paramType, innerType)} =>
      {...coreType, ptyp_desc: Ptyp_arrow(label, paramType, transformType(innerType))}
    | {ptyp_desc: Ptyp_arrow(Nolabel, paramType, innerType)} =>
      {...coreType, ptyp_desc: Ptyp_arrow(Labelled("children"), paramType, transformType(innerType))}
    | {ptyp_desc: Ptyp_constr({txt: Ldot(Lident("ReasonReact"), "component") | Lident("component")} as loc, typeArguments)} =>
      isComponent := true;
      {...coreType, ptyp_desc: Ptyp_constr({...loc, txt: Ldot(Lident("React"), "element")}, [])}
    | otherType => otherType
    };
    let newType = transformType(valueDescription.pval_type);
    let transformed = if (isComponent^) {
      {
        ...valueDescription,
        pval_attributes: [
          ({txt: "react.component", loc: valueDescription.pval_loc}, PStr([])),
          ...valueDescription.pval_attributes
        ],
        pval_type: newType,
      }
    } else {
      valueDescription
    };
    default_mapper.value_description(mapper, transformed);
  }
};

switch (Sys.argv) {
| [||]
| [|_|]
| [|_, "help" | "-help" | "--help"|] =>
  print_endline("Usage: pass a list of .re files you'd like to convert.")
| arguments =>
  let validFiles =
    Array.slice(arguments, ~offset=1, ~len=Array.length(arguments) - 1)
    |. Array.keep(file => {
      let isReason = Filename.check_suffix(file, ".re") || Filename.check_suffix(file, ".rei");
      if (isReason) {
        if (Sys.file_exists(file)) {
          true
        } else {
          print_endline(file ++ " doesn't exist. Skipped.");
          false
        }
      } else {
        false
      }
    });
  switch (validFiles) {
  | [||] => print_endline("You didn't pass any Reason file to convert.");
  | files =>
    Array.forEach(files, file => {
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
      let _cleanup = {
        prospectiveComponentMakePairs := [];
        prospectiveComponents := [];
        outerStructure := true;
      };
      Format.print_flush();
      close_out(oc);
    });
    print_endline(
      "\nDone! Please build your project again. It's possible that it fails; if so, it's expected. Check the changes this script made."
    );
  }
};
