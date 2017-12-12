open Ast_helper;

open Ast_mapper;

open Asttypes;

open Parsetree;

open Longident;

let rec wrapFunctionReturn = (body, wrap) =>
  switch body.pexp_desc {
  | Pexp_let(a, b, nextExpression) => {
      ...body,
      pexp_desc: Pexp_let(a, b, wrapFunctionReturn(nextExpression, wrap))
    }
  | Pexp_sequence(first, second) => {
      ...body,
      pexp_desc: Pexp_sequence(first, wrapFunctionReturn(second, wrap))
    }
  | Pexp_open(a, b, nextExpression) => {
      ...body,
      pexp_desc: Pexp_open(a, b, wrapFunctionReturn(nextExpression, wrap))
    }
  | anythingElse => wrap(body)
  };

let jsxMapper = {
  ...default_mapper,
  expr: (mapper, expression) =>
    /* self.reduce(foo) */
    /* self.reduce(() => Foo) */
    /* self.reduce((a) => Foo(a)) */
    /* self.ReasonReact.reduce(...) */
    /* reduce(...) */
    switch expression {
    | {
        pexp_desc:
          Pexp_apply(
            {
              pexp_desc:
                Pexp_field(
                  {pexp_desc: Pexp_ident({txt: Lident("self")})},
                  {
                    txt:
                      (
                        Lident("reduce") | Ldot(Lident("ReasonReact"), "reduce") |
                        Lident(
                          "reduce__pleaseInlineTheArgumentAndRunTheScriptAgain"
                        ) |
                        Ldot(
                          Lident("ReasonReact"),
                          "reduce__pleaseInlineTheArgumentAndRunTheScriptAgain"
                        )
                      ) as reduceCall
                  }
                )
            },
            ([_] | [_, _]) as arguments
          )
      } =>
      switch arguments {
      | [("", {pexp_desc: Pexp_fun("", None, argument, body)})] =>
        /* self.reduce(a => Foo(a)) --> a => self.send(Foo(a)) */
        let selfSendCall =
          switch reduceCall {
          | Lident(_) => Ldot(Lident("self"), "send")
          | Ldot(_) => Ldot(Ldot(Lident("self"), "ReasonReact"), "send")
          | anythingElse => anythingElse
          };
        let wrappedBody =
          wrapFunctionReturn(body, (return) =>
            Exp.apply(
              Exp.ident({txt: selfSendCall, loc: Location.none}),
              [("", return)]
            )
          );
        Exp.fun_("", None, argument, wrappedBody);
      | [
          ("", anything),
          ("", {pexp_desc: Pexp_construct({txt: Lident("()")}, None)})
        ] =>
        /* self.reduce(foo, ()) --> self.send(foo) */
        let selfSendCall =
          switch reduceCall {
          | Lident(_) => Ldot(Lident("self"), "send")
          | Ldot(_) => Ldot(Ldot(Lident("self"), "ReasonReact"), "send")
          | anythingElse => anythingElse
          };
        Exp.apply(
          Exp.ident({txt: selfSendCall, loc: Location.none}),
          [("", anything)]
        );
      | notInlinedCallback =>
        /* self.reduce(foo) --> self.reduce__pleaseInlineThisAndRunTheScriptAgain(foo) */
        let selfReduceCall =
          switch reduceCall {
          | Lident(_) =>
            Ldot(
              Lident("self"),
              "reduce__pleaseInlineTheArgumentAndRunTheScriptAgain"
            )
          | Ldot(_) =>
            Ldot(
              Ldot(Lident("self"), "ReasonReact"),
              "reduce__pleaseInlineTheArgumentAndRunTheScriptAgain"
            )
          | anythingElse => anythingElse
          };
        Exp.apply(
          Exp.ident({txt: selfReduceCall, loc: Location.none}),
          arguments
        );
      }
    | {
        pexp_desc:
          Pexp_apply(
            {pexp_desc: Pexp_ident({txt: Lident("reduce")})},
            [("", {pexp_desc: Pexp_fun("", None, argument, body)})]
          )
      } =>
      /* reduce(foo => Bar)) --> foo => send(Bar) */
      let wrappedBody =
        wrapFunctionReturn(body, (return) =>
          Exp.apply(
            Exp.ident({txt: Lident("send"), loc: Location.none}),
            [("", return)]
          )
        );
      Exp.fun_("", None, argument, wrappedBody);
    | {
        pexp_desc:
          Pexp_apply(
            {pexp_desc: Pexp_ident({txt: Lident("reduce")})},
            [
              ("", anything),
              ("", {pexp_desc: Pexp_construct({txt: Lident("()")}, None)})
            ]
          )
      } =>
      /* reduce(foo, ()) --> send(foo) */
      Exp.apply(
        Exp.ident({txt: Lident("send"), loc: Location.none}),
        [("", anything)]
      )
    | {
        pexp_desc:
          Pexp_fun(
            "",
            None,
            {ppat_desc: Ppat_record(fields, flag)} as pattern,
            body
          )
      }
        when
          List.exists(
            (({txt}, _)) =>
              switch txt {
              | Ldot(
                  Lident("ReasonReact"),
                  "state" | "reduce" | "handle" | "retainedProps" | "send"
                ) =>
                true
              | _ => false
              },
            fields
          ) =>
      /* ({ReasonReact.state, reduce}) --> ({ReasonReact.state, send}) */
      let newFields =
        List.map(
          (({txt} as fieldName, pattern)) =>
            switch txt {
            | Ldot(Lident("ReasonReact"), "reduce") => (
                {...fieldName, txt: Ldot(Lident("ReasonReact"), "send")},
                {
                  ...pattern,
                  ppat_desc: Ppat_var({txt: "send", loc: Location.none})
                }
              )
            | Lident("reduce") => (
                {...fieldName, txt: Lident("send")},
                {
                  ...pattern,
                  ppat_desc: Ppat_var({txt: "send", loc: Location.none})
                }
              )
            | _ => (fieldName, pattern)
            },
          fields
        );
      let newPattern = {...pattern, ppat_desc: Ppat_record(newFields, flag)};
      Exp.fun_("", None, newPattern, mapper.expr(mapper, body));
    | {pexp_desc: Pexp_fun("", None, pattern, body)} =>
      /* drill deeper into a function, check the subsequent args */
      Exp.fun_("", None, pattern, mapper.expr(mapper, body))
    | anythingElse => default_mapper.expr(mapper, anythingElse)
    }
};

let apply = (~source, ~target, mapper) => {
  let ic = open_in_bin(source);
  let magic =
    really_input_string(ic, String.length(Config.ast_impl_magic_number));
  let rewrite = (transform) => {
    Location.input_name := input_value(ic);
    let ast = input_value(ic);
    close_in(ic);
    let ast = transform(ast);
    let oc = open_out_bin(target);
    output_string(oc, magic);
    output_value(oc, Location.input_name^);
    output_value(oc, ast);
    close_out(oc);
  }
  and fail = () => {
    close_in(ic);
    failwith("Ast_mapper: OCaml version mismatch or malformed input");
  };
  if (magic == Config.ast_impl_magic_number) {
    rewrite((ast)
      => mapper.structure(mapper, ast));
      /* } else if (magic == Config.ast_intf_magic_number) {
         rewrite(iface: signature => signature); */
  } else {
    fail();
  };
};

/* Arg.parse([], parseFile, "Usage: migrate.exe MyFileToMigrate.re"); */
/*let () = Ast_mapper.run_main((_argv) => jsxMapper());*/
/* if (Sys.file_exists("temp") || Sys.file_exists("temp_processed")) {
     print_endline(
       "temp and/or temp_processed files are presesnt in the current directory. Move or remove them to continue."
     );
   } else { */
Array.sub(Sys.argv, 1, Array.length(Sys.argv) - 1)
|> Array.iter((file) =>
     if (Sys.command("refmt --print binary component.re > temp") == 0) {
       apply(~source="temp", ~target="temp_processed", jsxMapper);
       if (Sys.command(
             "refmt --parse=binary -p re temp_processed > result.re"
           )
           != 0) {
         print_endline("Could not print output file");
       } else {
         ();
       };
     } else {
       print_endline("Could not parse the input file named: " ++ file);
     }
   );
/* |> (() => ignore(Sys.command("rm temp; rm temp_processed"))); */
/* }; */
