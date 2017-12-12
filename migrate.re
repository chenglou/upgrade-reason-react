open Refmt_api.Migrate_parsetree;

open Ast_404;

module To_current = Convert(OCaml_404, OCaml_402);

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
      | [(Nolabel, {pexp_desc: Pexp_fun(Nolabel, None, argument, body)})] =>
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
              [(Nolabel, return)]
            )
          );
        Exp.fun_(Nolabel, None, argument, wrappedBody);
      | [
          (Nolabel, anything),
          (Nolabel, {pexp_desc: Pexp_construct({txt: Lident("()")}, None)})
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
          [(Nolabel, anything)]
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
    | anythingElse => anythingElse
    }
};

let parseFile = (filePath) => {
  let ic = open_in_bin(filePath);
  let lexbuf = Lexing.from_channel(ic);
  let (ast, comments) =
    Refmt_api.Reason_toolchain.RE.implementation_with_comments(lexbuf);
  /*let ast = Parse.implementation(lexbuf);*/
  let newAst = jsxMapper.structure(jsxMapper, ast);
  /* output now */
  let target = "./result.re";
  let oc = open_out_bin(target);
  let formatter = Format.formatter_of_out_channel(oc);
  Refmt_api.Reason_toolchain.RE.print_implementation_with_comments(
    formatter,
    (newAst, comments)
  );
  /*Format.flush_str_formatter()*/
  Format.print_flush();
  /*  let magic = really_input_string(ic, String.length(Config.ast_impl_magic_number));
        output_string(oc, magic);
        output_value(oc, target);
        output_value(oc, newAst);
      */ close_out(
    oc
  );
};

Arg.parse([], parseFile, "Usage: migrate.exe MyFileToMigrate.re");
/*let () = Ast_mapper.run_main((_argv) => jsxMapper());*/
