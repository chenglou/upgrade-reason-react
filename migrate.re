open Refmt_api.Migrate_parsetree;

open Ast_404;

/* module To_current = Convert(OCaml_404, OCaml_402); */
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

let refactorMapper = {
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
          (Nolabel, firstArgument),
          (Nolabel, {pexp_desc: Pexp_construct({txt: Lident("()")}, None)}) as second
        ] =>
        /* self.reduce(_bla => Foo(a), ()) --> self.send(Foo(a)) */
        /* self.reduce(foo, ()) --> self.reduce__pleaseInlineTheArgumentAndRunTheScriptAgain(foo, ()) */
        switch firstArgument {
        | {pexp_desc: Pexp_fun(Nolabel, None, argument, body)} =>
          let selfSendCall =
            switch reduceCall {
            | Lident(_) => Ldot(Lident("self"), "send")
            | Ldot(_) => Ldot(Ldot(Lident("self"), "ReasonReact"), "send")
            | anythingElse => anythingElse
            };
          Exp.apply(
            Exp.ident({txt: selfSendCall, loc: Location.none}),
            [(Nolabel, body)]
          );
        | anythingElse =>
          let selfSendCall =
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
            Exp.ident({txt: selfSendCall, loc: Location.none}),
            [(Nolabel, anythingElse), second]
          );
        }
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
            [(Nolabel, {pexp_desc: Pexp_fun(Nolabel, None, argument, body)})]
          )
      } =>
      /* reduce(foo => Bar)) --> foo => send(Bar) */
      let wrappedBody =
        wrapFunctionReturn(body, (return) =>
          Exp.apply(
            Exp.ident({txt: Lident("send"), loc: Location.none}),
            [(Nolabel, return)]
          )
        );
      Exp.fun_(Nolabel, None, argument, wrappedBody);
    | {
        pexp_desc:
          Pexp_apply(
            {pexp_desc: Pexp_ident({txt: Lident("reduce" | "reduce__pleaseInlineTheArgumentAndRunTheScriptAgain")})},
            [
              (Nolabel, firstArgument),
              (Nolabel, {pexp_desc: Pexp_construct({txt: Lident("()")}, None)}) as second
            ]
          )
      } =>
      /* reduce(_bla => Foo(a), ()) --> send(Foo(a)) */
      /* reduce__pleaseInlineTheArgumentAndRunTheScriptAgain(_bla => Foo(a), ()) --> send(Foo(a)) */
      /* reduce(foo, ()) --> reduce__pleaseInlineTheArgumentAndRunTheScriptAgain(foo, ()) */
      switch firstArgument {
      | {pexp_desc: Pexp_fun(Nolabel, None, argument, body)} =>
        Exp.apply(
          Exp.ident({txt: Lident("send"), loc: Location.none}),
          [(Nolabel, body)]
        )
      | anythingElse =>
        Exp.apply(
          Exp.ident({
            txt: Lident("reduce__pleaseInlineTheArgumentAndRunTheScriptAgain"),
            loc: Location.none
          }),
          [(Nolabel, anythingElse), second]
        )
      }
    | {
        pexp_desc:
          Pexp_fun(
            Nolabel,
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
      Exp.fun_(Nolabel, None, newPattern, mapper.expr(mapper, body));
    | {pexp_desc: Pexp_fun(Nolabel, None, pattern, body)} =>
      /* drill deeper into a function, check the subsequent args */
      Exp.fun_(Nolabel, None, pattern, mapper.expr(mapper, body))
    | anythingElse => default_mapper.expr(mapper, anythingElse)
    }
};

let apply = (~source, ~target, mapper) => {
  /* let ic = open_in_bin(source); */
  let ic = source;
  /* let magic =
     really_input_string(ic, String.length(Config.ast_impl_magic_number)); */
  let oc = target;
  let rewrite = (transform) => {
    Location.input_name := input_value(ic);
    let ast = input_value(ic);
    close_in(ic);
    let ast = transform(ast);
    /* let oc = open_out_bin(target); */
    let oc = target;
    /* output_string(oc, magic); */
    output_value(oc, Location.input_name^);
    output_value(oc, ast);
    /* close_out(oc); */
  }
  and fail = () => {
    close_in(ic);
    failwith("Ast_mapper: OCaml version mismatch or malformed input");
  };
  /* if (magic == Config.ast_impl_magic_number) {
       rewrite((ast)
         => mapper.structure(mapper, ast));
         /* } else if (magic == Config.ast_intf_magic_number) {
            rewrite(iface: signature => signature); */
     } else {
       fail();
     }; */
  let (magic_number, filename, ast, comments, parsedAsML, parsedAsInterface) as foo =
    input_value(ic);
  let ast = refactorMapper.structure(refactorMapper, ast);
  output_value(
    oc,
    (magic_number, filename, ast, comments, parsedAsML, parsedAsInterface)
  );
};

let fileCopy = (input, output) => {
  open Unix;
  let buffer_size = 8192;
  let buffer = Bytes.create(buffer_size);
  let fd_in = openfile(input, [O_RDONLY], 0);
  let fd_out = openfile(output, [O_WRONLY, O_CREAT, O_TRUNC], 438);
  let rec copy_loop = () =>
    switch (read(fd_in, buffer, 0, buffer_size)) {
    | 0 => ()
    | r =>
      ignore(write(fd_out, buffer, 0, r));
      copy_loop();
    };
  copy_loop();
  close(fd_in);
  close(fd_out);
};

/* switch Sys.argv {
   | [||]
   | [|_|] => print_endline("Pass a list of files you'd like to convert")
   | arguments =>
     let files = Array.sub(arguments, 1, Array.length(arguments) - 1);
     files
     |> Array.iter((file) => {
          let isReason = Filename.check_suffix(file, ".re");
          /* || Filename.check_suffix(file, ".rei"); */
          /* let isOCaml =
             Filename.check_suffix(file, ".ml")
             || Filename.check_suffix(file, ".mli"); */
          if (isReason) {
            fileCopy(file, file ++ ".backup");
            let inChannel =
              Unix.open_process_in(
                "refmt --parse re --print binary_reason " ++ file ++ ".backup"
              );
            let out =
              Unix.open_process_out(
                "refmt --parse binary_reason --print re > " ++ file
              );
            apply(~source=inChannel, ~target=out, refactorMapper);
            close_out(out);
          }
        });
   }; */
switch Sys.argv {
| [||]
| [|_|] => print_endline("Pass a list of files you'd like to convert")
| arguments =>
  let files = Array.sub(arguments, 1, Array.length(arguments) - 1);
  files
  |> Array.iter((file) => {
       let isReason = Filename.check_suffix(file, ".re");
       /* || Filename.check_suffix(file, ".rei"); */
       /* let isOCaml =
          Filename.check_suffix(file, ".ml")
          || Filename.check_suffix(file, ".mli"); */
       if (isReason) {
         let ic = open_in_bin(file);
         let lexbuf = Lexing.from_channel(ic);
         let (ast, comments) =
           Refmt_api.Reason_toolchain.RE.implementation_with_comments(lexbuf);
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
       }
     });
};
