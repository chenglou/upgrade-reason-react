open Refmt_api.Migrate_parsetree
open Ast_404
open Ast_helper
open Ast_mapper
open Asttypes
open Parsetree
open Longident
let rec wrapFunctionReturn body wrap =
  match body.pexp_desc with
  | ((Pexp_let (a,b,nextExpression))[@explicit_arity ]) ->
      {
        body with
        pexp_desc =
          ((Pexp_let (a, b, (wrapFunctionReturn nextExpression wrap)))
          [@explicit_arity ])
      }
  | ((Pexp_sequence (first,second))[@explicit_arity ]) ->
      {
        body with
        pexp_desc =
          ((Pexp_sequence (first, (wrapFunctionReturn second wrap)))[@explicit_arity
                                                                    ])
      }
  | ((Pexp_open (a,b,nextExpression))[@explicit_arity ]) ->
      {
        body with
        pexp_desc =
          ((Pexp_open (a, b, (wrapFunctionReturn nextExpression wrap)))
          [@explicit_arity ])
      }
  | anythingElse -> wrap body
let refactorMapper =
  {
    default_mapper with
    expr =
      (fun mapper  ->
         fun expression  ->
           match expression with
           | {
               pexp_desc = ((Pexp_apply
                 ({
                    pexp_desc = ((Pexp_field
                      ({
                         pexp_desc = ((Pexp_ident
                           ({ txt = ((Lident ("self"))[@explicit_arity ]) }))
                           [@explicit_arity ])
                         },{
                             txt =
                               (((Lident ("reduce"))[@explicit_arity ])
                                |((Ldot
                                (((Lident
                                 ("ReasonReact"))[@explicit_arity ]),"reduce"))
                                [@explicit_arity ])|((Lident
                                ("reduce__pleaseInlineTheArgumentAndRunTheScriptAgain"))
                                [@explicit_arity ])|((Ldot
                                (((Lident
                                 ("ReasonReact"))[@explicit_arity ]),"reduce__pleaseInlineTheArgumentAndRunTheScriptAgain"))
                                [@explicit_arity ]) as reduceCall)
                             }))[@explicit_arity ])
                    },(_::[]|_::_::[] as arguments)))[@explicit_arity ])
               } ->
               (match arguments with
                | (Nolabel
                   ,{
                      pexp_desc = ((Pexp_fun
                        (Nolabel ,None ,argument,body))[@explicit_arity ])
                      })::[]
                    ->
                    let selfSendCall =
                      match reduceCall with
                      | Lident _ ->
                          ((Ldot
                              (((Lident ("self"))[@explicit_arity ]), "send"))
                          [@explicit_arity ])
                      | Ldot _ ->
                          ((Ldot
                              (((Ldot
                                   (((Lident ("self"))[@explicit_arity ]),
                                     "ReasonReact"))[@explicit_arity ]),
                                "send"))[@explicit_arity ])
                      | anythingElse -> anythingElse in
                    let wrappedBody =
                      wrapFunctionReturn body
                        (fun return  ->
                           Exp.apply
                             (Exp.ident
                                { txt = selfSendCall; loc = Location.none })
                             [(Nolabel, return)]) in
                    Exp.fun_ Nolabel None argument wrappedBody
                | (Nolabel ,firstArgument)::((Nolabel
                                              ,{
                                                 pexp_desc = ((Pexp_construct
                                                   ({
                                                      txt = ((Lident
                                                        ("()"))[@explicit_arity
                                                                 ])
                                                      },None
                                                    ))[@explicit_arity ])
                                                 })
                                               as second)::[]
                    ->
                    (match firstArgument with
                     | {
                         pexp_desc = ((Pexp_fun
                           (Nolabel ,None ,argument,body))[@explicit_arity ])
                         } ->
                         let selfSendCall =
                           match reduceCall with
                           | Lident _ ->
                               ((Ldot
                                   (((Lident ("self"))[@explicit_arity ]),
                                     "send"))[@explicit_arity ])
                           | Ldot _ ->
                               ((Ldot
                                   (((Ldot
                                        (((Lident ("self"))[@explicit_arity ]),
                                          "ReasonReact"))[@explicit_arity ]),
                                     "send"))[@explicit_arity ])
                           | anythingElse -> anythingElse in
                         Exp.apply
                           (Exp.ident
                              { txt = selfSendCall; loc = Location.none })
                           [(Nolabel, body)]
                     | anythingElse ->
                         let selfSendCall =
                           match reduceCall with
                           | Lident _ ->
                               ((Ldot
                                   (((Lident ("self"))[@explicit_arity ]),
                                     "reduce__pleaseInlineTheArgumentAndRunTheScriptAgain"))
                               [@explicit_arity ])
                           | Ldot _ ->
                               ((Ldot
                                   (((Ldot
                                        (((Lident ("self"))[@explicit_arity ]),
                                          "ReasonReact"))[@explicit_arity ]),
                                     "reduce__pleaseInlineTheArgumentAndRunTheScriptAgain"))
                               [@explicit_arity ])
                           | anythingElse -> anythingElse in
                         Exp.apply
                           (Exp.ident
                              { txt = selfSendCall; loc = Location.none })
                           [(Nolabel, anythingElse); second])
                | notInlinedCallback ->
                    let selfReduceCall =
                      match reduceCall with
                      | Lident _ ->
                          ((Ldot
                              (((Lident ("self"))[@explicit_arity ]),
                                "reduce__pleaseInlineTheArgumentAndRunTheScriptAgain"))
                          [@explicit_arity ])
                      | Ldot _ ->
                          ((Ldot
                              (((Ldot
                                   (((Lident ("self"))[@explicit_arity ]),
                                     "ReasonReact"))[@explicit_arity ]),
                                "reduce__pleaseInlineTheArgumentAndRunTheScriptAgain"))
                          [@explicit_arity ])
                      | anythingElse -> anythingElse in
                    Exp.apply
                      (Exp.ident
                         { txt = selfReduceCall; loc = Location.none })
                      arguments)
           | {
               pexp_desc = ((Pexp_apply
                 ({
                    pexp_desc = ((Pexp_ident
                      ({ txt = ((Lident ("reduce"))[@explicit_arity ]) }))
                      [@explicit_arity ])
                    },(Nolabel
                       ,{
                          pexp_desc = ((Pexp_fun
                            (Nolabel ,None ,argument,body))[@explicit_arity ])
                          })::[]))[@explicit_arity ])
               } ->
               let wrappedBody =
                 wrapFunctionReturn body
                   (fun return  ->
                      Exp.apply
                        (Exp.ident
                           {
                             txt = ((Lident ("send"))[@explicit_arity ]);
                             loc = Location.none
                           }) [(Nolabel, return)]) in
               Exp.fun_ Nolabel None argument wrappedBody
           | {
               pexp_desc = ((Pexp_apply
                 ({
                    pexp_desc = ((Pexp_ident
                      ({
                         txt = ((Lident
                           (("reduce"
                             |"reduce__pleaseInlineTheArgumentAndRunTheScriptAgain")))
                           [@explicit_arity ])
                         }))[@explicit_arity ])
                    },(Nolabel ,firstArgument)::((Nolabel
                                                  ,{
                                                     pexp_desc =
                                                       ((Pexp_construct
                                                       ({
                                                          txt = ((Lident
                                                            ("()"))[@explicit_arity
                                                                    ])
                                                          },None
                                                        ))[@explicit_arity ])
                                                     })
                                                   as second)::[]))[@explicit_arity
                                                                    ])
               } ->
               (match firstArgument with
                | {
                    pexp_desc = ((Pexp_fun
                      (Nolabel ,None ,argument,body))[@explicit_arity ])
                    } ->
                    Exp.apply
                      (Exp.ident
                         {
                           txt = ((Lident ("send"))[@explicit_arity ]);
                           loc = Location.none
                         }) [(Nolabel, body)]
                | anythingElse ->
                    Exp.apply
                      (Exp.ident
                         {
                           txt =
                             ((Lident
                                 ("reduce__pleaseInlineTheArgumentAndRunTheScriptAgain"))
                             [@explicit_arity ]);
                           loc = Location.none
                         }) [(Nolabel, anythingElse); second])
           | {
               pexp_desc = ((Pexp_fun
                 (Nolabel ,None
                  ,({
                      ppat_desc = ((Ppat_record
                        (fields,flag))[@explicit_arity ])
                      } as pattern),body))[@explicit_arity ])
               } when
               List.exists
                 (fun ({ txt },_)  ->
                    match txt with
                    | ((Ldot
                        (((Lident
                         ("ReasonReact"))[@explicit_arity ]),("state"
                                                              |"reduce"
                                                              |"handle"
                                                              |"retainedProps"
                                                              |"send")))
                        [@explicit_arity ]) -> true
                    | _ -> false) fields
               ->
               let newFields =
                 List.map
                   (fun (({ txt } as fieldName),pattern)  ->
                      match txt with
                      | ((Ldot
                          (((Lident
                           ("ReasonReact"))[@explicit_arity ]),"reduce"))
                          [@explicit_arity ]) ->
                          ({
                             fieldName with
                             txt =
                               ((Ldot
                                   (((Lident ("ReasonReact"))[@explicit_arity
                                                               ]), "send"))
                               [@explicit_arity ])
                           },
                            {
                              pattern with
                              ppat_desc =
                                ((Ppat_var
                                    ({ txt = "send"; loc = Location.none }))
                                [@explicit_arity ])
                            })
                      | ((Lident ("reduce"))[@explicit_arity ]) ->
                          ({
                             fieldName with
                             txt = ((Lident ("send"))[@explicit_arity ])
                           },
                            {
                              pattern with
                              ppat_desc =
                                ((Ppat_var
                                    ({ txt = "send"; loc = Location.none }))
                                [@explicit_arity ])
                            })
                      | _ -> (fieldName, pattern)) fields in
               let newPattern =
                 {
                   pattern with
                   ppat_desc =
                     ((Ppat_record (newFields, flag))[@explicit_arity ])
                 } in
               Exp.fun_ Nolabel None newPattern (mapper.expr mapper body)
           | {
               pexp_desc = ((Pexp_fun
                 (Nolabel ,None ,pattern,body))[@explicit_arity ])
               } -> Exp.fun_ Nolabel None pattern (mapper.expr mapper body)
           | anythingElse -> default_mapper.expr mapper anythingElse)
  }
let _ =
  match Sys.argv with
  | [||]|[|_|] -> print_endline "Pass a list of files you'd like to convert"
  | arguments ->
      let files = Array.sub arguments 1 ((Array.length arguments) - 1) in
      files |>
        (Array.iter
           (fun file  ->
              let isReason = Filename.check_suffix file ".re" in
              if isReason
              then
                let ic = open_in_bin file in
                let lexbuf = Lexing.from_channel ic in
                let (ast,comments) =
                  Refmt_api.Reason_toolchain.RE.implementation_with_comments
                    lexbuf in
                let newAst = refactorMapper.structure refactorMapper ast in
                let target = file in
                let oc = open_out_bin target in
                let formatter = Format.formatter_of_out_channel oc in
                (Refmt_api.Reason_toolchain.RE.print_implementation_with_comments
                   formatter (newAst, comments);
                 Format.print_flush ();
                 close_out oc)))