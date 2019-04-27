module T = Ppxlib.Ast_builder.Default

let of_td ~loc (td: Ppxlib.type_declaration) =
  let n = td.ptype_name.txt in
  match td.ptype_kind with
  | Ppxlib.Ptype_record _r ->
    [T.pstr_module ~loc
       { pmb_name = {txt = (Printf.sprintf "Decode_%s" n); loc }
       ; pmb_expr =
           T.pmod_functor ~loc {txt = "D"; loc }
             (Some
                (T.pmty_ident ~loc
                   { txt =
                       Longident.Ldot
                         (Longident.Ldot (Longident.Lident "Decoders", "Decode"), "S")
                   ; loc
                   }))
             (let f_name = T.pvar ~loc (Printf.sprintf "decode_%s" n) in
              let f_type =
                T.ptyp_constr ~loc {txt = (Longident.Ldot (Longident.Lident "D", "decoder")); loc } [
                  T.ptyp_constr ~loc {txt = (Longident.Lident n); loc } []
                ] in
                T.pmod_structure ~loc
                [[%stri let [%p f_name] : [%t f_type] = 3]]
             )
       ; pmb_attributes = []
       ; pmb_loc = loc
       }
    ]
  | _ ->
    failwith (Printf.sprintf "Unsupported type for deriving decoders: %s" n)

let str_type_decl ~loc ~path:_ (_rf, tds) =
  tds
  |> List.map (of_td ~loc)
  |> List.concat
