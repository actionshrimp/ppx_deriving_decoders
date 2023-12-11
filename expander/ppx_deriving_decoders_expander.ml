open Ppxlib

module T = Ppxlib.Ast_builder.Default

(* let rec decoder_expr_of_rec_label_type rlt = *)

let expr_of_rec_labels ~loc (rls : label_declaration list) =
  let final_record = T.pexp_record ~loc (rls |> List.map (fun rl ->
      let loc = rl.pld_name.loc in
      let ident = { txt = Longident.Lident rl.pld_name.txt; loc } in
      (ident , T.pexp_ident ~loc ident)
    )) None in
  let suc = [%expr succeed [%e final_record]] in
  rls
  |> List.rev
  |> List.fold_left (fun acc_expr rl ->
      let loc = rl.pld_name.loc in
      let rl_pname = T.pvar ~loc rl.pld_name.txt in
      let rl_ename_str = T.pexp_constant ~loc (Pconst_string (rl.pld_name.txt, loc, None)) in
      let rl_type = rl.pld_type in
      let rl_etype_decoder = match rl.pld_type.ptyp_desc with
        | Ptyp_constr ({ txt = Longident.Lident "int"; _ }, []) -> [%expr int]
        | Ptyp_constr ({ txt = Longident.Lident "string"; _ }, []) -> [%expr string]
        | _ -> T.evar ~loc "_"
      in
      [%expr field [%e rl_ename_str] [%e rl_etype_decoder] >>=
        fun ([%p rl_pname] : [%t rl_type]) ->
        [%e acc_expr]]
    ) suc


let of_td ~loc (td: Ppxlib.type_declaration) =
  let n = td.ptype_name.txt in
  let decode_mod_sig =
    (T.pmty_ident ~loc
       { txt = Longident.Ldot (Longident.Ldot (Longident.Lident "Decoders", "Decode"), "S")
       ; loc })
  in
  match td.ptype_kind with
  | Ppxlib.Ptype_record rls ->
    [
      T.pstr_module ~loc
        { pmb_name = {txt = Some (Printf.sprintf "Decode_%s" n); loc }
        ; pmb_expr =
            T.pmod_functor ~loc (Named ({txt = Some "D"; loc } , decode_mod_sig))
              (let f_name = T.pvar ~loc (Printf.sprintf "decode_%s" n) in
               let f_type =
                 T.ptyp_constr ~loc {txt = (Longident.Lident "decoder"); loc }
                   [ T.ptyp_constr ~loc {txt = (Longident.Lident n); loc } []
                   ] in
               T.pmod_structure ~loc
                 [ [%stri open D]
                 ; [%stri
                   let [%p f_name] : [%t f_type] =
                     [%e expr_of_rec_labels ~loc rls]]
                 ]
              )
        ; pmb_attributes = []
        ; pmb_loc = loc
        }
    ]
  | _ ->
    []

let str_type_decl ~loc ~path:_ (_rf, tds) =
  tds
  |> List.map (of_td ~loc)
  |> List.concat
