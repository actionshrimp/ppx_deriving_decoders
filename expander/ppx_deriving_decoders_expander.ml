open Ppxlib

module T = Ppxlib.Ast_builder.Default


(* { pld_name; pld_mutable; pld_type; pld_loc; pld_attributes } *)

let expr_of_rec_labels ~loc (rs : label_declaration list) =
  let final_record = T.pexp_record ~loc (rs |> List.map (fun r ->
      let ident = { txt = Longident.Lident r.pld_name.txt; loc = r.pld_name.loc } in
      (ident , T.pexp_ident ~loc:r.pld_name.loc ident)
    )) None in
  let suc = [%expr succeed [%e final_record]] in
  suc


let of_td ~loc (td: Ppxlib.type_declaration) =
  let n = td.ptype_name.txt in
  let decode_mod_sig =
    (T.pmty_ident ~loc
       { txt = Longident.Ldot (Longident.Ldot (Longident.Lident "Decoders", "Decode"), "S")
       ; loc })
  in
  match td.ptype_kind with
  | Ppxlib.Ptype_record rs ->
    [
      T.pstr_module ~loc
        { pmb_name = {txt = (Printf.sprintf "Decode_%s" n); loc }
        ; pmb_expr =
            T.pmod_functor ~loc {txt = "D"; loc }
              (Some decode_mod_sig)
              (let f_name = T.pvar ~loc (Printf.sprintf "decode_%s" n) in
               let f_type =
                 T.ptyp_constr ~loc {txt = (Longident.Lident "decoder"); loc }
                   [ T.ptyp_constr ~loc {txt = (Longident.Lident n); loc } []
                   ] in
               T.pmod_structure ~loc
                 [ [%stri open D]
                 ; [%stri
                   let [%p f_name] : [%t f_type] =
                     [%e expr_of_rec_labels ~loc rs]]
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
