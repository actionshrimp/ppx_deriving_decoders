module T = Ppxlib.Ast_builder.Default

let str_type_decl ~loc ~path:_ (_rf, _tds) =
  [T.pstr_value ~loc Nonrecursive
    [{ pvb_pat = T.pvar ~loc "hello"
     ; pvb_expr = T.estring ~loc "world"; pvb_attributes=[]; pvb_loc=loc
     }]
  ]
