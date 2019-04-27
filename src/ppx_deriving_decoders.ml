open Ppxlib

let deriver =
  Deriving.add "decoder"
    ~str_type_decl:(Deriving.Generator.make_noarg Ppx_deriving_decoders_expander.str_type_decl)
