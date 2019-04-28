type test_type =
  { name : string
  ; age : int
  ; details : string list
  } [@@deriving_inline decoder]
[@@@end]

type test_type_using =
  { test : test_type list
  } [@@deriving_inline decoder]
[@@@end]
