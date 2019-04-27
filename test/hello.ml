type test_type =
  { hello : int
  ; there : string
  } [@@deriving_inline decoder]
[@@@end]

let x = 3
