type test_type =
  { hello : int
  ; there : string
  } [@@deriving_inline decoder]
[@@@end]


let%expect_test "addition" =
  Printf.printf "%d" (1 + 2);
  [%expect {| 4 |}]
