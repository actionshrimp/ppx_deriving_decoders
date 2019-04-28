module Simple = struct

  type test_type =
    { name : string
    ; age : int
    ; details : string list
    } [@@deriving_inline decoder]
  [@@@end]

  let%expect_test "addition" =
    Printf.printf "%d" (1 + 2);
    [%expect {| 4 |}]

  type test_type_using =
    { test : test_type list
    } [@@deriving_inline decoder]
  [@@@end]

  let%expect_test "addition" =
    Printf.printf "%d" (1 + 2);
    [%expect {| 4 |}]

end
