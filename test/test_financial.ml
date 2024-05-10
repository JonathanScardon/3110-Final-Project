(* modify_financial, modify_credit_data, remove_financial, string_financial *)

open OUnit2
open Test
open Final_project.Financial

let test1_modify_financial _ =
  assert_equal
    [ [ "credit_card"; "card1"; "10."; "0." ] ]
    (modify_financial "card1" "add" 10.
       [ [ "credit_card"; "card1"; "0."; "0." ] ]
       "credit_card")

let test2_modify_financial _ =
  assert_equal
    [
      [ "credit_card"; "card1"; "10."; "0." ];
      [ "credit_card"; "card2"; "10."; "0." ];
    ]
    (modify_financial "card1" "add" 10.
       [
         [ "credit_card"; "card1"; "0."; "0." ];
         [ "credit_card"; "card2"; "10."; "0." ];
       ]
       "credit_card")

let test3_modify_financial _ =
  assert_equal
    [
      [ "credit_card"; "card1"; "100."; "0." ];
      [ "credit_card"; "card2"; "10."; "0." ];
    ]
    (modify_financial "card1" "set" 100.
       [
         [ "credit_card"; "card1"; "10."; "0." ];
         [ "credit_card"; "card2"; "10."; "0." ];
       ]
       "credit_card")

let test4_modify_financial _ =
  assert_equal
    [
      [ "credit_card"; "card1"; "0."; "0." ];
      [ "credit_card"; "card2"; "10."; "0." ];
    ]
    (modify_financial "card1" "subtract" 10.
       [
         [ "credit_card"; "card1"; "10."; "0." ];
         [ "credit_card"; "card2"; "10."; "0." ];
       ]
       "credit_card")

let test5_modify_financial _ =
  assert_equal
    [
      [ "credit_card"; "card1"; "-10."; "0." ];
      [ "credit_card"; "card2"; "10."; "0." ];
    ]
    (modify_financial "card1" "subtract" 10.
       [
         [ "credit_card"; "card1"; "0."; "0." ];
         [ "credit_card"; "card2"; "10."; "0." ];
       ]
       "credit_card")

let suite =
  "Financial.ml"
  >::: [
         "modify_financial one-line add" >:: test1_modify_financial;
         "modify_financial two cards add" >:: test2_modify_financial;
         "modify_financial set" >:: test3_modify_financial;
         "modify_financial subtract" >:: test4_modify_financial;
         "modify_financial subtract negative" >:: test5_modify_financial;
         ( "modify_financial two cards add (different order)" >:: fun _ ->
           assert_equal
             [
               [ "credit_card"; "card1"; "0."; "0." ];
               [ "credit_card"; "card2"; "20."; "0." ];
             ]
             (modify_financial "card2" "add" 10.
                [
                  [ "credit_card"; "card1"; "0."; "0." ];
                  [ "credit_card"; "card2"; "10."; "0." ];
                ]
                "credit_card")
             ~printer:string_of_list_list );
         "modify_financial subtract (negative balance)"
         >:: test5_modify_financial;
         ( "modify_financial account and card entries" >:: fun _ ->
           assert_equal
             [
               [ "account"; "acc"; "10." ];
               [ "credit_card"; "card1"; "20."; "0." ];
               [ "credit_card"; "card2"; "10."; "0." ];
             ]
             (modify_financial "card1" "set" 20.
                [
                  [ "account"; "acc"; "10." ];
                  [ "credit_card"; "card1"; "10."; "0." ];
                  [ "credit_card"; "card2"; "10."; "0." ];
                ]
                "credit_card")
             ~printer:string_of_list_list );
         ( "modify_financial account and card have same name" >:: fun _ ->
           assert_equal
             [
               [ "account"; "card1"; "10." ];
               [ "credit_card"; "card1"; "20."; "0." ];
               [ "credit_card"; "card2"; "10."; "0." ];
             ]
             (modify_financial "card1" "set" 20.
                [
                  [ "account"; "card1"; "10." ];
                  [ "credit_card"; "card1"; "10."; "0." ];
                  [ "credit_card"; "card2"; "10."; "0." ];
                ]
                "credit_card")
             ~printer:string_of_list_list );
         ( "modify_financial multiple accounts" >:: fun _ ->
           assert_equal
             [
               [ "account"; "acc1"; "10." ];
               [ "account"; "acc3"; "100." ];
               [ "credit_card"; "card1"; "20."; "0." ];
               [ "credit_card"; "card2"; "10."; "0." ];
               [ "account"; "acc2"; "0." ];
             ]
             (modify_financial "acc3" "set" 100.
                [
                  [ "account"; "acc1"; "10." ];
                  [ "account"; "acc3"; "10." ];
                  [ "credit_card"; "card1"; "20."; "0." ];
                  [ "credit_card"; "card2"; "10."; "0." ];
                  [ "account"; "acc2"; "0." ];
                ]
                "account")
             ~printer:string_of_list_list );
         ( "modify_credit_data simple" >:: fun _ ->
           assert_equal
             [ [ "credit_card"; "card1"; "10."; "2." ] ]
             (modify_credit_data "card1" 2.
                [ [ "credit_card"; "card1"; "10."; "0." ] ])
             ~printer:string_of_list_list );
         ( "modify_credit_data zero" >:: fun _ ->
           assert_equal
             [ [ "credit_card"; "card1"; "10."; "10." ] ]
             (modify_credit_data "card1" 10.
                [ [ "credit_card"; "card1"; "10."; "0." ] ])
             ~printer:string_of_list_list );
         ( "modify_credit_data credit limit" >:: fun _ ->
           assert_raises CreditLimitReached (fun () ->
               modify_credit_data "card1" 2.
                 [ [ "credit_card"; "card1"; "10."; "9." ] ]) );
         ( "modify_credit_data credit limit 2" >:: fun _ ->
           assert_raises CreditLimitReached (fun () ->
               modify_credit_data "card1" 100.
                 [ [ "credit_card"; "card1"; "10."; "0." ] ]) );
         ( "remove_financial empty" >:: fun _ ->
           assert_equal []
             (remove_financial
                [ [ "credit_card"; "card1"; "10."; "0." ] ]
                "credit_card" "card1")
             ~printer:string_of_list_list );
         ( "remove_financial two cards, first" >:: fun _ ->
           assert_equal
             [ [ "credit_card"; "card2"; "10."; "0." ] ]
             (remove_financial
                [
                  [ "credit_card"; "card1"; "10."; "0." ];
                  [ "credit_card"; "card2"; "10."; "0." ];
                ]
                "credit_card" "card1")
             ~printer:string_of_list_list );
         ( "remove_financial two cards, second" >:: fun _ ->
           assert_equal
             [ [ "credit_card"; "card1"; "10."; "0." ] ]
             (remove_financial
                [
                  [ "credit_card"; "card1"; "10."; "0." ];
                  [ "credit_card"; "card2"; "10."; "0." ];
                ]
                "credit_card" "card2")
             ~printer:string_of_list_list );
         ( "remove_financial accounts and cards" >:: fun _ ->
           assert_equal
             [
               [ "credit_card"; "card1"; "10."; "0." ];
               [ "account"; "acc"; "10."; "0." ];
             ]
             (remove_financial
                [
                  [ "credit_card"; "card1"; "10."; "0." ];
                  [ "credit_card"; "card2"; "10."; "0." ];
                  [ "account"; "acc"; "10."; "0." ];
                ]
                "credit_card" "card2")
             ~printer:string_of_list_list );
         ( "remove_financial account end" >:: fun _ ->
           assert_equal
             [
               [ "credit_card"; "card1"; "10."; "0." ];
               [ "credit_card"; "card2"; "10."; "0." ];
               [ "account"; "acc1"; "10."; "0." ];
             ]
             (remove_financial
                [
                  [ "credit_card"; "card1"; "10."; "0." ];
                  [ "credit_card"; "card2"; "10."; "0." ];
                  [ "account"; "acc1"; "10."; "0." ];
                  [ "account"; "acc2"; "10."; "0." ];
                ]
                "account" "acc2")
             ~printer:string_of_list_list );
         ( "remove_financial account middle" >:: fun _ ->
           assert_equal
             [
               [ "credit_card"; "card1"; "10."; "0." ];
               [ "credit_card"; "card2"; "10."; "0." ];
               [ "account"; "acc2"; "10."; "0." ];
             ]
             (remove_financial
                [
                  [ "credit_card"; "card1"; "10."; "0." ];
                  [ "account"; "acc1"; "10."; "0." ];
                  [ "credit_card"; "card2"; "10."; "0." ];
                  [ "account"; "acc2"; "10."; "0." ];
                ]
                "account" "acc1")
             ~printer:string_of_list_list );
         ( "remove_financial account beginning" >:: fun _ ->
           assert_equal
             [
               [ "credit_card"; "card1"; "10."; "0." ];
               [ "account"; "acc1"; "10."; "0." ];
               [ "credit_card"; "card2"; "10."; "0." ];
               [ "account"; "acc2"; "10."; "0." ];
             ]
             (remove_financial
                [
                  [ "account"; "acc3"; "10."; "0." ];
                  [ "credit_card"; "card1"; "10."; "0." ];
                  [ "account"; "acc1"; "10."; "0." ];
                  [ "credit_card"; "card2"; "10."; "0." ];
                  [ "account"; "acc2"; "10."; "0." ];
                ]
                "account" "acc3")
             ~printer:string_of_list_list );
         ( "string_financial simple account" >:: fun _ ->
           assert_equal "acc3 10.\n"
             (string_financial [ [ "account"; "acc3"; "10." ] ] "account")
             ~printer:(fun x -> x) );
         ( "string_financial simple credit_card" >:: fun _ ->
           assert_equal "card1 10. 0.\n"
             (string_financial
                [ [ "credit_card"; "card1"; "10."; "0." ] ]
                "credit_card")
             ~printer:(fun x -> x) );
         ( "string_financial no accounts" >:: fun _ ->
           assert_equal "\n"
             (string_financial
                [ [ "credit_card"; "card1"; "10."; "0." ] ]
                "account")
             ~printer:(fun x -> x) );
         ( "string_financial no credit cards" >:: fun _ ->
           assert_equal "\n"
             (string_financial [ [ "account"; "acc"; "10." ] ] "credit_card")
             ~printer:(fun x -> x) );
         ( "string_financial credit card and account" >:: fun _ ->
           assert_equal "card1 10. 0.\n"
             (string_financial
                [
                  [ "account"; "acc3"; "10." ];
                  [ "credit_card"; "card1"; "10."; "0." ];
                ]
                "credit_card")
             ~printer:(fun x -> x) );
         ( "string_financial credit card and account" >:: fun _ ->
           assert_equal "acc3 10.\n"
             (string_financial
                [
                  [ "account"; "acc3"; "10." ];
                  [ "credit_card"; "card1"; "10."; "0." ];
                ]
                "account")
             ~printer:(fun x -> x) );
         ( "string_financial multiple credit cards, accounts" >:: fun _ ->
           assert_equal "acc3 10.\nacc1 10.\nacc2 20.\n"
             (string_financial
                [
                  [ "account"; "acc3"; "10." ];
                  [ "credit_card"; "card1"; "10."; "0." ];
                  [ "account"; "acc1"; "10." ];
                  [ "credit_card"; "card2"; "10."; "0." ];
                  [ "account"; "acc2"; "20." ];
                ]
                "account")
             ~printer:(fun x -> x) );
         ( "string_financial multiple credit cards, accounts" >:: fun _ ->
           assert_equal "card1 10. 0.\ncard2 10. 5.\n"
             (string_financial
                [
                  [ "account"; "acc3"; "10." ];
                  [ "credit_card"; "card1"; "10."; "0." ];
                  [ "account"; "acc1"; "10." ];
                  [ "credit_card"; "card2"; "10."; "5." ];
                  [ "account"; "acc2"; "20." ];
                ]
                "credit_card")
             ~printer:(fun x -> x) );
       ]

let _ = run_test_tt_main suite
let () = print_endline "financial tests succeeded"
