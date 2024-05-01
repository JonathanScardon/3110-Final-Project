open OUnit2
open Final_project

let suite =
  "Tests"
  >::: [ ("test" >:: fun _ -> assert_equal "" "" ~printer:(fun x -> x)) ]

let () = run_test_tt_main suite
