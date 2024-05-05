open OUnit2
open Test
open Final_project.Data

(* testing get_data, search, search2, find_entry, data_to_list *)

let suite =
  "Data.ml"
  >::: [
         ( "get_data empty no limit" >:: fun _ ->
           assert_equal "\n -- \n"
             (get_data "data/for_testing/test1_quotes.csv" None)
             ~printer:(fun x -> x) );
         ( "get_data empty limit" >:: fun _ ->
           assert_equal "\n -- \n"
             (get_data "data/for_testing/test1_quotes.csv" (Some 1))
             ~printer:(fun x -> x) );
         ( "get_data column no limit" >:: fun _ ->
           assert_equal "\nquote 1\nquote 2\nquote 3\n"
             (get_data "data/for_testing/test2_quotes.csv" None)
             ~printer:(fun x -> x) );
         ( "get_data column limit 1" >:: fun _ ->
           assert_equal "\nquote 1\n"
             (get_data "data/for_testing/test2_quotes.csv" (Some 1))
             ~printer:(fun x -> x) );
         ( "get_data column limit 2" >:: fun _ ->
           assert_equal "\nquote 1\nquote 2\n"
             (get_data "data/for_testing/test2_quotes.csv" (Some 2))
             ~printer:(fun x -> x) );
         ( "get_data column limit 0" >:: fun _ ->
           assert_equal "\n\n"
             (get_data "data/for_testing/test2_quotes.csv" (Some 0))
             ~printer:(fun x -> x) );
         ( "get_data rows no limit" >:: fun _ ->
           assert_equal "\na b c\nd e f\ng h i\n"
             (get_data "data/for_testing/test_data.csv" None) ~printer:(fun x ->
               x) );
         ( "get_data rows limit" >:: fun _ ->
           assert_equal "\na b c\nd e f\n"
             (get_data "data/for_testing/test_data.csv" (Some 2))
             ~printer:(fun x -> x) );
       ]

let () = run_tests suite
let () = print_endline "data tests succeeded"
