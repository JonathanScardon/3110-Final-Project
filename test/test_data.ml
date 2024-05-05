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
       ]

let () = run_tests suite
let () = print_endline "data tests succeeded"
