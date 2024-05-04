(* test modify_financial and modify_credit_data *)

open OUnit2
open Final_project.Financial

let rec string_of_list lst =
  match lst with [] -> "-" | h :: t -> h ^ "|" ^ string_of_list t

let rec string_of_list_list lst =
  match lst with
  | [] -> "[]"
  | h :: t -> "[" ^ string_of_list h ^ "]; " ^ string_of_list_list t

let test1_modify_financial _ =
  assert_equal
    [ [ "credit_card"; "card1"; "10." ] ]
    (modify_financial "card1" "add" 10.
       [ [ "credit_card"; "card1"; "0." ] ]
       "credit_card")

let test2_modify_financial _ =
  assert_equal
    [ [ "credit_card"; "card1"; "10." ]; [ "credit_card"; "card2"; "10." ] ]
    (modify_financial "card1" "add" 10.
       [ [ "credit_card"; "card1"; "0." ]; [ "credit_card"; "card2"; "10." ] ]
       "credit_card")

let test3_modify_financial _ =
  assert_equal
    [ [ "credit_card"; "card1"; "100." ]; [ "credit_card"; "card2"; "10." ] ]
    (modify_financial "card1" "set" 100.
       [ [ "credit_card"; "card1"; "10." ]; [ "credit_card"; "card2"; "10." ] ]
       "credit_card")

let test4_modify_financial _ =
  assert_equal
    [ [ "credit_card"; "card1"; "0." ]; [ "credit_card"; "card2"; "10." ] ]
    (modify_financial "card1" "subtract" 10.
       [ [ "credit_card"; "card1"; "10." ]; [ "credit_card"; "card2"; "10." ] ]
       "credit_card")

let test5_modify_financial _ =
  assert_equal
    [ [ "credit_card"; "card1"; "-10." ]; [ "credit_card"; "card2"; "10." ] ]
    (modify_financial "card1" "subtract" 10.
       [ [ "credit_card"; "card1"; "0." ]; [ "credit_card"; "card2"; "10." ] ]
       "credit_card")

let suite =
  "Financial.ml"
  >::: [
         "modify_financial one-line add" >:: test1_modify_financial;
         "modify_financial two cards add" >:: test2_modify_financial;
         "modify_financial set" >:: test3_modify_financial;
         "modify_financial subtract" >:: test4_modify_financial;
         "modify_financial subtract (negative balance)"
         >:: test5_modify_financial;
         ( "modify_financial account and card have same name" >:: fun _ ->
           assert_equal
             [
               [ "account"; "card1"; "10." ];
               [ "credit_card"; "card1"; "20." ];
               [ "credit_card"; "card2"; "10." ];
             ]
             (modify_financial "card1" "set" 20.
                [
                  [ "account"; "card2"; "10." ];
                  [ "credit_card"; "card1"; "10." ];
                  [ "credit_card"; "card2"; "10." ];
                ]
                "credit_card")
             ~printer:string_of_list_list );
       ]

(* Cite ChatGPT for appropriate navigation to correct root bc otherwise
   it wouldn't work right weirdly. *)
let rec find_and_set_directory target_dir =
  let current_dir = Sys.getcwd () in
  if Filename.basename current_dir = target_dir then current_dir
  else if current_dir = "/" then
    failwith "Reached the root directory, target directory not found."
  else (
    Sys.chdir "../";
    find_and_set_directory target_dir)

(* WE REALLY NEED TO MAKE SURE WE CHANGE THE DIRECTORY TO THE RIGHT NAME
   WHEN WE SUBMIT *)
let () =
  let target_directory = "3110-final-project" in
  try
    let found_dir = find_and_set_directory target_directory in
    Sys.chdir found_dir;
    Printf.printf "Changed to directory: %s\n" (Sys.getcwd ());
    run_test_tt_main suite
  with
  | Failure msg -> Printf.printf "Error: %s\n" msg
  | Sys_error msg -> Printf.printf "System error: %s\n" msg

let _ = run_test_tt_main suite
let () = print_endline "financial tests succeeded"
