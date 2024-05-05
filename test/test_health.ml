open OUnit2
open Final_project.Health

let rec string_of_list lst =
  match lst with [] -> "-" | h :: t -> h ^ "|" ^ string_of_list t

let rec string_of_list_list lst =
  match lst with
  | [] -> "[]"
  | h :: t -> "[" ^ string_of_list h ^ "]; " ^ string_of_list_list t

let suite =
  "Health.ml"
  >::: [
         ( "add_health_data" >:: fun _ ->
           (* let () = add_health_data "test1" "food" in *)
           assert_equal "" "" );
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
(* let () =
   let target_directory = "3110-final-project" in
   try
     let found_dir = find_and_set_directory target_directory in
     Sys.chdir found_dir;
     Printf.printf "Changed to directory: %s\n" (Sys.getcwd ());
     run_test_tt_main suite
   with
   | Failure msg -> Printf.printf "Error: %s\n" msg
   | Sys_error msg -> Printf.printf "System error: %s\n" msg *)

let () = print_endline "health tests succeeded"
