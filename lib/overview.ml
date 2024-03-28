open Mood

let rec process_choice user =
  let choice = read_line () in
  match choice with
  | "1" -> mood_interface user
  | "2" -> ()
  | "3" -> ()
  | "4" ->
      print_endline "Exiting...";
      exit 0
  | _ ->
      print_endline "Invalid option. Please try again.";
      dashboard_login user

and dashboard_login user =
  print_endline ("Welcome to your dashboard, " ^ user ^ "!");
  print_endline "1. Mood Tracker";
  print_endline "2. Diet Tracker";
  print_endline "3. Finances Tracker";
  print_endline "4. Exit";
  print_string "Please choose an option: ";
  process_choice user
