open Data

let day = string_of_int (Unix.localtime (Unix.time ())).tm_mday
let month = string_of_int ((Unix.localtime (Unix.time ())).tm_mon + 1)
let year = string_of_int ((Unix.localtime (Unix.time ())).tm_year + 1900)
let curr_date = day ^ "-" ^ month ^ "-" ^ year

let rec happiness_log () =
  print_endline "Rate your happiness 1-10";
  let happiness_lvl = read_line () in
  let hap_lvl_int = int_of_string happiness_lvl in
  if hap_lvl_int >= 1 && hap_lvl_int <= 10 then happiness_lvl
  else happiness_log ()

let process_mood user =
  let path = "data/" ^ user ^ "_mood.csv" in
  let data = [ read_line () ] in
  add_data (curr_date :: happiness_log () :: data) path

let see_history user =
  print_endline "Would you like to limit the history you see? (y/n)";
  if read_line () = "y" then
    let () =
      print_endline "Up to what entry would you like to see? (enter a number)"
    in
    let limit = int_of_string (read_line ()) in
    print_endline (get_data ("data/" ^ user ^ "_mood.csv") (Some limit))
  else print_endline (get_data ("data/" ^ user ^ "_mood.csv") None)

let process_choice user =
  let choice = read_line () in
  match choice with
  | "1" -> ()
  | "2" -> see_history user
  | "3" ->
      print_endline "Exiting...";
      exit 0
  | _ -> print_endline "Invalid option. Please try again."

let mood_interface user =
  let path = "data/" ^ user ^ "_mood.csv" in
  print_endline ("Welcome to your mood tracker, " ^ user ^ "!");
  if search curr_date path then (
    print_endline "Would you like to:";
    print_endline "1. Add a small message for your future self?";
    print_endline "2. See your journal history?";
    print_endline "3. Exit";
    process_choice user)
  else (
    print_endline "Inspirational Quote";
    print_endline "How are you feeling today?";
    process_mood user)
