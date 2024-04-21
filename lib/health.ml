open Data
open ANSITerminal

let () = Random.self_init ()
let day = string_of_int (Unix.localtime (Unix.time ())).tm_mday
let month = string_of_int ((Unix.localtime (Unix.time ())).tm_mon + 1)
let year = string_of_int ((Unix.localtime (Unix.time ())).tm_year + 1900)
let curr_date = day ^ "-" ^ month ^ "-" ^ year
let hour = string_of_int (Unix.localtime (Unix.time ())).tm_hour
let min = string_of_int (Unix.localtime (Unix.time ())).tm_min
let time_of_day = hour ^ ":" ^ min

let add_health_data user journal =
  let msg1 =
    if journal = "food" then "What type of meal did you eat? "
    else "Which type of exercise did you do? "
  in
  let msg2 =
    if journal = "food" then "Describe the food that you ate: "
    else "How many hours did you spend exercising? "
  in
  let path = "data/" ^ user ^ "_" ^ journal ^ ".csv" in
  print_string [ Reset ] ("\nEnter 'back' to go back to the menu. \n" ^ msg1);
  let input1 = read_line () in
  if input1 = "back" then ()
  else
    let () = print_string [ Reset ] ("\n" ^ msg2) in
    let input2 =
      if journal = "exercise" then read_line () ^ " hr" else read_line ()
    in
    if input2 = "back" then ()
    else
      let data = input1 ^ "; " ^ input2 ^ "; " ^ time_of_day in
      print_string [ Foreground Green ] "\nEntry added successfully!\n";
      edit curr_date path data

(* let search_entry user =
   (* ask user whether they want to see food or exercise journal *)
   Data.search_entry user "" ("data/" ^ user ^ "_food.csv") *)
