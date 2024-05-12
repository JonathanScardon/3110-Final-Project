open ANSITerminal

let day = string_of_int (Unix.localtime (Unix.time ())).tm_mday
let month = string_of_int ((Unix.localtime (Unix.time ())).tm_mon + 1)
let year = string_of_int ((Unix.localtime (Unix.time ())).tm_year + 1900)
let curr_date = month ^ "-" ^ day ^ "-" ^ year

let add_new_goal user =
  print_string [] "\n";
  print_string [] "What would you like to title your new goal?\n";
  let goal_name = read_line () in
  let path = "data/" ^ user ^ "_goals_list.csv" in
  if Data.search goal_name path then
    print_string [Bold; Foreground Red] "Error: you've already added this goal!\n"
  else
    begin
      (* Now also adding the date to the goal information *)
      let entry = goal_name ^ "," ^ curr_date in
      Data.add_data [entry] path;
      let goal_file_path = "data/" ^ user ^ "_" ^ goal_name ^ ".csv" in
      try
        Csv.save goal_file_path []; (* Initialize the goal file with the goal name and date *)
        print_string [Bold; Foreground Green] "Goal added successfully!\n"
      with
      | Sys_error msg ->
        print_string [Foreground Red]
          (Printf.sprintf "Failed to create file %s: %s\n" goal_file_path msg)
    end

let display_all_goals user =
  print_newline ();
  let path = "data/" ^ user ^ "_goals_list.csv" in
  let goals_list = Data.data_to_list path in
  print_string [Bold; Foreground Yellow] "Goal             Date Added\n";
  List.iter (fun goal_line ->
    match String.split_on_char ',' goal_line with
    | [goal_name; date_added] -> 
      Printf.printf "%-16s %s\n" goal_name date_added
    | _ -> print_string [Foreground Red] "Error: Invalid goal entry format.\n"
  ) goals_list

