open ANSITerminal

(*
let day = string_of_int (Unix.localtime (Unix.time ())).tm_mday
let month = string_of_int ((Unix.localtime (Unix.time ())).tm_mon + 1)
let year = string_of_int ((Unix.localtime (Unix.time ())).tm_year + 1900)
let curr_date = month ^ "-" ^ day ^ "-" ^ year
*)

let add_new_goal user =
  print_string [] "\n";
  print_string [] "What would you like to title your new goal?\n";
  let goal_name = read_line () in
  let path = "data/" ^ user ^ "_goals_list.csv" in
  if Data.search goal_name path then
    print_string [Bold; Foreground Red] "Error: you've already added this goal!\n"
  else
    begin
      Data.add_data (goal_name :: Mood.curr_date :: []) path;
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
  let csv_content = Csv.load path in
  match csv_content with
  | [] -> print_endline "No goals available."
  | _  ->
      print_string [Bold; Foreground Yellow] (Printf.sprintf "%-20s\t%s\n" "Goal" "Date Added");
      List.iter
        (fun row ->
          match row with
          | [goal; date] ->
              Printf.printf "%-20s\t%s\n" goal date  (* Ensure both columns have the same width *)
          | _ -> print_endline "Invalid data format")
        csv_content


let log_progress user =
  print_newline ();
  print_string [] "Which goal would like you like to log progress towards? ";
  let target_goal = read_line () in
  let path = "data/" ^ user ^ "_goals_list.csv" in
  if Data.search target_goal path then
    begin
    print_string [] "Please describe the progress you've made: ";
    let user_progress = read_line () in
    let goal_file_path = "data/" ^ user ^ "_" ^ target_goal ^ ".csv" in
    Data.add_data (user_progress :: Mood.curr_date :: []) goal_file_path;
    print_string [Bold; Foreground Green] "Progress successfully logged!\n";
    end
  else
    print_string [Bold; Foreground Red] "This goal does not exist"

