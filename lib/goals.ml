open ANSITerminal


let incomplete_goals_path user = 
  "data/" ^ user ^ "_incomplete_goals.csv"


let complete_goals_path user =
  "data/" ^ user ^ "_complete_goals.csv"


let add_new_goal user =
  print_string [] "\n";
  print_string [] "What would you like to title your new goal?\n";
  let goal_name = read_line () in
  let path = incomplete_goals_path user in
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


let display_goals path header1 header2 =
  print_newline ();
  let csv_content = Csv.load path in
  match csv_content with
  | [] -> print_endline "No available data."
  | _  ->
      print_string [Bold; Foreground Yellow] (Printf.sprintf "%-20s\t%s\n" header1 header2);
      List.iter
        (fun row ->
          match row with
          | [goal; date] ->
              Printf.printf "%-20s\t%s\n" goal date  (* Ensure both columns have the same width *)
          | _ -> print_endline "Invalid data format")
        csv_content


let view_incomplete_goals user =
  let path = incomplete_goals_path user in
  display_goals path "In-Progress Goals" "Date Added"


let view_complete_goals user =
  let path = complete_goals_path user in
  display_goals path "Complete Goals" "Date Completed"


let view_progress_log user =
  print_newline ();
  print_string [] "Which goal's progress log do you want to view? ";
  let target_goal = read_line () in
  let path_one = incomplete_goals_path user in
  let path_two = complete_goals_path user in
  let header1 = "Progress Description" in
  let header2 = "Date" in
  if Data.search target_goal path_one || Data.search target_goal path_two then
    let log_path = "data/" ^ user ^ "_" ^ target_goal ^ ".csv" in
    display_goals log_path header1 header2
  else
    print_string [Bold;Foreground Red] "Error: Goal not found\n"



let log_progress user =
  print_newline ();
  print_string [] "Which goal would like you like to log progress towards? ";
  let target_goal = read_line () in
  let path = incomplete_goals_path user in
  if Data.search target_goal path then
    begin
    print_string [] "Please describe the progress you've made: ";
    let user_progress = read_line () in
    let goal_file_path = "data/" ^ user ^ "_" ^ target_goal ^ ".csv" in
    Data.add_data (user_progress :: Mood.curr_date :: []) goal_file_path;
    print_string [Bold; Foreground Green] "Progress successfully logged!\n";
    end
  else
    print_string [Bold;Foreground Red] "Error: Goal not found\n"



let remove_goal_helper user goal_id =
  let path = incomplete_goals_path user in
  let original_goals = Csv.load path in
  let filtered_goals = List.filter (fun goal -> List.hd goal <> goal_id) original_goals in
  Csv.save path filtered_goals


let remove_goal user =
  print_newline ();
  print_string [] "Which goal would like you to remove? Note that only incomplete goals can be removed. ";
  let target_goal = read_line () in
  let path = incomplete_goals_path user in
  if Data.search target_goal path then
    begin
      print_string [] ("Are you sure you would like to remove " ^ target_goal ^ "? [y/n] ");
      let rec prompt_user () =
        let user_response = String.uppercase_ascii (read_line ()) in
        match user_response with
        | "Y" ->
          Sys.remove ("data/" ^ user ^ "_" ^ target_goal ^ ".csv");
          remove_goal_helper user target_goal;
          print_string [Bold;Foreground Green] (target_goal ^ " successfully removed.\n")
        | "N" ->
          ()
        | _ ->
          print_string [] "Please enter 'y' for yes or 'n' for no: ";
          prompt_user ()
      in
      prompt_user ()
    end
  else
    print_string [Bold;Foreground Red] "Error: Goal not found\n"


let complete_goal user =
  print_newline ();
  print_string [] "Which goal would you like to mark as complete? ";
  let target_goal = read_line () in
  let incomplete_goal_path = incomplete_goals_path user in
  if Data.search target_goal incomplete_goal_path then
    begin
    remove_goal_helper user target_goal;
    (*adds completed to progress log*)
    let goal_file_path = "data/" ^ user ^ "_" ^ target_goal ^ ".csv" in
    Data.add_data ("Completed!" :: Mood.curr_date :: []) goal_file_path;
    (*adds goal to file with completed goals*)
    let complete_goal_path = complete_goals_path user in
    Data.add_data (target_goal :: Mood.curr_date :: []) complete_goal_path;    
    print_string [Bold;Foreground Green] ("Congratulations on accomplishing your goal! " ^ target_goal ^ " is now marked as complete.\n");
    end
  else
    print_string [Bold;Foreground Red] "Error: Goal not found\n"


let delete_goal_logs user =
  let incomplete_goals = (Data.data_to_list (incomplete_goals_path user)) in
  let all_goals = incomplete_goals @ (Data.data_to_list (complete_goals_path user)) in
  let i = ref 0 in  
  while !i < List.length all_goals do
    let target_goal = List.nth all_goals !i in
    Sys.remove ("data/" ^ user ^ "_" ^ target_goal ^".csv");
    i := !i + 2;
  done
