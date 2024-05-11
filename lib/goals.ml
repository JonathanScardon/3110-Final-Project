(*add colored output later; ocaml was throwing a fit*)

open ANSITerminal

let add_new_goal user =
  print_string [] "\n";
  print_string [] "What would you like to title your new goal?\n";
  let goal_name = read_line () in
  let path = "data/" ^ user ^ "_goals_list.csv" in
  if Data.search goal_name path then
    print_string [Bold; Foreground Red] "Error: you've already added this goal!\n"
  else
    begin
    Data.add_data (goal_name :: []) path;
    print_string [Bold; Foreground Green] "Goal added successfully!\n"
    end
