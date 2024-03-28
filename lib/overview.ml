open Data

let rec mood_interface user =
  let path = "data/" ^ user ^ "_mood.csv" in
  print_endline "\nMood Tracker";
  if search Mood.curr_date path then (
    print_endline "Would you like to:";
    print_endline "1. Add a small message for your future self?";
    print_endline "2. See your journal history?";
    print_endline "3. Search for a particular day?";
    print_endline "4. Remove a journal entry?";
    print_endline "5. Exit";
    print_string "Please choose an option: ";
    after_mood_input user)
  else (
    print_endline "Inspirational Quote";
    print_string "How are you feeling today? ";
    process_mood user)

and process_mood user =
  let path = "data/" ^ user ^ "_mood.csv" in
  let data = [ read_line () ] in
  add_data (Mood.curr_date :: Mood.happiness_log () :: data) path;
  mood_interface user

and after_mood_input user =
  let choice = read_line () in
  match choice with
  | "1" -> ()
  | "2" ->
      Mood.see_history user;
      mood_interface user
  | "3" ->
      Mood.search_entry user;
      mood_interface user
  | "4" ->
      Mood.remove_entry user;
      mood_interface user
  | "5" -> dashboard_login user
  | _ ->
      print_endline "Invalid option. Please try again.";
      mood_interface user

and process_choice user =
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
  print_endline ("\nHello, " ^ user);
  print_endline "1. Mood Tracker";
  print_endline "2. Health Tracker";
  print_endline "3. Finances Tracker";
  print_endline "4. Exit";
  print_string "Please choose an option: ";
  process_choice user
