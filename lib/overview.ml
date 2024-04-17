open Data
open ANSITerminal

let print_strings style lines =
  List.iter (fun line -> print_string style line) lines

(* mood interface *)

let rec mood_interface user =
  let path = "data/" ^ user ^ "_mood.csv" in
  print_string [ Bold; Foreground Yellow ] "\nMood Tracker\n";
  let rand_quote = Mood.get_random_quote user in
  print_string [ Reset; Foreground Cyan ] (rand_quote ^ "\n");
  if search Mood.curr_date path then (
    print_strings [ Reset ]
      [
        "Would you like to:\n";
        "1. Add a small message for your future self?\n";
        "2. See your journal history?\n";
        "3. Search for a particular day?\n";
        "4. Remove a journal entry?\n";
        "5. Remove currently displayed quote?\n";
        "6. Exit\n";
      ];
    print_string [ Bold ] "Please choose an option (1-6): ";
    after_mood_input user rand_quote)
  else (
    print_string [ Bold ] "How are you feeling today? ";
    process_mood user)

and process_mood user =
  let path = "data/" ^ user ^ "_mood.csv" in
  let data = [ read_line () ] in
  add_data (Mood.curr_date :: Mood.happiness_log () :: data) path;
  mood_interface user

and after_mood_input user rand_quote =
  let choice = read_line () in
  match choice with
  | "1" ->
      Mood.add_quote user;
      Unix.sleep 2;
      mood_interface user
  | "2" ->
      Mood.see_history user;
      Unix.sleep 2;
      mood_interface user
  | "3" ->
      Mood.search_entry user;
      Unix.sleep 2;
      mood_interface user
  | "4" ->
      Mood.remove_entry user;
      Unix.sleep 2;
      mood_interface user
  | "5" ->
      Mood.remove_curr_quote user rand_quote;
      Unix.sleep 2;
      mood_interface user
  | "6" -> dashboard_login user
  | _ ->
      print_endline "Invalid option. Please try again.";
      Unix.sleep 10;
      mood_interface user

(* health interface *)

and health_interface user =
  print_string [ Bold; Foreground Yellow ] "\nHealth Tracker\n";
  print_strings [ Reset ]
    [
      "Would you like to:\n";
      "1. Add to your food journal?\n";
      "2. Add to your exercise journal?\n";
      "3. See your journal history?\n";
      "4. Search for a particular day?\n";
      "5. Remove a journal entry?\n";
      "6. Exit\n";
    ];
  print_string [ Bold ] "Please choose an option (1-6): ";
  health_input user

and health_input user =
  let choice = read_line () in
  match choice with
  | "1" ->
      ();
      health_interface user
  | "2" ->
      ();
      health_interface user
  | "3" ->
      ();
      health_interface user
  | "4" ->
      ();
      health_interface user
  | "5" ->
      ();
      health_interface user
  | "6" -> dashboard_login user
  | _ ->
      print_endline "Invalid option. Please try again.";
      health_interface user

(* dashboard interface *)

and process_choice user =
  let choice = read_line () in
  match choice with
  | "1" -> mood_interface user
  | "2" -> health_interface user
  | "3" -> ()
  | "4" ->
      print_endline "Exiting...";
      exit 0
  | _ -> dashboard_login user

and dashboard_login user =
  print_string [ Reset ] "\n";
  print_string [ Bold; Foreground Green ] ("\nHello, " ^ user ^ "\n");
  print_strings [ Reset ]
    [
      "1. Mood Tracker\n";
      "2. Health Tracker\n";
      "3. Finances Tracker\n";
      "4. Exit\n";
    ];
  print_string [ Bold ] "Please choose an option (1-4): ";
  process_choice user
