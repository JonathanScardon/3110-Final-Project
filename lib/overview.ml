(* overview.ml *)
open Data
open ANSITerminal

(* open Lwt *)
(* open Financial
   open Financial_stock *)

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
    print_string [ Reset ] "How are you feeling today? ";
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
      Unix.sleep 1;
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
      Unix.sleep 1;
      mood_interface user
  | "5" ->
      Mood.remove_curr_quote user rand_quote;
      Unix.sleep 1;
      mood_interface user
  | "6" -> dashboard_login user
  | _ ->
      print_endline "Invalid option. Please try again.";
      Unix.sleep 1;
      mood_interface user

(* health interface *)

and health_interface user =
  print_string [ Bold; Foreground Blue ] "\nHealth Tracker\n";
  print_strings [ Reset ]
    [
      "Would you like to:\n";
      "1. Add to your food journal?\n";
      "2. Add to your exercise journal?\n";
      "3. See your journal history?\n";
      "4. Search for a particular day?\n";
      "5. Remove a journal entry?\n";
      "6. Make a meal plan?\n";
      "7. Exit\n";
    ];
  print_string [ Bold ] "Please choose an option (1-7): ";
  health_input user

and health_input user =
  let choice = read_line () in
  match choice with
  | "1" ->
      Health.add_health_data user "food";
      health_interface user
  | "2" ->
      Health.add_health_data user "exercise";
      health_interface user
  | "3" ->
      Health.select_journal user Health.see_history
        "Would you like to see your food or exercise journal? ";
      health_interface user
  | "4" ->
      Health.select_journal user Health.search_entry
        "Would you like to search in your food or exercise journal? ";
      health_interface user
  | "5" ->
      Health.select_journal user Health.remove_entry
        "Would you like to delete an entry in your food or exercise journal? ";
      health_interface user
  | "6" ->
      mealplan_interface user;
      health_interface user
  | "7" -> dashboard_login user
  | _ ->
      print_endline "Invalid option. Please try again.";
      health_interface user

and mealplan_interface user =
  print_string [ Bold; Foreground Blue ] "\nMeal Planner\n";
  print_strings [ Reset ]
    [
      "Would you like to:\n";
      "1. Generate a meal plan?\n";
      "2. Add meal ideas?\n";
      "3. Remove meal ideas?\n";
      "4. View meal ideas?\n";
      "5. Exit\n";
    ];
  print_string [ Bold ] "Please choose an option (1-5): ";
  meal_input user

and meal_input user =
  let choice = read_line () in
  match choice with
  | "1" ->
      Health.mealplan user (get_n_days ());
      Unix.sleep 2;
      mealplan_interface user
  | "2" ->
      Health.add_meal user;
      mealplan_interface user
  | "3" ->
      Health.remove_meal user;
      mealplan_interface user
  | "4" ->
      Health.view_meal user;
      mealplan_interface user
  | "5" -> health_interface user
  | _ ->
      print_endline "Invalid option. Please try again.";
      mealplan_interface user

and get_n_days () =
  print_string [ Reset ] "How many days would you like to meal plan? ";
  let days = read_line () in
  try
    let n = int_of_string days in
    if n > 0 then n
    else (
      print_string [ Foreground Red ] "\nSorry, this number is invalid!\n";
      get_n_days ())
  with _ ->
    print_string [ Foreground Red ] "\nSorry, this number is invalid!\n";
    get_n_days ()

(* dashboard interface *)

and process_choice user =
  let choice = read_line () in
  match choice with
  | "1" -> mood_interface user
  | "2" -> health_interface user
  | "3" -> financial_interface user
  | "4" -> ()
  | "5" ->
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
      "4. Goal Tracker\n";
      "5. Exit\n";
    ];
  print_string [ Bold ] "Please choose an option (1-5): ";
  process_choice user

(* financial interface *)

and financial_interface user =
  print_string [ Reset; Bold; Foreground Green ] "\nFinancial Tracker\n";
  print_strings [ Reset ]
    [
      "1. View personal stock spread\n";
      "2. View all bank accounts\n";
      "3. Add new account to bank\n";
      "4. Edit funds in bank accounts\n";
      "5. Return to main menu\n";
    ];
  print_string [ Bold ] "Please enter a command: ";
  financial_input user

and financial_input user =
  let choice = read_line () in
  match choice with
  | "1" ->
      manage_stock_options user;
      financial_interface user
  | "2" ->
      (* view_all_banks user; *)
      financial_interface user
  | "3" ->
      Financial.prompt_add_account user;
      financial_interface user
  | "4" ->
      Financial.prompt_edit_account user;
      financial_interface user
  | "5" -> dashboard_login user
  | _ ->
      print_endline "Invalid option. Please try again.";
      financial_interface user

and manage_stock_options user =
  print_string [ Reset; Bold; Foreground Blue ] "\nStock Management\n";
  print_strings [ Reset ]
    [
      "1. Add a new stock\n";
      "2. Remove a stock\n";
      "3. Modify a stock\n";
      "4. Update stock prices\n";
      "5. View stocks\n";
      "6. Return to financial menu\n";
    ];
  print_string [ Bold ] "Select an option: ";
  stock_input user

and stock_input user =
  let choice = read_line () in
  match choice with
  | "1" ->
      (* prompt_add_stock user; *)
      manage_stock_options user
  | "2" ->
      (* prompt_remove_stock user; *)
      manage_stock_options user
  | "3" ->
      (* prompt_modify_stock user; *)
      manage_stock_options user
  | "4" ->
      (* Financial.update_stock_prices user; *)
      manage_stock_options user
  | "5" ->
      (* Financial.view_stock_spread user; *)
      manage_stock_options user
  | "6" -> financial_interface user
  | _ ->
      print_endline "Invalid option. Please try again.";
      manage_stock_options user

(* and prompt_add_stock user =
     let () = print_string [ Reset ] "Enter stock symbol: " in
     let symbol = read_line () in
     let () = print_string [ Reset ] "Enter number of shares: " in
     let shares = read_line () in
     let () = print_string [ Reset ] "Enter purchase price: " in
     let price = read_line () in
     Financial.add_stock user symbol (int_of_string shares) (float_of_string price)

   and prompt_remove_stock user =
     let () = print_string [ Reset ] "Enter stock symbol to remove: " in
     let symbol = read_line () in
     Financial.remove_stock user symbol

   and prompt_modify_stock user =
     let () = Financial.view_stock_spread user in
     let () = print_string [ Reset ] "Enter stock index to modify: " in
     let index = read_line () in
     let () = print_string [ Reset ] "Enter new stock symbol: " in
     let symbol = read_line () in
     let () = print_string [ Reset ] "Enter new number of shares: " in
     let shares = read_line () in
     let () = print_string [ Reset ] "Enter new purchase price: " in
     let price = read_line () in
     let () = print_string [ Reset ] "Enter last known price: " in
     let last_price = read_line () in
     Financial.modify_stock user (int_of_string index) symbol
       (int_of_string shares) (float_of_string price)
       (float_of_string last_price) *)
