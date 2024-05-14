(* overview.ml *)
open Data
open ANSITerminal

let print_strings style lines =
  List.iter (fun line -> print_string style line) lines

let rec get_valid_int prompt error_msg =
  print_string [ Reset ] prompt;
  match read_line () with
  | "back" -> None
  | input -> (
      try Some (int_of_string input)
      with Failure _ ->
        print_string [ Foreground Red ] (error_msg ^ "\n");
        get_valid_int prompt error_msg)

let rec get_valid_float prompt error_msg =
  print_string [ Reset ] prompt;
  match read_line () with
  | "back" -> None
  | input -> (
      try Some (float_of_string input)
      with Failure _ ->
        print_string [ Foreground Red ] ("\n" ^ error_msg ^ "\n");
        get_valid_float prompt error_msg)

let rec get_valid_string prompt error_msg =
  print_string [ Reset ] prompt;
  let input = read_line () in
  if input = "" || input = "back" then
    if input = "back" then None
    else (
      print_string [ Foreground Red ] ("\n" ^ error_msg ^ "\n");
      get_valid_string prompt error_msg)
  else Some input

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
  | "4" -> goals_interface user
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
      "1. Manage stocks\n";
      "2. Manage bank accounts\n";
      "3. Manage credit cards\n";
      "4. Manage transactions\n";
      "5. Return to main menu\n";
    ];
  print_string [ Bold ] "Please choose an option (1-5): ";
  financial_input user

and financial_input user =
  let choice = read_line () in
  match choice with
  | "1" -> manage_stock_options user
  | "2" -> manage_accounts user
  | "3" -> manage_credit user
  | "4" -> manage_transac user
  | "5" -> dashboard_login user
  | _ ->
      print_endline "Invalid option. Please try again.";
      financial_interface user

and manage_accounts user =
  print_string [ Reset; Bold; Foreground Blue ] "\nBank Accounts\n";
  print_strings [ Reset ]
    [
      "1. View all bank accounts\n";
      "2. Add new account to bank\n";
      "3. Remove account from bank\n";
      "4. Edit funds in bank accounts\n";
      "5. Return to financial menu\n";
    ];
  print_string [ Bold ] "Please choose an option (1-5): ";
  account_input user

and account_input user =
  let choice = read_line () in
  match choice with
  | "1" ->
      Financial.view_financial user "account";
      Unix.sleep 2;
      manage_accounts user
  | "2" ->
      Financial.prompt_add_account user;
      manage_accounts user
  | "3" ->
      Financial.remove_account user;
      manage_accounts user
  | "4" ->
      Financial.prompt_edit_account user;
      manage_accounts user
  | "5" -> financial_interface user
  | _ ->
      print_endline "Invalid option. Please try again.";
      manage_accounts user

and manage_credit user =
  print_string [ Reset; Bold; Foreground Blue ] "\nCredit Cards\n";
  print_strings [ Reset ]
    [
      "1. View credit cards\n";
      "2. Add new credit card\n";
      "3. Remove credit card\n";
      "4. Pay off credit card\n";
      "5. Return to financial menu\n";
    ];
  print_string [ Bold ] "Please choose an option (1-5): ";
  credit_input user

and credit_input user =
  let choice = read_line () in
  match choice with
  | "1" ->
      Financial.view_financial user "credit_card";
      Unix.sleep 2;
      manage_credit user
  | "2" ->
      Financial.add_credit_card user;
      manage_credit user
  | "3" ->
      Financial.remove_credit user;
      manage_credit user
  | "4" ->
      Financial.prompt_pay_credit user;
      manage_credit user
  | "5" -> financial_interface user
  | _ ->
      print_endline "Invalid option. Please try again.";
      manage_credit user

and manage_transac user =
  print_string [ Reset; Bold; Foreground Blue ] "\nTransactions\n";
  print_strings [ Reset ]
    [
      "1. Make a transaction\n";
      "2. See transaction log\n";
      "3. Return to financial menu\n";
    ];
  print_string [ Bold ] "Please choose an option (1-3): ";
  transac_input user

and transac_input user =
  let choice = read_line () in
  match choice with
  | "1" ->
      Financial.make_transaction user;
      manage_transac user
  | "2" ->
      Financial.view_transactions user;
      manage_transac user
  | "3" -> financial_interface user
  | _ ->
      print_endline "Invalid option. Please try again.";
      manage_transac user

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
  print_string [ Bold ] "Please choose an option (1-6): ";
  stock_input user

and stock_input user =
  let choice = read_line () in
  match choice with
  | "1" ->
      prompt_add_stock user;
      manage_stock_options user
  | "2" ->
      prompt_remove_stock user;
      Financial_stock.display_stocks ("data/" ^ user ^ "_stock_financials.csv");
      manage_stock_options user
  | "3" ->
      prompt_modify_stock user;
      Financial_stock.display_stocks ("data/" ^ user ^ "_stock_financials.csv");
      manage_stock_options user
  | "4" ->
      Financial_stock.update_stock_prices user;
      Financial_stock.display_stocks ("data/" ^ user ^ "_stock_financials.csv");
      manage_stock_options user
  | "5" ->
      Financial_stock.display_stocks ("data/" ^ user ^ "_stock_financials.csv");
      manage_stock_options user
  | "6" -> financial_interface user
  | _ ->
      print_endline "Invalid option. Please try again.";
      manage_stock_options user

and prompt_add_stock user =
  match
    get_valid_string "\nEnter stock symbol or type 'back' to return: "
      "\nPlease enter a valid symbol or type 'back' to return."
  with
  | None -> manage_stock_options user
  | Some symbol ->
      if String.length symbol <= 5 && String.length symbol > 0 then
        let symbol = String.uppercase_ascii symbol in
        match
          get_valid_int "\nEnter number of shares: "
            "\nPlease enter a valid integer number of shares."
        with
        | None -> manage_stock_options user
        | Some shares -> (
            match
              get_valid_float
                "\nEnter purchase price (use a decimal point for cents): "
                "\nPlease enter a valid price."
            with
            | None -> manage_stock_options user
            | Some price ->
                Financial_stock.add_stock user symbol shares price;
                print_string [ Foreground Green ]
                  "\nStock added successfully!\n";
                manage_stock_options user)
      else
        print_string [ Foreground Red ]
          "\nPlease enter a valid stock symbol! (1-5 characters)\n"

and prompt_remove_stock user =
  match
    get_valid_string "\nEnter stock symbol to remove or type 'back' to return: "
      "\nPlease enter a valid symbol or type 'back' to return."
  with
  | None -> manage_stock_options user
  | Some symbol ->
      Financial_stock.remove_stock user symbol;
      manage_stock_options user

and prompt_modify_stock user =
  Financial_stock.view_stock_spread user;
  match
    get_valid_string "\nEnter stock symbol to modify or type 'back' to return: "
      "\nPlease enter a valid symbol."
  with
  | Some symbol when symbol <> "back" -> (
      let symbol = String.capitalize_ascii symbol in
      let path = "data/" ^ user ^ "_stock_financials.csv" in
      if not (Data.search symbol path) then (
        print_string [ Foreground Red ] "\nStock symbol not found.\n";
        prompt_modify_stock user)
      else
        match
          get_valid_int "\nEnter new number of shares: "
            "\nPlease enter a valid integer number of shares."
        with
        | Some shares -> (
            match
              get_valid_float
                "\nEnter new purchase price (use a decimal point for cents): "
                "\nPlease enter a valid price."
            with
            | Some purchase_price -> (
                match
                  get_valid_float
                    "\nEnter last known price (use a decimal point for cents): "
                    "\nPlease enter a valid price."
                with
                | Some last_price ->
                    Financial_stock.modify_stock user symbol shares
                      purchase_price last_price;
                    manage_stock_options user
                | None -> manage_stock_options user)
            | None -> manage_stock_options user)
        | None -> manage_stock_options user)
  | _ -> manage_stock_options user

(* goals interface *)

and goals_interface user =
  print_string [ Bold; Foreground Magenta ] "\nGoals Tracker\n";
  print_strings [ Reset ]
    (*begin by displaying all current goals, then prompt with menu?*)
    [
      "1. Add a new goal\n";
      "2. Log progress\n";
      "3. Mark goal as complete\n";
      "4. Remove a goal\n";
      "5. View in-progress goals\n";
      "6. View complete goals\n";
      "7. View progress logs\n";
      "8. Exit\n";
    ];
  print_string [ Bold ] "Please choose an option (1-7): ";
  goals_menu_choice user

and goals_menu_choice user =
  let choice = read_line () in
  match choice with
  | "1" ->
      Goals.add_new_goal user;
      goals_interface user
  | "2" ->
      Goals.log_progress user;
      goals_interface user
  | "3" ->
      Goals.complete_goal user;
      goals_interface user
  | "4" ->
      Goals.remove_goal user;
      goals_interface user
  | "5" ->
      Goals.view_incomplete_goals user;
      goals_interface user
  | "6" ->
      Goals.view_complete_goals user;
      goals_interface user
  | "7" ->
      Goals.view_progress_log user;
      goals_interface user
  | "8" -> dashboard_login user
  | _ ->
      print_string [] "Invalid option. Please try again.\n";
      goals_interface user
