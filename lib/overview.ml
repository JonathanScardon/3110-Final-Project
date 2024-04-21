(* overview.ml *)
open Data
open ANSITerminal
open Lwt
open Financial

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
      "6. Exit\n";
    ];
  print_string [ Bold ] "Please choose an option (1-6): ";
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
  (* | "3" -> financial_interface user *)
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

(* and financial_interface user =
   print_string [ Reset; Bold; Foreground Green ] "\nFinancial Tracker\n";
   print_strings [ Reset ]
     [
       "1. View personal stock spread\n";
       "2. Manage stock options\n";
       "3. View all bank accounts\n";
       "4. Add bank to overall wallet\n";
       "5. Edit funds in bank wallet\n";
       "6. Return to main menu\n";
     ];
   print_string [ Bold ] "Please enter a command: ";
   financial_input user *)

(* and financial_input user =
   Lwt_io.read_line Lwt_io.stdin >>= fun choice ->
   match choice with
   | "1" -> view_stock_spread user >>= fun () -> financial_interface user
   | "2" -> manage_stock_options user >>= fun () -> financial_interface user
   | "3" -> view_all_banks user >>= fun () -> financial_interface user
   | "4" -> prompt_add_wallet user >>= fun () -> financial_interface user
   | "5" -> prompt_edit_wallet user >>= fun () -> financial_interface user
   | "6" -> dashboard_login user >>= fun () -> Lwt.return_unit
   | _ ->
       Lwt_io.printl "Invalid option. Please try again." >>= fun () ->
       financial_interface user;
       Lwt.return_unit *)

and view_all_banks user = view_wallet_spread user >>= fun () -> Lwt.return ()

and prompt_add_wallet user =
  Lwt_io.printl "Enter wallet name: " >>= fun () ->
  Lwt_io.read_line Lwt_io.stdin >>= fun wallet_name ->
  Lwt_io.printl "Enter initial balance: " >>= fun () ->
  Lwt_io.read_line Lwt_io.stdin >>= fun balance ->
  add_wallet user wallet_name (float_of_string balance) >>= fun () ->
  Lwt.return ()

and prompt_edit_wallet user =
  Lwt_io.printl "Enter wallet name to edit: " >>= fun () ->
  Lwt_io.read_line Lwt_io.stdin >>= fun wallet_name ->
  Lwt_io.printl "Select operation (add/subtract/set): " >>= fun () ->
  Lwt_io.read_line Lwt_io.stdin >>= fun operation ->
  Lwt_io.printl "Enter amount: " >>= fun () ->
  Lwt_io.read_line Lwt_io.stdin >>= fun amount ->
  let op =
    match operation with
    | "add" -> Add
    | "subtract" -> Subtract
    | "set" -> Set
    | _ -> failwith "Invalid operation"
  in
  adjust_wallet_balance user wallet_name op (float_of_string amount)
  >>= fun () -> Lwt.return ()

and manage_stock_options user =
  print_string [ Reset; Bold; Foreground Blue ] "\nStock Management\n";
  print_strings [ Reset ]
    [
      "1. Add a new stock\n";
      "2. Remove a stock\n";
      "3. Modify a stock\n";
      "4. Update stock prices\n";
      "5. Return to financial menu\n";
    ];
  print_string [ Bold ] "Select an option: ";
  stock_input user

and stock_input user =
  let choice = read_line () in
  match choice with
  | "1" -> prompt_add_stock user >>= fun () -> manage_stock_options user
  | "2" -> prompt_remove_stock user >>= fun () -> manage_stock_options user
  | "3" -> prompt_modify_stock user >>= fun () -> manage_stock_options user
  | "4" -> update_stock_prices user >>= fun () -> manage_stock_options user
  (* | "5" -> financial_interface user *)
  | _ ->
      Lwt_io.printl "Invalid choice. Please try again." >>= fun () ->
      manage_stock_options user

and prompt_add_stock user =
  Lwt_io.printl "Enter stock symbol: " >>= fun () ->
  Lwt_io.read_line Lwt_io.stdin >>= fun symbol ->
  Lwt_io.printl "Enter number of shares: " >>= fun () ->
  Lwt_io.read_line Lwt_io.stdin >>= fun shares ->
  Lwt_io.printl "Enter purchase price: " >>= fun () ->
  Lwt_io.read_line Lwt_io.stdin >>= fun price ->
  add_stock user symbol (int_of_string shares) (float_of_string price)

and prompt_remove_stock user =
  Lwt_io.printl "Enter stock symbol to remove: " >>= fun () ->
  Lwt_io.read_line Lwt_io.stdin >>= fun symbol -> remove_stock user symbol

and prompt_modify_stock user =
  Lwt_io.printl "Enter stock index to modify: " >>= fun () ->
  Lwt_io.read_line Lwt_io.stdin >>= fun index ->
  Lwt_io.printl "Enter new stock symbol: " >>= fun () ->
  Lwt_io.read_line Lwt_io.stdin >>= fun symbol ->
  Lwt_io.printl "Enter new number of shares: " >>= fun () ->
  Lwt_io.read_line Lwt_io.stdin >>= fun shares ->
  Lwt_io.printl "Enter new purchase price: " >>= fun () ->
  Lwt_io.read_line Lwt_io.stdin >>= fun price ->
  Lwt_io.printl "Enter last known price: " >>= fun () ->
  Lwt_io.read_line Lwt_io.stdin >>= fun last_price ->
  modify_stock user (int_of_string index) symbol (int_of_string shares)
    (float_of_string price)
    (float_of_string last_price)
