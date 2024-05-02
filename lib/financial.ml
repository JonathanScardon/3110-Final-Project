open Data
open ANSITerminal

(* type account = { name : string; balance : float }
   type credit_card = { name : string; limit : float; balance : float }
   type transaction = { date : string; amount : float; description : string } *)

let user_financial_file user = "data/" ^ user ^ "_financials.csv"

let load_financial_data user =
  let path = user_financial_file user in
  if Sys.file_exists path then Csv.load path else [ [ "tb"; "0.0" ] ]

let save_financial_data user data =
  let path = user_financial_file user in
  Csv.save path data

(* accounts *)

let view_financial user aspect =
  let sheet = Csv.load (user_financial_file user) in
  let header =
    if aspect = "account" then "\nAccount | Balance"
    else "\nName | Limit | Debt"
  in
  print_endline header;
  print_endline
    (List.fold_right
       (fun lst acc ->
         match lst with
         | [] -> ""
         | h :: t when h = aspect ->
             if acc <> "" then String.concat " " t ^ "\n" ^ acc
             else String.concat " " t
         | _ :: _ -> "")
       sheet ""
    ^ "\n")

let add_account user name balance =
  let data = load_financial_data user in
  let new_data = [ "account"; name; string_of_float balance ] :: data in
  save_financial_data user new_data;
  print_string [ Foreground Green ] "\nNew account saved successfully!\n"

let rec prompt_add_account user =
  print_string [ Reset ]
    "\nEnter 'back' to go back to the menu. \nEnter account name: ";
  let account_name = read_line () in
  if account_name = "back" then ()
  else if Data.search2 "account" account_name (user_financial_file user) then (
    print_string [ Foreground Red ] "\nThis account already exists!\n";
    prompt_add_account user)
  else
    let () = print_string [ Reset ] "Enter initial balance: " in
    try
      let input = read_line () in
      if input = "back" then ()
      else
        let balance = float_of_string input in
        add_account user account_name balance
    with _ ->
      print_string [ Foreground Red ] "\nPlease enter a number!\n";
      prompt_add_account user

let rec modify_account name operation amount (data : string list list) =
  match data with
  | [] -> []
  | h :: (t : string list list) -> (
      match h with
      | [] -> modify_account name operation amount t
      | a :: (b : string) :: (c : string) :: _ when a = "account" && b = name
        -> (
          match operation with
          | "add" ->
              [ "account"; name; string_of_float (float_of_string c +. amount) ]
              :: t
          | "subtract" ->
              [ "account"; name; string_of_float (float_of_string c -. amount) ]
              :: t
          | "set" -> [ "account"; name; string_of_float amount ] :: t
          | _ -> modify_account name operation amount t)
      | _ :: _ -> modify_account name operation amount t)

let rec edit_account_balance user name operation =
  let () = print_string [ Reset ] "Enter amount: " in
  let input = read_line () in
  if input = "back" then ()
  else
    let amount = float_of_string_opt input in
    match amount with
    | None ->
        print_string [ Foreground Red ] "\nPlease enter a numerical amount!\n";
        edit_account_balance user name operation
    | Some amount ->
        let data = load_financial_data user in
        let modified_data = modify_account name operation amount data in
        save_financial_data user modified_data;
        print_string [ Foreground Green ]
          "\nAccount balance modified successfully.\n"

let rec prompt_edit_account user =
  let () =
    print_string [ Reset ]
      "\nEnter 'back' to go back to the menu. \nEnter account name to edit: "
  in
  let account_name = read_line () in
  if account_name = "back" then ()
  else if not (Data.search2 "account" account_name (user_financial_file user))
  then (
    print_string [ Foreground Red ] "\nThis account does not exist.\n";
    prompt_edit_account user)
  else
    let () = print_string [ Reset ] "Select operation (add/subtract/set): " in
    let op = read_line () in
    if op = "back" then ()
    else if op <> "add" && op <> "subtract" && op <> "set" then (
      print_string [ Foreground Red ] "\nPlease input add, subtract, or set.\n";
      prompt_edit_account user)
    else edit_account_balance user account_name op

let rec remove_financial lst aspect name =
  match lst with
  | [] -> raise Not_found
  | h :: t -> (
      match h with
      | [] -> remove_financial t aspect name
      | a :: b :: _ when a = aspect && b = name -> t
      | _ -> h :: remove_financial t aspect name)

let rec remove_account user =
  let path = user_financial_file user in
  print_string [ Reset ]
    "\nEnter 'back' to go back to the menu. \nEnter account name: ";
  let account = read_line () in
  if account = "back" then ()
  else (
    if account = "" then (
      print_string [ Foreground Red ] "\nSorry, this account does not exist!\n";
      remove_account user)
    else
      print_string [ Reset ]
        ("\nAre you sure you want to remove account " ^ account ^ "? (y/n) ");
    let confirm = read_line () in
    if confirm <> "y" then ()
    else
      try
        Csv.save path
          (remove_financial (load_financial_data user) "account" account);
        print_string [ Foreground Green ] "\nRemoved account successfully.\n"
      with Not_found ->
        print_string [ Foreground Red ] "Sorry, this account does not exist!\n";
        remove_account user)

(* credit cards *)
let rec add_credit_card user =
  print_string [ Reset ]
    "\nEnter 'back' to go back to the menu.\nEnter credit card name: ";
  let name = read_line () in
  if name = "back" then ()
  else if Data.search2 "credit_card" name (user_financial_file user) then (
    print_string [ Foreground Red ] "\nThis credit card already exists!\n";
    add_credit_card user)
  else
    let () = print_string [ Reset ] "Enter credit limit: " in
    let input = read_line () in
    if input = "back" then ()
    else
      let limit = float_of_string_opt input in
      match limit with
      | None ->
          print_string [ Foreground Red ] "\nPlease enter a numerical limit!\n";
          add_credit_card user
      | Some limit ->
          let data = load_financial_data user in
          let new_data =
            [ "credit_card"; name; string_of_float limit; "0.0" ] :: data
          in
          save_financial_data user new_data;
          print_string [ Foreground Green ]
            "\nCredit card added successfully.\n"

let rec remove_credit user =
  let path = user_financial_file user in
  print_string [ Reset ]
    "\nEnter 'back' to go back to the menu. \nEnter credit card name: ";
  let card = read_line () in
  if card = "back" then ()
  else if card = "" then (
    print_string [ Foreground Red ]
      "\nSorry, this credit card does not exist!\n";
    remove_credit user)
  else (
    print_string [ Reset ]
      ("\nAre you sure you want to remove credit card " ^ card ^ "? (y/n) ");
    let confirm = read_line () in
    if confirm <> "y" then ()
    else
      try
        Csv.save path
          (remove_financial (load_financial_data user) "credit_card" card);
        print_string [ Foreground Green ]
          "\nRemoved credit card successfully.\n"
      with Not_found ->
        print_string [ Foreground Red ]
          "Sorry, this credit card does not exist!\n";
        remove_credit user)

let charge_credit_card user name amount =
  let data = load_financial_data user in
  try
    let modified_data =
      List.map
        (fun row ->
          if List.nth row 1 = name then
            [
              "credit_card";
              name;
              List.nth row 2
              ^ string_of_float (float_of_string (List.nth row 3) +. amount);
            ]
          else row)
        data
    in
    save_financial_data user modified_data
  with _ ->
    print_string [ Foreground Red ] "\nYou do not have a credit card yet.\n"

(* Cross functionality *)

let pay_credit_card_balance user credit_name account_name amount =
  let data = load_financial_data user in
  let credit_card, others =
    List.partition
      (fun row ->
        List.nth row 1 = credit_name && List.nth row 0 = "credit_card")
      data
  in
  let account, others =
    List.partition
      (fun row -> List.nth row 1 = account_name && List.nth row 0 = "account")
      others
  in
  match (credit_card, account) with
  | ( [ [ "credit_card"; _; limit; balance ] ],
      [ [ "account"; _; account_balance ] ] ) ->
      let balance = float_of_string balance in
      let account_balance = float_of_string account_balance in
      let pay_amount = min balance (min amount account_balance) in
      if pay_amount > 0.0 then (
        let new_credit_balance = balance -. pay_amount in
        let new_account_balance = account_balance -. pay_amount in
        let new_data =
          [
            "credit_card";
            credit_name;
            limit;
            string_of_float new_credit_balance;
          ]
          :: [ "account"; account_name; string_of_float new_account_balance ]
          :: others
        in
        save_financial_data user new_data;
        print_string [ Foreground Green ] "\nPayment successful.\n")
      else
        print_string [ Foreground Red ]
          "\nInvalid payment amount or insufficient funds.\n"
  | _ -> print_string [ Foreground Red ] "\nCredit card or account not found.\n"

(* transactions *)

let user_transaction_log_file user = "data/" ^ user ^ "_transaction_log.csv"

let log_transaction user t_type date amount description entity =
  let path = user_transaction_log_file user in
  let data = Csv.load path in
  let new_entry =
    [ t_type; date; string_of_float amount; description; entity ]
  in
  let updated_data = new_entry :: data in
  Csv.save path updated_data

let load_transaction_log user =
  let path = user_transaction_log_file user in
  if Sys.file_exists path then Csv.load path else []

let view_transaction_history user limit =
  let transactions = load_transaction_log user in
  let limited_transactions =
    match limit with Some l -> take l transactions | None -> transactions
  in
  List.iter
    (fun row ->
      Printf.printf "%s, %s, %s, %s, %s\n" (List.nth row 0) (List.nth row 1)
        (List.nth row 2) (List.nth row 3) (List.nth row 4))
    limited_transactions

(* total balance *)

let calculate_total_balance user =
  let data = load_financial_data user in
  List.fold_left
    (fun acc row ->
      if List.nth row 0 = "account" then acc +. float_of_string (List.nth row 2)
      else acc)
    0.0 data

let save_total_balance user =
  let total = calculate_total_balance user in
  let data = load_financial_data user in
  let updated_data =
    List.map
      (fun row ->
        if List.nth row 0 = "TB" then [ "TB"; string_of_float total ] else row)
      data
  in
  save_financial_data user updated_data
