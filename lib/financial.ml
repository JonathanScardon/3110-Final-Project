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

let add_account user name balance =
  let data = load_financial_data user in
  let new_data = [ "account"; name; string_of_float balance ] :: data in
  save_financial_data user new_data

let rec prompt_add_account user =
  print_string [ Reset ] "Enter account name: ";
  let account_name = read_line () in
  let () = print_string [ Reset ] "Enter initial balance: " in
  try
    let balance = float_of_string (read_line ()) in
    add_account user account_name balance
  with _ ->
    print_string [ Foreground Red ] "\nPlease enter a number!\n";
    prompt_add_account user

let edit_account_balance user name operation amount =
  let data = load_financial_data user in
  try
    let modified_data =
      List.map
        (fun row ->
          if List.nth row 1 = name then
            match operation with
            | "add" ->
                [
                  "account";
                  name;
                  string_of_float (float_of_string (List.nth row 2) +. amount);
                ]
            | "subtract" ->
                [
                  "account";
                  name;
                  string_of_float (float_of_string (List.nth row 2) -. amount);
                ]
            | "set" -> [ "account"; name; string_of_float amount ]
            | _ -> row
          else row)
        data
    in
    save_financial_data user modified_data
  with _ ->
    print_string [ Foreground Red ]
      "\nYou do not have a account yet. Please add one to edit it.\n"

(* credit cards *)
let add_credit_card user name limit =
  let data = load_financial_data user in
  let new_data =
    [ "credit_card"; name; string_of_float limit; "0.0" ] :: data
  in
  save_financial_data user new_data

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
