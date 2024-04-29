open Data
open ANSITerminal

type wallet = { name : string; balance : float }
type credit_card = { name : string; limit : float; balance : float }
type transaction = { date : string; amount : float; description : string }

let user_financial_file user = "data/" ^ user ^ "_financials.csv"

let load_financial_data user =
  let path = user_financial_file user in
  if Sys.file_exists path then Csv.load path else [ [ "tb"; "0.0" ] ]

let save_financial_data user data =
  let path = user_financial_file user in
  Csv.save path data

(* wallets *)

let add_wallet user name initial_balance =
  let data = load_financial_data user in
  let new_data = [ "wallet"; name; string_of_float initial_balance ] :: data in
  save_financial_data user new_data

let edit_wallet_balance user name operation amount =
  let data = load_financial_data user in
  try
    let modified_data =
      List.map
        (fun row ->
          if List.nth row 1 = name then
            match operation with
            | "add" ->
                [
                  "wallet";
                  name;
                  string_of_float (float_of_string (List.nth row 2) +. amount);
                ]
            | "subtract" ->
                [
                  "wallet";
                  name;
                  string_of_float (float_of_string (List.nth row 2) -. amount);
                ]
            | "set" -> [ "wallet"; name; string_of_float amount ]
            | _ -> row
          else row)
        data
    in
    save_financial_data user modified_data
  with _ ->
    print_string [ Foreground Red ]
      "\nYou do not have a wallet yet. Please add one to edit it.\n"

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

let pay_credit_card_balance user credit_name wallet_name amount =
  let data = load_financial_data user in
  let credit_card, others =
    List.partition
      (fun row ->
        List.nth row 1 = credit_name && List.nth row 0 = "credit_card")
      data
  in
  let wallet, others =
    List.partition
      (fun row -> List.nth row 1 = wallet_name && List.nth row 0 = "wallet")
      others
  in
  match (credit_card, wallet) with
  | ( [ [ "credit_card"; _; limit; balance ] ],
      [ [ "wallet"; _; wallet_balance ] ] ) ->
      let balance = float_of_string balance in
      let wallet_balance = float_of_string wallet_balance in
      let pay_amount = min balance (min amount wallet_balance) in
      if pay_amount > 0.0 then (
        let new_credit_balance = balance -. pay_amount in
        let new_wallet_balance = wallet_balance -. pay_amount in
        let new_data =
          [
            "credit_card";
            credit_name;
            limit;
            string_of_float new_credit_balance;
          ]
          :: [ "wallet"; wallet_name; string_of_float new_wallet_balance ]
          :: others
        in
        save_financial_data user new_data;
        print_string [ Foreground Green ] "\nPayment successful.\n")
      else
        print_string [ Foreground Red ]
          "\nInvalid payment amount or insufficient funds.\n"
  | _ -> print_string [ Foreground Red ] "\nCredit card or wallet not found.\n"

(* transactions *)

let log_transaction user date amount description =
  let data = load_financial_data user in
  let new_data =
    [ "transaction"; date; string_of_float amount; description ] :: data
  in
  save_financial_data user new_data

let calculate_total_balance user =
  let data = load_financial_data user in
  List.fold_left
    (fun acc row ->
      if List.nth row 0 = "wallet" then acc +. float_of_string (List.nth row 2)
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
