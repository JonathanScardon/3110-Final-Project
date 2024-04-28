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
  print_endline ""
(* check if wallet balance < amount *)
(* subtract amount from both credit and wallet *)

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
  let data = load_financial_data user in
  let total = calculate_total_balance user in
  let updated_data =
    List.map
      (fun row ->
        if List.nth row 0 = "TB" then [ "TB"; string_of_float total ] else row)
      data
  in
  save_financial_data user updated_data
