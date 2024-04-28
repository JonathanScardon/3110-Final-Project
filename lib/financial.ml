let load_financials user_id =
  let path = "data/" ^ user_id ^ "_financials.csv" in
  Csv.load path

let save_financials user_id data =
  let path = "data/" ^ user_id ^ "_financials.csv" in
  Csv.save path data

let modify_wallet user_id wallet_name amount =
  let wallets = load_financials user_id in
  let exists, others =
    List.partition (fun row -> List.hd row = wallet_name) wallets
  in
  let new_wallets =
    match exists with
    | [] -> [ wallet_name; string_of_float amount ] :: wallets
    | [ existing ] ->
        [
          wallet_name;
          string_of_float (float_of_string (List.nth existing 1) +. amount);
        ]
        :: others
    | _ -> wallets
  in
  save_financials user_id new_wallets

type balance_operation = Add | Subtract | Set

let adjust_wallet_balance user_id wallet_name operation amount =
  let wallets = load_financials user_id in
  let exists, others =
    List.partition (fun row -> List.hd row = wallet_name) wallets
  in
  let new_wallets =
    match exists with
    | [ existing ] ->
        let current_balance = float_of_string (List.nth existing 1) in
        let new_balance =
          match operation with
          | Add -> current_balance +. amount
          | Subtract -> current_balance -. amount
          | Set -> amount
        in
        [ wallet_name; string_of_float new_balance ] :: others
    | [] ->
        if operation = Set then
          [ wallet_name; string_of_float amount ] :: wallets
        else failwith "Wallet does not exist, cannot add or subtract."
    | _ -> wallets
  in
  save_financials user_id new_wallets

let view_wallet_spread user_id =
  let wallets = load_financials user_id in
  let total =
    List.fold_left
      (fun acc row ->
        match row with _ :: amt :: _ -> acc +. float_of_string amt | _ -> acc)
      0.0 wallets
  in

  List.iter
    (fun row ->
      match row with
      | _ :: amt :: _ ->
          let name = List.hd row in
          let balance = float_of_string amt in
          Printf.printf "%s holds $%.2f, which is %.2f%% of total funds.\n" name
            balance
            (100. *. balance /. total)
      | _ -> ())
    wallets

let add_wallet user_id wallet_name initial_balance =
  let wallets = load_financials user_id in
  if List.exists (fun row -> List.hd row = wallet_name) wallets then
    print_endline "Wallet already exists.\n"
  else
    let new_wallets =
      [ wallet_name; string_of_float initial_balance ] :: wallets
    in
    save_financials user_id new_wallets;
    print_endline "Wallet added successfully.\n"

let remove_wallet user_id wallet_name =
  let wallets = load_financials user_id in
  let exists, others =
    List.partition (fun row -> List.hd row = wallet_name) wallets
  in
  match exists with
  | [] -> print_endline "Wallet does not exist.\n"
  | _ ->
      save_financials user_id others;
      print_endline "Wallet removed successfully.\n"

let log_transaction user_id wallet_name operation amount =
  let path = "data/" ^ user_id ^ "_transactions.csv" in
  let transactions = Csv.load path in
  let new_entry =
    [
      wallet_name;
      operation;
      string_of_float amount;
      string_of_float (Unix.time ());
    ]
  in
  Csv.save path (new_entry :: transactions);
  print_endline "Transaction logged successfully.\n"
