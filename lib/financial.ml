(* open Cohttp_lwt_unix
   open Lwt *)

(* stock tracker *)
(* let api_key = "64ROAIJNDDZD98UE"
   let base_url = "https://www.alphavantage.co/query"

   let take n lst =
     let rec aux n acc lst =
       match lst with
       | [] -> List.rev acc
       | _ when n = 0 -> List.rev acc
       | x :: xs -> aux (n - 1) (x :: acc) xs
     in
     aux n [] lst

   let load_user_stock_financials user_id =
     let path = "data/" ^ user_id ^ "_stock_financials.csv" in
     Lwt_preemptive.detach (fun () -> Csv.load path) () >>= fun data ->
     Lwt.return (take 3 data)

   let save_user_stock_financials user_id data =
     let path = "data/" ^ user_id ^ "_stock_financials.csv" in
     Lwt_preemptive.detach (fun () -> Csv.save path (take 3 data)) () >>= fun () ->
     Lwt.return_unit

   let fetch_stock_data symbol =
     let uri =
       Uri.add_query_params' (Uri.of_string base_url)
         [
           ("function", "TIME_SERIES_INTRADAY");
           ("symbol", symbol);
           ("interval", "5min");
           ("apikey", api_key);
         ]
     in
     Lwt.catch
       (fun () ->
         let%lwt resp, body = Client.get uri in
         match Cohttp.Response.status resp with
         | `OK -> (
             let%lwt body = Cohttp_lwt.Body.to_string body in
             try
               let json = Yojson.Basic.from_string body in
               Lwt.return (Some json)
             with
             | Yojson.Json_error msg ->
                 Lwt_io.printf "JSON parsing error: %s\n" msg >>= fun () ->
                 Lwt.return None
             | _ ->
                 Lwt_io.printf "Unexpected JSON structure.\n" >>= fun () ->
                 Lwt.return None)
         | status ->
             let status_code = Cohttp.Code.string_of_status status in
             Lwt_io.printf "Failed to fetch data: HTTP Status %s\n" status_code
             >>= fun () -> Lwt.return None)
       (fun ex ->
         Lwt_io.printf "Network or unexpected error: %s\n" (Printexc.to_string ex)
         >>= fun () -> Lwt.return None)

   let update_stock_prices user_id =
     let%lwt stocks = load_user_stock_financials user_id in
     let%lwt updated_stocks =
       Lwt_list.mapi_s
         (fun i row ->
           if i < 3 then
             match row with
             | symbol :: _ :: _ :: _ :: _ as stock -> (
                 match%lwt fetch_stock_data symbol with
                 | Some json ->
                     let price =
                       Yojson.Basic.Util.(
                         json
                         |> member "Time Series (5min)"
                         |> to_assoc |> List.hd |> snd |> member "4. close"
                         |> to_string)
                     in
                     Lwt.return (List.append (List.tl stock) [ price ])
                 | None -> Lwt.return stock)
             | _ -> Lwt.return row
           else Lwt.return row)
         stocks
     in
     save_user_stock_financials user_id updated_stocks

   let calculate_portfolio_value user_id =
     let%lwt stocks = load_user_stock_financials user_id in
     let total_value =
       List.fold_left
         (fun acc row ->
           match row with
           | _ :: shares :: price :: _ ->
               acc +. (float_of_string shares *. float_of_string price)
           | _ -> acc)
         0.0 stocks
     in
     Lwt.return total_value

   let add_stock user_id symbol shares purchase_price =
     let%lwt stocks = load_user_stock_financials user_id in
     let new_stock =
       [ symbol; string_of_int shares; string_of_float purchase_price; "0" ]
     in
     save_user_stock_financials user_id (new_stock :: stocks)

   let remove_stock user_id symbol =
     let%lwt stocks = load_user_stock_financials user_id in
     let filtered_stocks = List.filter (fun row -> List.hd row <> symbol) stocks in
     save_user_stock_financials user_id filtered_stocks

   let modify_stock user_id index symbol shares purchase_price last_price =
     let%lwt stocks = load_user_stock_financials user_id in
     let new_stocks =
       List.mapi
         (fun i row ->
           if i = index then
             [
               symbol;
               string_of_int shares;
               string_of_float purchase_price;
               string_of_float last_price;
             ]
           else row)
         stocks
     in
     save_user_stock_financials user_id new_stocks

   let view_stock_spread user =
     let%lwt stocks = load_user_stock_financials user in
     Lwt_list.iter_s
       (fun stock ->
         Lwt_io.printf
           "Stock: %s, Shares: %s, Purchase Price: $%s, Current Price: $%s\n"
           (List.nth stock 0) (List.nth stock 1) (List.nth stock 2)
           (List.nth stock 3))
       stocks

   let update_and_calculate_changes user_id =
     let%lwt stocks = load_user_stock_financials user_id in
     let%lwt updated_stocks =
       Lwt_list.mapi_s
         (fun _ stock ->
           match stock with
           | [ symbol; shares; purchase_price; last_price ] -> (
               match%lwt fetch_stock_data symbol with
               | Some json ->
                   let new_price =
                     Yojson.Basic.Util.(
                       json
                       |> member "Time Series (5min)"
                       |> to_assoc |> List.hd |> snd |> member "4. close"
                       |> to_float)
                   in
                   let change =
                     (new_price -. float_of_string last_price)
                     /. float_of_string last_price *. 100.0
                   in
                   Lwt_io.printf "Stock: %s, Change: %.2f%%\n" symbol change
                   >>= fun () ->
                   Lwt.return
                     [ symbol; shares; purchase_price; string_of_float new_price ]
               | None -> Lwt.return [ symbol; shares; purchase_price; last_price ])
           | _ -> Lwt.return stock)
         stocks
     in
     save_user_stock_financials user_id updated_stocks *)

(* other financial tracker stuff *)

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
      (fun acc row -> acc +. float_of_string (List.nth row 1))
      0.0 wallets
  in
  List.iter
    (fun row ->
      let name = List.hd row in
      let balance = float_of_string (List.nth row 1) in
      Printf.printf "%s holds $%.2f, which is %.2f%% of total funds.\n" name
        balance
        (100. *. balance /. total))
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
