open Data
open ANSITerminal
open Cohttp
open Cohttp_lwt_unix
open Yojson.Basic.Util
open Lwt
open Lwt.Syntax
open Csv

(* stock tracker *)
let api_key = Sys.getenv "64ROAIJNDDZD98UE"
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
  let data = Csv.load path in
  Lwt.return (take 3 data)

let save_user_stock_financials user_id data =
  let path = "data/" ^ user_id ^ "_stock_financials.csv" in
  Lwt.return (Csv.save path (take 3 data))

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
  let%lwt resp, body = Client.get uri in
  match Cohttp.Response.status resp with
  | `OK -> (
      let%lwt body = Cohttp_lwt.Body.to_string body in
      try
        let json = Yojson.Basic.from_string body in
        Lwt.return (Some json)
      with Yojson.Json_error msg ->
        Lwt_io.printf "JSON parsing error: %s\n" msg >>= fun () ->
        Lwt.return None)
  | status ->
      let status_code = Cohttp.Code.string_of_status status in
      Lwt_io.printf "Failed to fetch data: HTTP Status %s\n" status_code
      >>= fun () -> Lwt.return None

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

let update_and_calculate_changes user_id =
  let%lwt stocks = load_user_stock_financials user_id in
  let%lwt updated_stocks =
    Lwt_list.mapi_s
      (fun _ [ symbol; shares; purchase_price; last_price ] ->
        match%lwt fetch_stock_data symbol with
        | Some json ->
            let new_price =
              Yojson.Basic.Util.(
                json
                |> member "Time Series (5min)"
                |> to_assoc |> List.hd |> snd |> member "4. close" |> to_float)
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
      stocks
  in
  save_user_stock_financials user_id updated_stocks

(* other financial tracker stuff *)

let load_financials user_id =
  let path = "data/" ^ user_id ^ "_financicoals.csv" in
  let data = Csv.load path in
  Lwt.return data

let save_financials user_id data =
  let path = "data/" ^ user_id ^ "_financials.csv" in
  Csv.save path data |> Lwt.return

let modify_wallet user_id wallet_name amount =
  let%lwt wallets = load_financials user_id in
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
  let%lwt wallets = load_financials user_id in
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
  let%lwt wallets = load_financials user_id in
  let total =
    List.fold_left
      (fun acc row -> acc +. float_of_string (List.nth row 1))
      0.0 wallets
  in
  Lwt_list.iter_s
    (fun row ->
      let name = List.hd row in
      let balance = float_of_string (List.nth row 1) in
      Lwt_io.printf "%s holds $%.2f, which is %.2f%% of total funds.\n" name
        balance
        (100. *. balance /. total))
    wallets
