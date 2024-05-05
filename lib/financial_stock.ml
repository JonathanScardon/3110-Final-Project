open Yojson.Basic.Util

(* stock tracker *)
let api_key = "64ROAIJNDDZD98UE"
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
  take 3 data

let save_user_stock_financials user_id data =
  let path = "data/" ^ user_id ^ "_stock_financials.csv" in
  Csv.save path (take 3 data)

let fetch_stock_data symbol =
  let url =
    Printf.sprintf "%s?function=GLOBAL_QUOTE&symbol=%s&apikey=%s" base_url
      symbol api_key
  in
  let response = ref "" in
  let c = Curl.init () in
  Curl.set_url c url;
  Curl.set_writefunction c (fun x ->
      response := !response ^ x;
      String.length x);
  Curl.perform c;
  Curl.cleanup c;
  match Yojson.Basic.from_string !response with
  | json -> (
      try
        let quote = json |> member "Global Quote" in
        if Yojson.Basic.Util.to_option (fun x -> x) quote = None then None
        else
          let price = quote |> member "05. price" |> to_string in
          Some price (* Ensure the price is a string here *)
      with _ ->
        None (* Handle cases where data might be missing or not a string *))
  | exception Yojson.Json_error _ -> None

let update_stock_prices user_id =
  let stocks = load_user_stock_financials user_id in
  let updated_stocks =
    List.mapi
      (fun _ row ->
        match row with
        | symbol :: _ as stock -> (
            match fetch_stock_data symbol with
            | Some price ->
                let new_stock_data = List.tl stock in
                (* Remove the current price *)
                List.append new_stock_data [ price ]
                (* Add the new price *)
            | None ->
                stock (* Preserve the original data if no update is available *)
            )
        | _ -> row) (* Handle malformed rows *)
      stocks
  in
  save_user_stock_financials user_id updated_stocks

let calculate_portfolio_value user_id =
  let stocks = load_user_stock_financials user_id in
  let total_value =
    List.fold_left
      (fun acc row ->
        match row with
        | _ :: shares :: price :: _ ->
            acc +. (float_of_string shares *. float_of_string price)
        | _ -> acc)
      0.0 stocks
  in
  total_value

let add_stock user_id symbol shares purchase_price =
  let stocks = load_user_stock_financials user_id in
  let new_stock =
    [ symbol; string_of_int shares; string_of_float purchase_price; "0" ]
  in
  save_user_stock_financials user_id (new_stock :: stocks)

let remove_stock user_id symbol =
  let stocks = load_user_stock_financials user_id in
  let filtered_stocks = List.filter (fun row -> List.hd row <> symbol) stocks in
  save_user_stock_financials user_id filtered_stocks

let modify_stock user_id index symbol shares purchase_price last_price =
  let index = index - 1 in
  let stocks = load_user_stock_financials user_id in
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
  let stocks = load_user_stock_financials user in
  List.iteri
    (fun index stock ->
      Printf.printf
        "Index: %i, Stock: %s, Shares: %s, Purchase Price: $%s, Current Price: \
         $%s\n"
        (index + 1) (List.nth stock 0) (List.nth stock 1) (List.nth stock 2)
        (List.nth stock 3))
    stocks

let fetch_stock_data_sync symbol =
  let url =
    Printf.sprintf
      "%s?function=TIME_SERIES_INTRADAY&symbol=%s&interval=5min&apikey=%s"
      base_url symbol api_key
  in
  let c = Curl.init () and response = ref "" in
  Curl.set_url c url;
  Curl.set_writefunction c (fun data ->
      response := !response ^ data;
      String.length data);
  Curl.perform c;
  Curl.cleanup c;
  match Yojson.Basic.from_string !response with
  | json -> Some json
  | exception Yojson.Json_error _ -> None

let update_and_calculate_changes user_id =
  let stocks = load_user_stock_financials user_id in
  let updated_stocks =
    List.mapi
      (fun _ stock ->
        match stock with
        | [ symbol; shares; purchase_price; last_price ] -> (
            match fetch_stock_data_sync symbol with
            | Some data ->
                let new_price : float =
                  data
                  |> member "Time Series (5min)"
                  |> to_assoc |> List.hd |> snd |> member "4. close" |> to_float
                in

                let last_price_float : float = float_of_string last_price in

                let change : float =
                  (new_price -. last_price_float) /. last_price_float *. 100.0
                in

                Printf.printf "Stock: %s, Change: %.2f%%\n" symbol change;
                [ symbol; shares; purchase_price; string_of_float new_price ]
            | None -> [ symbol; shares; purchase_price; last_price ])
        | _ -> stock)
      stocks
  in
  save_user_stock_financials user_id updated_stocks
