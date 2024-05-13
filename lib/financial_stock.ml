open Yojson.Basic.Util

(* stock tracker *)
let api_key = "64ROAIJNDDZD98UE"
let base_url = "https://www.alphavantage.co/query"

let load_user_stock_financials user_id =
  let path = "data/" ^ user_id ^ "_stock_financials.csv" in
  Csv.load path

let save_user_stock_financials user_id data =
  let path = "data/" ^ user_id ^ "_stock_financials.csv" in
  Csv.save path data

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
          Some price
      with _ -> None)
  | exception Yojson.Json_error _ -> None

let update_stock_prices user_id =
  let stocks = load_user_stock_financials user_id in
  let updated_stocks =
    List.mapi
      (fun _ row ->
        match row with
        | symbol :: shares :: purchase_price_str :: _ :: _ -> (
            match fetch_stock_data symbol with
            | Some new_current_price ->
                let purchase_price = float_of_string purchase_price_str in
                let current_price = float_of_string new_current_price in
                let percent_change =
                  if purchase_price = 0.0 then 0.0
                  else
                    (current_price -. purchase_price) /. purchase_price *. 100.0
                in
                let new_percent_change_str =
                  Printf.sprintf "%.2f" percent_change
                in
                [
                  symbol;
                  shares;
                  purchase_price_str;
                  new_current_price;
                  new_percent_change_str;
                ]
            | None -> row)
        | _ -> row)
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
  try
    let stocks = load_user_stock_financials user_id in
    let shares_str = string_of_int shares in
    let purchase_price_str = string_of_float purchase_price in
    let new_stock = [ symbol; shares_str; purchase_price_str; "0.0"; "0.0" ] in
    save_user_stock_financials user_id (stocks @ [ new_stock ])
  with
  | Failure err -> Printf.printf "Error adding stock: %s\n" err
  | ex -> Printf.printf "Unexpected error: %s\n" (Printexc.to_string ex)

let remove_stock user_id symbol =
  let stocks = load_user_stock_financials user_id in
  let filtered_stocks = List.filter (fun row -> List.hd row <> symbol) stocks in
  save_user_stock_financials user_id filtered_stocks

let modify_stock user_id symbol shares purchase_price last_price =
  let stocks = load_user_stock_financials user_id in

  let new_stocks, modified =
    List.map
      (fun row ->
        match row with
        | existing_symbol :: _ when existing_symbol = symbol ->
            let percent_change =
              if purchase_price = 0.0 then 0.0
              else (last_price -. purchase_price) /. purchase_price *. 100.0
            in
            ( [
                symbol;
                string_of_int shares;
                Printf.sprintf "%.2f" purchase_price;
                Printf.sprintf "%.2f" last_price;
                Printf.sprintf "%.2f" percent_change;
              ],
              true )
        | _ -> (row, false))
      stocks
    |> List.split
  in

  if List.exists (fun flag -> flag) modified then (
    save_user_stock_financials user_id new_stocks;
    Printf.printf "Stock modified successfully. Updated data for %s.\n" symbol)
  else
    Printf.printf "Error: Stock symbol '%s' not found. No changes made.\n"
      symbol

let view_stock_spread user =
  let stocks = load_user_stock_financials user in
  if List.length stocks > 0 then (
    Printf.printf "%-10s %-10s %-15s %-15s %-15s\n" "Symbol" "Shares"
      "Purchase Price" "Current Price" "Percent Change";
    List.iteri
      (fun _ stock ->
        match stock with
        | [ symbol; shares; purchase_price; current_price; percent_change ] ->
            Printf.printf "%-10s %-10s $%-14s $%-14s %s%%\n" symbol shares
              purchase_price current_price percent_change
        | _ -> Printf.printf "Skipping malformed row\n")
      (List.tl stocks))
  else Printf.printf "No stocks to display.\n"

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
      (fun _ row ->
        match row with
        | symbol :: shares_str :: purchase_price_str :: _ :: _ -> (
            match fetch_stock_data symbol with
            | Some current_price_str -> (
                let purchase_price = float_of_string_opt purchase_price_str in
                let current_price = float_of_string_opt current_price_str in
                match (purchase_price, current_price) with
                | Some purchase, Some current ->
                    let percent_change =
                      if purchase = 0.0 then "0.00"
                      else
                        let change =
                          (current -. purchase) /. purchase *. 100.0
                        in
                        Printf.sprintf "%.2f" change
                    in
                    [
                      symbol;
                      shares_str;
                      purchase_price_str;
                      current_price_str;
                      percent_change;
                    ]
                | _ ->
                    Printf.printf "Error: Failed to convert prices for %s\n"
                      symbol;
                    row)
            | None ->
                Printf.printf
                  "Failed to fetch new price for %s, keeping last known data\n"
                  symbol;
                row)
        | _ ->
            Printf.printf "Skipping malformed row\n";
            row)
      stocks
  in
  save_user_stock_financials user_id updated_stocks

let display_stocks path =
  let csv_content = Csv.load path in
  match csv_content with
  | [] -> print_endline "No data available."
  | _ :: data_rows ->
      Printf.printf
        "Symbol\tShares\tPurchase Price\tCurrent Price\tPercent Change\n";
      List.iter
        (fun row ->
          match row with
          | [ symbol; shares; purchase_price; current_price; percent_change ] ->
              let color =
                if float_of_string percent_change >= 0.0 then ANSITerminal.green
                else ANSITerminal.red
              in
              ANSITerminal.print_string
                [ ANSITerminal.Bold; color ]
                (Printf.sprintf "%s\t%s\t$%s\t\t$%s\t\t%s%%\n" symbol shares
                   purchase_price current_price percent_change)
          | _ -> ())
        data_rows
