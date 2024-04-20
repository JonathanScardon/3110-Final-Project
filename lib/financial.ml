open Data
open ANSITerminal
open Cohttp
open Cohttp_lwt_unix
open Yojson.Basic.Util
open Lwt
open Lwt.Syntax

let api_key = "64ROAIJNDDZD98UE"
let base_url = "https://www.alphavantage.co/query"

let fetch_stock_data api_key symbol =
  let base_url = "https://www.alphavantage.co/query" in
  let uri =
    Uri.add_query_params' (Uri.of_string base_url)
      [
        ("function", "TIME_SERIES_INTRADAY");
        ("symbol", symbol);
        ("interval", "5min");
        ("apikey", api_key);
      ]
  in
  Client.get uri >>= fun (resp, body) ->
  match Cohttp.Response.status resp with
  | `OK ->
      Cohttp_lwt.Body.to_string body >>= fun body ->
      let json = Yojson.Basic.from_string body in
      Lwt.return (Some json)
  | _ -> Lwt.return None
