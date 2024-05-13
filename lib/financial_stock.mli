val load_user_stock_financials : string -> Csv.t
(** [load_user_stock_financials user_id] loads user [user_id]'s stocks as a 
    Csv.t which is a string list list*)

val save_user_stock_financials : string -> Csv.t -> unit
(** [save_user_stock_financials user_id data] saves [data] to user [user_id]'s 
    stock CSV file*)

val fetch_stock_data : string -> string option
(** [fetch_stock_data symbol] returns the price for the provided stock [symbol]*)

val update_stock_prices : string -> unit

val add_stock : string -> string -> int -> float -> unit
(** [add_stock user_id symbol shares purchase_price] adds [shares] number of 
    shares of [symbol] stock with price of [purchase_price] to user [user_id]'s 
    stock CSV file*)

val remove_stock : string -> string -> unit
(**[remove_stock user_id symbol] removes the stock [symbol] from user [user_id]'s 
    stock CSV file*)

val modify_stock : string -> string -> int -> float -> float -> unit
(** [modify_stock user_id symbol shares purchase_price last_price] updates the 
    given stock [symbol] with with a new number of [shares], and a new price of 
    [last_price]. It uses the [purchase_price] to calculate the percent change as 
    well. It saves these changes to user [user_id]'s stock CSV file. If the stock 
    [symbol] is not already owned by the user, it lets the user know.*)

val view_stock_spread : string -> unit
(** [view_stock_spread user] prints the [user]'s stock information, telling them 
    the symbol, number of shares, the price the stock was purchased at, the 
    current price of the stock, and the percent change from the purchase price to 
    the current price. *)

val fetch_stock_data_sync : string -> Yojson.Basic.t option
(** [fetch_stock_data_sync symbol] fetches real-time stock data synchronously for 
    the given stock [symbol].*)

val update_and_calculate_changes : string -> unit
(** [update_and_calculate_changes user_id] updates user [user_id]'s stocks with 
    current prices, and calculates the percent change for each stock, saving this 
    information to user [user_id]'s stock CSV file*)

val display_stocks : string -> unit
(** [display_stocks path] prints the stock information from the CSV file stored 
    at [path]. Specifically, the symbol, number of shares, the price the stock 
    was purchased at, the current price of the stock, and the percent change 
    (with colors indicating a positive or negative change) from the purchase 
    price to the current price. *)
