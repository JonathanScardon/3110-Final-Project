val display_stocks : string -> unit
val update_stock_prices : string -> unit
val calculate_portfolio_value : string -> float
val add_stock : string -> string -> int -> float -> unit
val remove_stock : string -> string -> unit
val modify_stock : string -> string -> int -> float -> float -> unit
val view_stock_spread : string -> unit
val fetch_stock_data_sync : string -> Yojson.Basic.t option
val update_and_calculate_changes : string -> unit
