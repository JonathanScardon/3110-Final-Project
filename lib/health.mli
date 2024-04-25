(* TODO: add specs *)

val add_health_data : string -> string -> unit
val search_entry : string -> unit
val see_history : string -> unit
val select_journal : string -> (string -> unit) -> string -> unit
val remove_entry : string -> unit

val mealplan : string -> int -> unit
(** Generates a meal plan of breakfast, lunch, and dinner for [n] days. 
    Requires: [n] > 0.  *)
