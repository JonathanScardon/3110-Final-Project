(* TODO: add specs *)

val add_health_data : string -> string -> unit
val search_entry : string -> unit
val see_history : string -> unit
val select_journal : string -> (string -> unit) -> string -> unit
val remove_entry : string -> unit

val mealplan : string -> int -> unit
(** Generates a meal plan of breakfast, lunch, and dinner for [n] days. 
    Requires: [n] > 0.  *)

val add_meal : string -> unit
(** Adds a meal to the user's breakfast, lunch, or dinner CSV. *)

val remove_meal : string -> unit
(** Removes a meal from the user's breakfast, lunch, or dinner CSV. *)

val view_meal : string -> unit
(** Prints meals from the user's breakfast, lunch, or dinner CSV. *)
