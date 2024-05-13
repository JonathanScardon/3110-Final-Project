(* TODO: add specs *)

val add_health_data : string -> string -> unit
(** [add_health data user journal] prompts the user for input depending on 
    which [journal] is provided, and adds user's health data to 
    their journal. *)

val search_entry : string -> unit
(** [search_entry path] prompts the user to enter a journal to search and
    a date in the format day-month-year (e.g., 2-3-2024) to search for entries 
    in the CSV file at [path]. If 'back' is entered, the search is 
    canceled. If the entry does not exist, it informs the user. *)

val see_history : string -> unit
(** [see_history path] allows users to view the data in the journal stored at [path]*)

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
