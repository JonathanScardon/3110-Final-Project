val print_strings : ANSITerminal.style list -> string list -> unit
(** Prints a list of strings each styled according to the given style. *)

val get_valid_int : string -> string -> int option
(** Prompts the user with a string and returns a valid integer or None if the 
    user chooses to go back. *)

val get_valid_float : string -> string -> float option
(** Prompts the user with a string and returns a valid float or None if the 
    user chooses to go back. *)

val get_valid_string : string -> string -> string option
(** Prompts the user with a string and returns a non-empty string or None if 
    the user chooses to go back. *)

val mood_interface : string -> unit
(** Mood tracking interface that handles user interactions related to mood 
    tracking. *)

val health_interface : string -> unit
(** Health tracking interface that handles user interactions related to 
    health tracking. *)

val mealplan_interface : string -> unit
(** Meal planning interface that handles user interactions related to 
    meal planning. *)

val financial_interface : string -> unit
(** Financial tracking interface that handles user interactions related to 
    financial tracking. *)

val goals_interface : string -> unit
(** Goals tracking interface that handles user interactions related to goal
     management. *)

val dashboard_login : string -> unit
(** Entry point for the dashboard after login, presenting the main menu to 
    the user. *)
