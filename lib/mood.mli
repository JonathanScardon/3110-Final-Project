(* mood.mli *)
(* Interface specification for the mood module *)

val curr_date : string
(** Current date formatted as "day-month-year" *)

val get_user_input : string -> string
(** Prompts the user with a specified message and returns their input. *)

val validate_happiness : string -> int option
(** Validates the user's input for a happiness level. Returns Some integer if the input is a valid happiness level (1-10), otherwise None. *)

val happiness_log : unit -> string
(** Recursively prompts the user to rate their happiness on a scale of 1-10 until a valid input is given. Returns the valid happiness level as a string. *)

val see_history : string -> unit
(** Displays the history of happiness ratings and moods for a given user by reading from a CSV file. *)

val search_entry : string -> unit
(** Searches for a specific entry of happiness and mood for a given user by reading from a CSV file. *)

val remove_entry : string -> unit
(** Removes a mood entry from the specified user's CSV file. *)

val add_quote : string -> unit
(** Allows the user to add a quote. It captures user input and, unless the input is 'back', appends it to the user's quote CSV file. *)

val get_random_quote : string -> string
(** Retrieves a random quote from the specified user's collection of saved quotes. Returns a default message if no quotes are found. *)

val remove_curr_quote : string -> string -> unit
(** Removes a specific quote from the specified user's collection, given the quote text. It updates the CSV file accordingly. *)
