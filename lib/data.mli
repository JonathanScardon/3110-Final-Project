val add_data : string list -> string -> unit
(** [add_data data path] adds [data] to the CSV file [path]. *)

val get_data : string -> int option -> string
(** [get_data path limit] is a string representation of the data in [path], 
    limited to [limit] fields. If [limit] is None, all of the data is displayed. *)

val remove_data : string -> string -> unit
(** [remove_data path data] removes the row containing [data] from the CSV at 
    the given [path]. Raises [Not_found] if there is no such entry. *)

val search : string -> string -> bool
(** [search id path] returns if the row with [id] as a first element is 
    contained within the CSV [path]. *)

val find_entry : string -> string -> string
(** [find_entry id path] returns the first entry with [id] as a first element, 
    contained within the CSV [path]. *)

val edit : string -> string -> string -> unit
(** [edit id path data] edits the row with [id] as a first element to contain 
    [data], contained within the CSV [path]. If the row does not exist, then it
     is appended to the end. *)

val take : int -> 'a list -> 'a list
(** [take n lst] returns the first [n] elements of [lst]. If [n] is greater 
    than the length of [lst], it returns [lst]. If [n] is less than or equal to 
    zero, it returns an empty list. *)

val search_entry : string -> string -> unit
(** [search_entry header path] prompts the user to enter a date in the format 
    day-month-year (e.g., 2-3-2024) to search for entries in the CSV file at 
    [path] under the specified [header]. If 'back' is entered, the search is 
    canceled. If the entry does not exist, it informs the user. *)

val see_history : string -> string -> unit
(** [see_history header path] displays the history of entries under the 
    specified [header] in the CSV file at [path]. The user can specify whether
    to limit the number of entries displayed and, if so, to how many.
    If 'back' is entered, the operation is canceled. *)

val remove_entry : string -> unit
(** [remove_entry path] removes an entry corresponding to a date in the 
    format day-month-year (e.g., 2-3-2024) from the CSV at the given [path]. 
    If the date is not found, it informs the user. If 'back' is entered, the 
    operation is canceled. *)

val data_to_list : string -> string list
(** [data_to_list path] converts all CSV data from [path] into a list. *)
