val add_data : string list -> string -> bool
(** [add_data data path] adds [data] to the CSV file [path]. *)

val get_data : string -> int option -> string
(** [get_data path limit] is a string representation of the data in [path], limited to [limit] fields. 
    If [limit] is None, all of the data is displayed. *)

val remove_data : string -> string -> unit
(** [remove_data path data] removes the row containing [data] from the CSV at the given [path] *)
