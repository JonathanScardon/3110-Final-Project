val add_data : string list -> string -> unit
(** [add_data data path] adds [data] to the CSV file [path]. *)

val get_data : string -> int option -> string
(** [get_data path limit] is a string representation of the data in [path], limited to [limit] fields. 
    If [limit] is None, all of the data is displayed. *)

val remove_data : string -> string -> unit
(** [remove_data path data] removes the row containing [data] from the CSV at the given [path] *)

val search : string -> string -> bool
(** [search id path] returns if the row with [id] as a first element is 
    contained within the CSV [path]. *)
