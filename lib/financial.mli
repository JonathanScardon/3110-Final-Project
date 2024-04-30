val prompt_add_account : string -> unit

val add_account : string -> string -> float -> unit
(** [add_account user name balance] adds the account [name] with 
    balance [balance] to the user's bank. *)

val edit_account_balance : string -> string -> string -> float -> unit
