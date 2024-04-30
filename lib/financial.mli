val prompt_add_account : string -> unit
val prompt_edit_account : string -> unit

val add_account : string -> string -> float -> unit
(** [add_account user name balance] adds the account [name] with 
    balance [balance] to the user's bank. *)

val edit_account_balance : string -> string -> string -> unit
val view_bank_accounts : string -> unit

val add_credit_card : string -> unit
(** [add_credit_card user name limit] *)
