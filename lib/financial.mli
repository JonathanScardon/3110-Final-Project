val view_financial : string -> string -> unit

val prompt_add_account : string -> unit
(** [prompt_add_account user] adds a new account with a user-specified 
    name and balance to the user's bank. *)

val prompt_edit_account : string -> unit
(** [prompt_edit_account user] edits the balance of one of the 
    user's bank accounts. *)

val add_credit_card : string -> unit
(** [add_credit_card user] adds a new credit card with a user-specified
    name and credit limit. *)

val remove_account : string -> unit
(** [remove_account user] removes a bank account specified by the user. *)

val remove_credit : string -> unit
(** [remove_credit user] removes a credit card specified by the user. *)

val view_transactions : string -> unit

val log_transaction :
  string -> string -> string -> float -> string -> string -> unit
