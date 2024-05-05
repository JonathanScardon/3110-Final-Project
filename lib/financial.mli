exception CreditLimitReached

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
val make_transaction : string -> unit
val prompt_pay_credit : string -> unit

val modify_financial :
  string -> string -> float -> string list list -> string -> string list list
(** [modify_financial name operation amount data aspect] modifies the nested 
list [data] with the given operation, amount, account/card [name], and 
[aspect] (accounts/credit cards). If [name] does not exist, [data] is unchanged. *)

val modify_credit_data : string -> float -> string list list -> string list list
(** [modify_credit_data card amount data] adds [amount] to the debt of 
[card] in [data]. Factoring in the user's current debt, if the amount
  goes over the credit limit, the exception [CreditLimitReached] is raised.
  If the [card] does not exist, [data] is unchanged. *)
