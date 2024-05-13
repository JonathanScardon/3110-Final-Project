exception CreditLimitReached

val load_financial_data : string -> Csv.t 
(** [load_financial_data user] loads the [user]'s financial data from their CSV 
    file as a string list list *)
val view_financial : string -> string -> unit
(** [view_financial user aspect] allows the [user] to view an [aspect] of their 
    financial data. The [aspect] is either their credit cards or bank accounts. *)

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
(** [view_transactions user] allows [user] to view their transaction history. *)

val make_transaction : string -> unit
(** [make_transaction user] allows [user] to make a transaction on one of their 
    credit cards*)

val prompt_pay_credit : string -> unit
(** [prompt_pay_credit user] allows [user] to make a transfer to pay their credit
    card from a bank account. It prompts them for the credit card they want to pay, 
    as well as the bank account they want to pay from. *)

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

val remove_financial : 'a list list -> 'a -> 'a -> 'a list list
(** [remove_financial lst aspect name] removes the account or credit card,
    specified by [aspect], with name [name] from [lst]. *)

val string_financial : string list list -> string -> string
(** [string_financial sheet aspect] is a string representation of the 
    [aspect] data (accounts or credit cards) in [sheet]. *)
