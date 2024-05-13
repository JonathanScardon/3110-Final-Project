val string_of_list : string list -> string
(** [string_of_list lst] converts a list of strings [lst] into a single string 
  representation, separated by "|", and ending with a "-". *)

val string_of_list_list : string list list -> string
(** [string_of_list_list lst] converts a list of lists of strings [lst] into a 
  single string representation, with each list converted by [string_of_list] 
  and separated by "; ". *)

val find_and_set_directory : string -> string
(** [find_and_set_directory target_dir] changes the current working directory 
  to [target_dir]. If the directory cannot be found by traversing up to the 
  root, it raises an error. *)

val run_tests : OUnit2.test -> unit
(** [run_tests suite] runs the test suite [suite] after changing the directory 
  to the project's root directory named "3110-final-project". It handles 
  directory changes and catches exceptions related to file or system errors. *)
