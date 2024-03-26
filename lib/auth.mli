(* auth.mli *)

val credentials_path : unit -> string
(** Returns the path to the CSV file storing user credentials. *)

val username_exists : string -> bool
(** [username_exists username] checks if the given [username] exists in the 
    credentials store.
    @param username The username to check.
    @return [true] if the username exists, [false] otherwise. *)

val hash_password : string -> string
(** [hash_password password] hashes the given [password] using SHA-256.
    @param password The password to hash.
    @return The hashed password as a string. *)

val add_user : string -> string -> bool
(** [add_user username hashed_password] attempts to add a new user with the 
    given [username] and [hashed_password].
    It checks if the username already exists, and if not, adds the new user to 
      the credentials store.
    @param username The username of the new user.
    @param hashed_password The hashed password of the new user.
    @return [true] if the user was successfully added, [false] if the username 
      already exists. *)

val authenticate : string -> string -> bool
(** [authenticate username password] checks if the given [username] and 
    [password] match a user in the credentials store.
    The password is hashed before comparison.
    @param username The username to authenticate.
    @param password The password to authenticate.
    @return [true] if the authentication is successful, [false] otherwise. *)
