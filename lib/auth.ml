(* auth.ml *)
open Cryptokit
(* Cite ChatGPT for cryptokit password hashing *)

let credentials_path () =
  match Sys.getenv_opt "CREDENTIALS_PATH" with
  | Some path -> path
  | None -> (
      match Sys.getenv_opt "ENV" with
      | Some "test" -> "test/test_auth.csv"
      | _ -> "data/user_credentials.csv")

let username_exists username =
  let credentials = Csv.load (credentials_path ()) in
  List.exists (fun row -> List.nth row 0 = username) credentials

let hash_password password = hash_string (Hash.sha256 ()) password

let add_user username hashed_password =
  if username_exists username then false
  else
    let credentials = Csv.load (credentials_path ()) in
    Csv.save (credentials_path ())
      (credentials @ [ [ username; hashed_password ] ]);
    true

let authenticate username password =
  let credentials = Csv.load (credentials_path ()) in
  List.exists
    (fun row ->
      let stored_username = List.nth row 0 in
      let stored_hashed_password = List.nth row 1 in
      stored_username = username
      && stored_hashed_password = hash_password password)
    credentials
