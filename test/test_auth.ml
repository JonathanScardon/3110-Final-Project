open OUnit2
open Final_project.Auth

let test_password_not_unhashed _ =
  assert_bool "Hashed password should not match the plaintext"
    ("test" <> hash_password "test")

let test_hash_password _ =
  assert_equal
    "9f86d081884c7d659a2feaa0c55ad015a3bf4f1b2b0b822cd15d6c15b0f00a08"
    (hash_password "test")
    ~printer:(fun x -> x)
    ~msg:"Should return the correct SHA-256 hash of the password"

let test_authenticate_success _ =
  assert_equal true
    (authenticate "test" "test" 1)
    ~msg:"Should authenticate successfully with correct username and password"

let test_authenticate_failure _ =
  assert_equal false
    (authenticate "wronguser" "test" 1)
    ~msg:"Should fail authentication with incorrect username"

let test_password_case_sensitivity _ =
  assert_equal false
    (authenticate "test" "TEST" 1)
    ~msg:"Should fail authentication with incorrect password case"

let test_non_existent_user _ =
  assert_equal false
    (authenticate "nonexistentuser" "password" 1)
    ~msg:"Should fail authentication for a non-existent user"

let test_empty_username_and_password _ =
  assert_equal false (authenticate "" "" 1)
    ~msg:"Should fail authentication with empty username and password"

let test_empty_username_and_password _ =
  assert_equal false (authenticate "" "" 1)
    ~msg:"Should fail authentication with empty username and password"

let test_valid_username_incorrect_password _ =
  assert_equal false
    (authenticate "test" "wrongpassword" 1)
    ~msg:
      "Should fail authentication with correct username and incorrect password"

let test_special_characters _ =
  assert_equal true
    (authenticate "user@name" "!password123!" 1)
    ~msg:
      "Should authenticate successfully with special characters in username \
       and password"

let suite =
  "auth_tests"
  >::: [
         "password_not_unhashed" >:: test_password_not_unhashed;
         "hash_password" >:: test_hash_password;
         "authenticate_success" >:: test_authenticate_success;
         "authenticate_failure" >:: test_authenticate_failure;
         "password_case_sensitivity" >:: test_password_case_sensitivity;
         "non_existent_user" >:: test_non_existent_user;
         "empty_username_and_password" >:: test_empty_username_and_password;
         "valid_username_incorrect_password"
         >:: test_valid_username_incorrect_password;
         "special_characters" >:: test_special_characters;
       ]

(* Cite ChatGPT for appropriate navigation to correct root bc otherwise
   it wouldn't work right weirdly. *)
let rec find_and_set_directory target_dir =
  let current_dir = Sys.getcwd () in
  if Filename.basename current_dir = target_dir then current_dir
  else if current_dir = "/" then
    failwith "Reached the root directory, target directory not found."
  else (
    Sys.chdir "../";
    find_and_set_directory target_dir)

(* WE REALLY NEED TO MAKE SURE WE CHANGE THE DIRECTORY TO THE RIGHT NAME
   WHEN WE SUBMIT *)
let () =
  let target_directory = "3110-final-project" in
  try
    let found_dir = find_and_set_directory target_directory in
    Sys.chdir found_dir;
    Printf.printf "Changed to directory: %s\n" (Sys.getcwd ());
    run_test_tt_main suite
  with
  | Failure msg -> Printf.printf "Error: %s\n" msg
  | Sys_error msg -> Printf.printf "System error: %s\n" msg

let _ = run_test_tt_main suite
let () = print_endline "authentication tests succeeded"
