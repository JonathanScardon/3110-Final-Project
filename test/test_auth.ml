open OUnit2
open Final_project.Auth

(* Helper function to clear the test CSV file before each test *)
(* Cite ChatGPT for assistance in file clearing with Unix *)
let clear_test_csv () =
  let test_dir = "test" in
  if not (Sys.file_exists test_dir) then Unix.mkdir test_dir 0o755;
  let oc = open_out (test_dir ^ "/test_auth.csv") in
  output_string oc "";
  close_out oc

let test_add_user _ =
  clear_test_csv ();
  assert_bool "add_user should return true for a new username"
    (add_user "testuser" (hash_password "testpass"))

let test_user_exists _ =
  clear_test_csv ();
  let _ = add_user "existinguser" (hash_password "password") in
  assert_bool "username_exists should return true for existing user"
    (username_exists "existinguser")

let test_authenticate _ =
  clear_test_csv ();
  let _ = add_user "authuser" (hash_password "authpass") in
  assert_bool "authenticate should return true for correct credentials"
    (authenticate "authuser" "authpass")

let suite =
  "Auth Tests"
  >::: [
         "test_add_user" >:: test_add_user;
         "test_user_exists" >:: test_user_exists;
         "test_authenticate" >:: test_authenticate;
       ]

let () = run_test_tt_main suite
