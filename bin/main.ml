(* main.ml *)
open Final_project.Auth

let print_menu () =
  print_endline "Welcome to the Daily Life Planner (DLP)";
  print_endline "1. Login";
  print_endline "2. Register";
  print_endline "3. Exit";
  print_string "Please choose a number: "

let rec process_choice () =
  let choice = read_line () in
  match choice with
  | "1" -> login ()
  | "2" -> register ()
  | "3" -> exit_program ()
  | _ -> invalid_option ()

and exit_program () =
  print_endline "Exiting...";
  exit 0

and invalid_option () =
  print_endline "Invalid option. Please try again.";
  main_menu ()

and login () =
  let username, password = request_credentials "Login" in
  if Final_project.Auth.username_exists username = false then
    username_doesnt_exist ()
  else if authenticate username password then login_success username
  else login_failure ()

and register () =
  let username, password = request_credentials "Register" in
  let hashed_password = hash_password password in
  if add_user username hashed_password then registration_success ()
  else username_exists ()

and request_credentials prompt =
  print_endline prompt;
  print_string "Enter a username: ";
  let username = read_line () in
  print_string "Enter a password: ";
  let password = read_line () in
  (username, password)

and login_success user =
  print_endline "Login successful!";
  Final_project.Overview.dashboard_login user

and login_failure () =
  print_endline "Invalid username or password.";
  main_menu ()

and registration_success () =
  print_endline "Registration successful!";
  login ()

and username_exists () =
  print_endline "Username already exists.";
  main_menu ()

and username_doesnt_exist () =
  print_endline "This user does not exist. Please try again.";
  login ()

and main_menu () =
  print_menu ();
  process_choice ()

let () = main_menu ()
