(* main.ml *)
open Final_project.Auth

let print_menu () =
  print_endline "Welcome to the Daily Life Planner (DLP)";
  print_endline "1. Login";
  print_endline "2. Register";
  print_endline "3. Exit";
  print_string "Please choose an option: "

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
  if authenticate username password then login_success () else login_failure ()

and register () =
  let username, password = request_credentials "Register" in
  let hashed_password = hash_password password in
  if add_user username hashed_password then registration_success ()
  else username_exists ()

and request_credentials prompt =
  print_endline prompt;
  print_string "Choose a username: ";
  let username = read_line () in
  print_string "Choose a password: ";
  let password = read_line () in
  (username, password)

and login_success () =
  print_endline "Login successful!";
  (* Placeholder for redirecting to the dashboard *) main_menu ()

and login_failure () =
  print_endline "Invalid username or password.";
  main_menu ()

and registration_success () =
  print_endline "Registration successful!";
  login ()

and username_exists () =
  print_endline "Username already exists.";
  main_menu ()

and main_menu () =
  print_menu ();
  process_choice ()

let () = main_menu ()
