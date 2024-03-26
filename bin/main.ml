(* main.ml *)
open Final_project.Auth

let rec main_menu () =
  print_endline "Welcome to the Daily Life Planner (DLP)";
  print_endline "1. Login";
  print_endline "2. Register";
  print_endline "3. Exit";
  print_string "Please choose an option: ";
  match read_line () with
  | "1" -> login ()
  | "2" -> register ()
  | "3" ->
      print_endline "Exiting...";
      exit 0
  | _ ->
      print_endline "Invalid option. Please try again.";
      main_menu ()

and login () =
  print_endline "Login";
  print_string "Username: ";
  let username = read_line () in
  print_string "Password: ";
  let password = read_line () in
  if authenticate username password then (
    print_endline "Login successful!";
    (* Placeholder for redirecting to the dashboard *) ())
  else (
    print_endline "Invalid username or password.";
    main_menu ())

and register () =
  print_endline "Register";
  print_string "Choose a username: ";
  let username = read_line () in
  print_string "Choose a password: ";
  let password = read_line () in
  let hashed_password = hash_password password in
  if add_user username hashed_password then (
    print_endline "Registration successful!";
    login ())
  else (
    print_endline "Username already exists.";
    main_menu ())

let () = main_menu ()
