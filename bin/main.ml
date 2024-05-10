(* main.ml *)
open Final_project.Auth
open ANSITerminal

let print_menu () =
  print_string [ Reset ] "\n";

  print_string [ Bold ] "\nWelcome to OCamLife\n";
  print_string [ Reset ] "1. Login\n";
  print_string [ Reset ] "2. Register\n";
  print_string [ Reset ] "3. Delete Account\n";
  print_string [ Reset ] "4. Exit\n";
  print_string [ Bold ] "Please choose an option: "

let rec process_choice () =
  try
    let choice = read_line () in
    match choice with
    | "1" -> login ()
    | "2" -> register ()
    | "3" -> delete ()
    | "4" -> exit_program ()
    | _ -> invalid_option ()
  with Exit -> main_menu ()

and exit_program () =
  print_string [ Reset ] "\n";
  print_string [ Foreground Green ] "Exiting... Thank you for using OcamLife!\n";
  exit 0

and invalid_option () =
  print_string [ Reset ] "\n";
  print_string [ Foreground Red ] "Invalid option. Please try again.\n";
  main_menu ()

and login () =
  let username, password = request_credentials "Login" in
  if Final_project.Auth.username_exists username = false then
    username_doesnt_exist ()
  else if authenticate username password 0 then login_success username
  else login_failure ()

and register () =
  let username, password = request_credentials "Register" in
  let hashed_password = hash_password password in
  if add_user username hashed_password then registration_success ()
  else username_exists ()

and delete () =
  let username, password = request_credentials "Login" in
  if Final_project.Auth.username_exists username = false then
    username_doesnt_exist ()
  else if authenticate username password 0 then (
    print_string [ Reset ]
      "Are you sure you want to delete your account? (y/n) ";
    if read_line () = "y" then (
      try
        Final_project.Data.remove_data "data/user_credentials.csv" username;
        Sys.remove ("data/" ^ username ^ "_mood.csv");
        Sys.remove ("data/" ^ username ^ "_quotes.csv");
        Sys.remove ("data/" ^ username ^ "_food.csv");
        Sys.remove ("data/" ^ username ^ "_exercise.csv");
        Sys.remove ("data/" ^ username ^ "_breakfast.csv");
        Sys.remove ("data/" ^ username ^ "_lunch.csv");
        Sys.remove ("data/" ^ username ^ "_dinner.csv");
        Sys.remove ("data/" ^ username ^ "_financials.csv");
        Sys.remove ("data/" ^ username ^ "_stock_financials.csv");
        Sys.remove ("data/" ^ username ^ "_transaction_log.csv");
        print_string [ Foreground Green ] "\nAccount removed successfully.";
        print_menu ();
        process_choice ()
      with Not_found ->
        print_endline "Sorry, this account does not exist!";
        print_menu ();
        process_choice ())
    else (
      print_menu ();
      process_choice ()))
  else login_failure ()

and request_credentials prompt =
  print_string [ Reset ] "\n";
  let action_color =
    match prompt with
    | "Login" -> [ Foreground Green ]
    | "Register" -> [ Foreground Blue ]
    | _ -> [ Reset ]
  in
  print_string action_color
    (prompt ^ " (or enter 'back' to return to the main menu)\n");
  print_string [ Reset ] "Enter a username: ";
  let username = read_line () in
  if username = "back" then (
    main_menu ();
    raise Exit)
  else (
    print_string [ Reset ] "Enter a password: ";
    let password = read_line () in
    (username, password))

and login_success user =
  print_string [ Foreground Green ] "Login successful!\n";
  Final_project.Overview.dashboard_login user

and login_failure () =
  print_string [ Foreground Red ]
    "Invalid username or password. Please press enter to try again.\n";
  ignore (read_line ());
  login ()

and registration_success () =
  print_string [ Foreground Green ] "\nRegistration successful!\n";
  prompt_for_acknowledgment "Redirecting to login\n";
  login ()

and prompt_for_acknowledgment message =
  print_string [ Reset ] (message ^ " Press enter to continue.\n");
  ignore (read_line ())

and username_exists () =
  print_string [ Foreground Red ]
    "Username already exists. Please press enter to try a different one.\n";
  ignore (read_line ());
  register ()

and username_doesnt_exist () =
  print_string [ Foreground Red ]
    "This user does not exist. Please try again.\n";
  login ()

and main_menu () =
  print_menu ();
  process_choice ()

let () = main_menu ()
