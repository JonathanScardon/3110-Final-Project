(* auth.ml *)
open Cryptokit
open ANSITerminal
(* Cite ChatGPT for cryptokit password hashing *)

let credentials_path () =
  let path =
    match Sys.getenv_opt "CREDENTIALS_PATH" with
    | Some path -> path
    | None -> (
        match Sys.getenv_opt "ENV" with
        | Some "test" -> "test/test_auth.csv"
        | _ -> "data/user_credentials.csv")
  in
  path

let username_exists username =
  let credentials = Csv.load (credentials_path ()) in
  List.exists (fun row -> List.nth row 0 = username) credentials

let hash_password password = hash_string (Hash.sha256 ()) password

let add_user username hashed_password =
  try
    if username_exists username then (
      print_string [ Foreground Red ]
        (Printf.sprintf "Username %s already exists.\n" username);
      false)
    else
      let credentials = Csv.load (credentials_path ()) in
      Csv.save (credentials_path ())
        (credentials @ [ [ username; hashed_password ] ]);
      (* Attempt to create necessary user files and handle each failure separately *)
      let create_user_file file data =
        try
          Csv.save file data;
          print_string [ Foreground Green ]
            (Printf.sprintf "File created: %s\n" file)
        with Sys_error msg ->
          print_string [ Foreground Red ]
            (Printf.sprintf "Failed to create file %s: %s\n" file msg)
      in
      create_user_file ("data/" ^ username ^ "_mood.csv") [ [] ];
      create_user_file ("data/" ^ username ^ "_food.csv") [ [] ];
      create_user_file ("data/" ^ username ^ "_exercise.csv") [ [] ];
      create_user_file ("data/" ^ username ^ "_financials.csv") [ [] ];
      (* Check for the existence of default files before copying *)
      let copy_defaults src_file dest_file =
        if Sys.file_exists src_file then
          let data = Csv.load src_file in
          create_user_file dest_file data
        else
          print_string [ Foreground Red ]
            (Printf.sprintf "Source file %s does not exist.\n" src_file)
      in
      copy_defaults "data/stock_financials.csv"
        ("data/" ^ username ^ "_stock_financials.csv");
      copy_defaults "data/quotesdata.csv" ("data/" ^ username ^ "_quotes.csv");
      true
  with
  | Sys_error msg ->
      print_string [ Foreground Red ]
        (Printf.sprintf "File system error during registration: %s\n" msg);
      false
  | e ->
      print_string [ Foreground Red ]
        (Printf.sprintf "Unexpected exception during registration: %s\n"
           (Printexc.to_string e));
      false

let authenticate username password =
  let credentials = Csv.load (credentials_path ()) in
  List.exists
    (fun row ->
      let stored_username = List.nth row 0 in
      let stored_hashed_password = List.nth row 1 in
      stored_username = username
      && stored_hashed_password = hash_password password)
    credentials
(**)
