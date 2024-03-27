let add_data user data path =
  try
    let sheet = Csv.load path in
    Csv.save path ([ user; data ] :: sheet);
    true
  with
  | Sys_error msg ->
      Printf.eprintf "File system error: %s\n" msg;
      false
  | e ->
      Printf.eprintf "CSV operation failed or unexpected exception: %s\n"
        (Printexc.to_string e);
      false
