open ANSITerminal

let add_data data path =
  try
    let sheet = Csv.load path in
    Csv.save path (data :: sheet)
  with
  | Sys_error msg -> Printf.eprintf "File system error: %s\n" msg
  | e ->
      Printf.eprintf "CSV operation failed or unexpected exception: %s\n"
        (Printexc.to_string e)

let rec data_aux_limit lst acc limit =
  if limit = 0 then acc
  else
    match lst with
    | [] -> acc
    | h :: t ->
        if acc <> "" then
          data_aux_limit t (acc ^ "\n" ^ String.concat " " h) (limit - 1)
        else data_aux_limit t (String.concat " " h) (limit - 1)

let get_data path limit =
  let sheet = Csv.load path in
  match sheet with
  | [] -> "\n -- \n"
  | _ :: _ -> (
      match limit with
      | Some n -> "\n" ^ data_aux_limit sheet "" n ^ "\n"
      | None ->
          "\n"
          ^ List.fold_right
              (fun lst acc ->
                if acc <> "" then String.concat " " lst ^ "\n" ^ acc
                else String.concat " " lst)
              sheet ""
          ^ "\n")

let data_to_list path = List.concat (Csv.load path)

(* modify, data analysis, erase all data *)

let rec contains lst elm =
  match lst with
  | [] -> false
  | h :: t -> if h = elm then true else contains t elm

let rec remove_data_list lst data =
  match lst with
  | [] -> raise Not_found
  | h :: t -> if contains h data then t else h :: remove_data_list t data

let rec edit_data lst id data =
  match lst with
  | [] -> [ [ id; "\n"; data ] ]
  | h :: t -> (
      match h with
      | [] -> h :: edit_data t id data
      | a :: _ as c ->
          if a = id then (c @ [ data ]) :: t else h :: edit_data t id data)

let edit id path data =
  let lst = Csv.load path in
  Csv.save path (edit_data lst id data)

let remove_data path data =
  let lst = Csv.load path in
  Csv.save path (remove_data_list lst data)

let search id path =
  let sheet = Csv.load path in
  try List.exists (fun row -> List.nth row 0 = id) sheet
  with Failure _ -> false

let search2 id1 id2 path =
  let sheet = Csv.load path in
  try
    List.exists (fun row -> List.nth row 0 = id1 && List.nth row 1 = id2) sheet
  with Failure _ -> false

let find_entry id path =
  let sheet = Csv.load path in
  "\n"
  ^ String.concat " " (List.find (fun row -> List.nth row 0 = id) sheet)
  ^ "\n"

let rec search_entry header path =
  print_string [ Reset ]
    "\n\
     Enter 'back' to go back to the menu. \n\
    \ Enter a date in the format day-month-year (ex. 2-3-2024) ";
  let date = read_line () in
  if date = "back" then ()
  else if date = "" then (
    print_string [ Foreground Red ] "Sorry, this entry does not exist!\n";
    search_entry header path)
  else
    try print_endline (header ^ find_entry date path)
    with Not_found ->
      print_string [ Foreground Red ] "Sorry, this entry does not exist!\n";
      search_entry header path

let rec see_history header path =
  print_string [ Reset ]
    "\n\
     Enter 'back' to go back to the menu. \n\
    \ Would you like to limit the history you see? (y/n) ";
  let message = read_line () in
  if message = "back" then ()
  else if message = "y" then
    let () =
      print_string [ Reset ]
        "\nHow many recent entries would you like to see? (enter a number) "
    in
    let message2 = read_line () in
    try
      let limit = int_of_string message2 in
      print_endline (header ^ get_data path (Some limit))
    with _ -> print_endline (header ^ get_data path None)
  else if message = "n" then print_endline (header ^ get_data path None)
  else (
    print_string [ Foreground Red ] "\nPlease enter y or n\n";
    see_history header path)

let rec remove_entry path =
  print_string [ Reset ]
    "\n\
     Enter 'back' to go back to the menu. \n\
     Enter a date in the format day-month-year (ex. 2-3-2024) ";
  let date = read_line () in
  if date = "back" then ()
  else if date = "" then (
    print_string [ Foreground Red ] "\nSorry, this entry does not exist!\n";
    remove_entry path)
  else
    try
      remove_data path date;
      print_string [ Foreground Green ] "Removed entry successfully.\n"
    with Not_found ->
      print_string [ Foreground Red ] "Sorry, this entry does not exist!\n";
      remove_entry path
