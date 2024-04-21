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
  | [] -> [ [ id; data ] ]
  | h :: t -> (
      match h with
      | [] -> h :: edit_data t id data
      | a :: b ->
          if a = id then ([ id; data ] @ b) :: t else h :: edit_data t id data)

let edit id path data =
  let lst = Csv.load path in
  Csv.save path (edit_data lst id data)

let remove_data path data =
  let lst = Csv.load path in
  Csv.save path (remove_data_list lst data)

let search id path =
  let sheet = Csv.load path in
  List.exists (fun row -> List.nth row 0 = id) sheet

let find_entry id path =
  let sheet = Csv.load path in
  "\n"
  ^ String.concat " " (List.find (fun row -> List.nth row 0 = id) sheet)
  ^ "\n"

let rec search_entry user header path =
  print_string [ Reset ]
    "\n\
     Enter 'back' to go back to the menu. \n\
    \ Enter a date in the format day-month-year (ex. 2-3-2024) ";
  let date = read_line () in
  if date = "back" then ()
  else if date = "" then (
    print_string [ Foreground Red ] "Sorry, this entry does not exist!\n";
    search_entry user header path)
  else
    try print_endline (header ^ find_entry date path)
    with Not_found ->
      print_string [ Foreground Red ] "Sorry, this entry does not exist!\n";
      search_entry user header path
