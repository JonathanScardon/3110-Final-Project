let add_data data path =
  try
    let sheet = Csv.load path in
    Csv.save path (data :: sheet);
    true
  with
  | Sys_error msg ->
      Printf.eprintf "File system error: %s\n" msg;
      false
  | e ->
      Printf.eprintf "CSV operation failed or unexpected exception: %s\n"
        (Printexc.to_string e);
      false

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

(* remove, modify, data analysis, search *)

let rec contains lst elm =
  match lst with
  | [] -> false
  | h :: t -> if h = elm then true else contains t elm

let rec remove_data_list path data =
  let lst = Csv.load path in
  match lst with
  | [] -> []
  | h :: t -> if contains h data then t else h :: remove_data_list path data

let remove_data path data = Csv.save path (remove_data_list path data)
