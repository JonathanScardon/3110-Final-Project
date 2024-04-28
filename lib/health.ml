open Data
open ANSITerminal

let () = Random.self_init ()
let day = string_of_int (Unix.localtime (Unix.time ())).tm_mday
let month = string_of_int ((Unix.localtime (Unix.time ())).tm_mon + 1)
let year = string_of_int ((Unix.localtime (Unix.time ())).tm_year + 1900)
let curr_date = day ^ "-" ^ month ^ "-" ^ year
let hour = string_of_int (Unix.localtime (Unix.time ())).tm_hour

let min =
  let num = (Unix.localtime (Unix.time ())).tm_min in
  if num < 10 then "0" ^ string_of_int num else string_of_int num

let time_of_day = hour ^ ":" ^ min

let add_health_data user journal =
  let msg1 =
    if journal = "food" then "What type of meal did you eat? "
    else "Which type of exercise did you do? "
  in
  let msg2 =
    if journal = "food" then "Describe the food that you ate: "
    else "How many hours did you spend exercising? "
  in
  let path = "data/" ^ user ^ "_" ^ journal ^ ".csv" in
  print_string [ Reset ] ("\nEnter 'back' to go back to the menu. \n" ^ msg1);
  let input1 = read_line () in
  if input1 = "back" then ()
  else
    let () = print_string [ Reset ] ("\n" ^ msg2) in
    let input2 = read_line () in
    if input2 = "back" then ()
    else
      let input2 = if journal = "exercise" then input2 ^ " hr" else input2 in
      let data = input1 ^ "; " ^ input2 ^ "; " ^ time_of_day ^ "\n" in
      print_string [ Foreground Green ] "\nEntry added successfully!\n";
      edit curr_date path data

let rec select_journal user func msg =
  let () =
    print_string [] ("\nEnter 'back' to go back to the menu. \n " ^ msg)
  in
  let input = read_line () in
  if input = "back" then ()
  else if
    String.lowercase_ascii input = "food"
    || String.lowercase_ascii input = "food journal"
  then (
    func ("data/" ^ user ^ "_food.csv");
    Unix.sleep 2)
  else if
    String.lowercase_ascii input = "exercise"
    || String.lowercase_ascii input = "exercise journal"
  then (
    func ("data/" ^ user ^ "_exercise.csv");
    Unix.sleep 2)
  else
    let () =
      print_string [ Foreground Red ]
        "\nPlease choose between your food or exercise journal.\n"
    in
    select_journal user func msg

let search_entry path = Data.search_entry "" path

let see_history path =
  Data.see_history "" path;
  Unix.sleep 2

let remove_entry path = Data.remove_entry path

(** Generates a meal from a list, [meals], of meal ideas with name [name]. *)
let generate_meal name meals =
  print_endline (name ^ ":");
  print_endline (List.nth meals (Random.int (List.length meals)))

let rec mealplan user n =
  let mealplan_day breakfast lunch dinner =
    let () = print_endline "" in
    let () = generate_meal "breakfast" breakfast in
    let () = generate_meal "lunch" lunch in
    let () = generate_meal "dinner" dinner in
    print_endline ""
  in
  let breakfast = data_to_list ("data/" ^ user ^ "_breakfast.csv") in
  let lunch = data_to_list ("data/" ^ user ^ "_lunch.csv") in
  let dinner = data_to_list ("data/" ^ user ^ "_dinner.csv") in
  if breakfast = [] || lunch = [] || dinner = [] then
    print_string [ Foreground Red ]
      "\nPlease add more meal ideas! You do not have enough right now.\n"
  else if n = 1 then mealplan_day breakfast lunch dinner
  else (
    mealplan_day breakfast lunch dinner;
    mealplan user (n - 1))

let rec add_meal user =
  print_string [ Reset ]
    "\n\
     Enter 'back' to go back to the menu. \n\
     Enter type of meal (breakfast, lunch, or dinner): ";
  let message = read_line () in
  match message with
  | "back" -> ()
  | ("breakfast" | "lunch" | "dinner") as meal ->
      let path = "data/" ^ user ^ "_" ^ meal ^ ".csv" in
      let () = print_string [ Reset ] "\nEnter meal idea: " in
      let message2 = read_line () in
      if message2 = "back" then ()
      else (
        add_data [ message2 ] path;
        print_string [ Foreground Green ] "\nMeal saved successfully.\n")
  | _ ->
      print_string [ Foreground Red ]
        "\nInvalid meal type. Please enter breakfast, lunch, or dinner.\n";
      add_meal user

let rec view_meal user =
  print_string [ Reset ]
    "\n\
     Enter 'back' to go back to the menu. \n\
     Enter type of meal (breakfast, lunch, or dinner): ";
  let message = read_line () in
  match message with
  | "back" -> ()
  | ("breakfast" | "lunch" | "dinner") as meal ->
      let path = "data/" ^ user ^ "_" ^ meal ^ ".csv" in
      print_endline ("\n" ^ meal ^ "\n" ^ get_data path None);
      Unix.sleep 2
  | _ ->
      print_string [ Foreground Red ]
        "\nInvalid meal type. Please enter breakfast, lunch, or dinner.\n";
      view_meal user

let rec remove_meal_entry path =
  let () = print_string [ Reset ] "\nEnter meal to remove: " in
  let message2 = read_line () in
  if message2 = "back" then ()
  else
    try
      remove_data path message2;
      print_string [ Foreground Green ] "\nRemoved meal successfully.\n"
    with Not_found ->
      print_string [ Foreground Red ] "\nSorry, this meal does not exist!\n";
      remove_meal_entry path

let rec remove_meal user =
  print_string [ Reset ]
    "\n\
     Enter 'back' to go back to the menu. \n\
     Enter type of meal (breakfast, lunch, or dinner): ";
  let message = read_line () in
  match message with
  | "back" -> ()
  | ("breakfast" | "lunch" | "dinner") as meal ->
      let path = "data/" ^ user ^ "_" ^ meal ^ ".csv" in
      remove_meal_entry path
  | _ ->
      print_string [ Foreground Red ]
        "\nInvalid meal type. Please enter breakfast, lunch, or dinner.\n";
      remove_meal user
