open Data
open ANSITerminal

let () = Random.self_init ()
let day = string_of_int (Unix.localtime (Unix.time ())).tm_mday
let month = string_of_int ((Unix.localtime (Unix.time ())).tm_mon + 1)
let year = string_of_int ((Unix.localtime (Unix.time ())).tm_year + 1900)
let curr_date = day ^ "-" ^ month ^ "-" ^ year

let rec happiness_log () =
  print_string [ Reset ] "Rate your happiness 1-10: ";
  let happiness_lvl = read_line () in
  try
    let hap_lvl_int = int_of_string happiness_lvl in
    if hap_lvl_int >= 1 && hap_lvl_int <= 10 then happiness_lvl
    else happiness_log ()
  with Failure _ -> happiness_log ()

let see_history user =
  erase Screen;
  let header = "\nDate | Happiness | Mood" in
  print_string [ Reset ]
    "Type 0 to go back to the menu. \n\
    \ Would you like to limit the history you see? (y/n) ";
  let message = read_line () in
  if message = "0" then ()
  else if message = "y" then
    let () =
      print_string [ Reset ]
        "How many recent entries would you like to see? (enter a number) "
    in
    try
      let limit = int_of_string message in
      print_endline
        (header ^ get_data ("data/" ^ user ^ "_mood.csv") (Some limit))
    with _ ->
      print_endline (header ^ get_data ("data/" ^ user ^ "_mood.csv") None)
  else print_endline (header ^ get_data ("data/" ^ user ^ "_mood.csv") None)

let rec search_entry user =
  print_string [ Reset ]
    "Type 0 to go back to the menu. \n\
    \ Enter a date in the format day-month-year (ex. 2-3-2024) ";
  let date = read_line () in
  let header = "\nDate | Happiness | Mood" in
  let path = "data/" ^ user ^ "_mood.csv" in
  if date = "0" then ()
  else if date = "" then (
    print_string [ Foreground Red ] "Sorry, this entry does not exist!\n";
    search_entry user)
  else
    try print_endline (header ^ find_entry date path)
    with Not_found ->
      print_string [ Foreground Red ] "Sorry, this entry does not exist!\n";
      search_entry user

let rec remove_entry user =
  print_string [ Reset ]
    "Type 0 to go back to the menu. \n\
     Enter a date in the format day-month-year (ex. 2-3-2024) ";
  let date = read_line () in
  let path = "data/" ^ user ^ "_mood.csv" in
  if date = "0" then ()
  else if date = "" then (
    print_string [ Foreground Red ] "Sorry, this entry does not exist!\n";
    remove_entry user)
  else
    try
      remove_data path date;
      print_string [ Foreground Green ] "Removed entry successfully.\n"
    with Not_found ->
      print_string [ Foreground Red ] "Sorry, this entry does not exist!\n";
      remove_entry user

let add_quote user =
  print_string [ Reset ] "Type 0 to go back to the menu. \nEnter a message: ";
  let message = read_line () in
  if message = "0" then ()
  else
    let path = "data/" ^ user ^ "_quotes.csv" in
    add_data [ message ] path;
    print_string [ Foreground Green ] "Message saved successfully.\n"

let get_random_quote user =
  let quotes = Csv.load ("data/" ^ user ^ "_quotes.csv") in
  try List.nth (List.nth quotes (Random.int (List.length quotes))) 0
  with Invalid_argument _ -> "No quotes found. Why not add one?"

let remove_curr_quote user curr_quote =
  try
    remove_data ("data/" ^ user ^ "_quotes.csv") curr_quote;
    print_string [ Foreground Green ] "Removed quote successfully.\n"
  with Not_found -> print_string [ Foreground Red ] "Something went wrong.\n"
