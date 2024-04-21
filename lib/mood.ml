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
  Data.see_history "\nDate | Happiness | Mood" ("data/" ^ user ^ "_mood.csv")

let search_entry user =
  Data.search_entry "\nDate | Happiness | Mood" ("data/" ^ user ^ "_mood.csv")

let remove_entry user = Data.remove_entry ("data/" ^ user ^ "_mood.csv")

let add_quote user =
  print_string [ Reset ]
    "Enter 'back' to go back to the menu. \nEnter a message: ";
  let message = read_line () in
  if message = "back" then ()
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
