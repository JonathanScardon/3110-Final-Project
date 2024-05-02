open OUnit2
open Final_project.Health

let test_add_health_data_correct_entry _ =
  (* Mock read_line to simulate user input and edit to capture its effect *)
  let original_read_line = !Clflags.read_line_ref in
  let original_edit = !Data.edit_ref in
  (Clflags.read_line_ref := fun () -> "Running");
  (Data.edit_ref :=
     fun date path data ->
       assert_equal ~msg:"Should save correct data"
         "Running; 1 hr; current_time_string\n" data);

  try
    Health.add_health_data "test_user" "exercise";
    (* Clean up: Restore original functions *)
    Clflags.read_line_ref := original_read_line;
    Data.edit_ref := original_edit
  with ex ->
    (* Ensure cleanup if an exception occurs *)
    Clflags.read_line_ref := original_read_line;
    Data.edit_ref := original_edit;
    raise ex
