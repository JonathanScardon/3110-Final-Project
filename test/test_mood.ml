open OUnit2
open Final_project.Mood

(* validate_happiness *)

let test_validate_happiness _ =
  assert_equal (Some 5) (validate_happiness "5")
    ~msg:"Should validate 5 as within range";
  assert_equal None (validate_happiness "0")
    ~msg:"Should reject 0 as out of range";
  assert_equal None (validate_happiness "11")
    ~msg:"Should reject 11 as out of range";
  assert_equal None (validate_happiness "abc")
    ~msg:"Should reject non-numeric input";
  assert_equal (Some 1) (validate_happiness "1")
    ~msg:"Should validate the lower boundary 1";
  assert_equal (Some 10) (validate_happiness "10")
    ~msg:"Should validate the upper boundary 10"

let suite =
  "Happiness Tests" >::: [ "validate_happiness" >:: test_validate_happiness ]

let () = run_test_tt_main
