open OUnit2
open Final_project.Mood

let test_validate_happiness _ =
  assert_equal (Some 5) (validate_happiness "5")
    ~msg:"Should validate 5 as within range";
  assert_equal None (validate_happiness "0")
    ~msg:"Should reject 0 as out of range";
  assert_equal None (validate_happiness "11")
    ~msg:"Should reject 11 as out of range";
  assert_equal None (validate_happiness "abc")
    ~msg:"Should reject non-numeric input";
  assert_equal None (validate_happiness "2.0")
    ~msg:"Should reject non-intL input";
  assert_equal (Some 1) (validate_happiness "1")
    ~msg:"Should validate the lower boundary 1";
  assert_equal (Some 10) (validate_happiness "10")
    ~msg:"Should validate the upper boundary 10"

let test_get_random_quote_empty _ =
  assert_equal "No quotes found. Why not add one?"
    (get_random_quote "for_testing/test1")
    ~msg:"Should test for empty csv response"

let test_get_random_quote_valid _ =
  let result = get_random_quote "for_testing/test2" in
  let expected_quotes = [ "quote 1"; "quote 2"; "quote 3" ] in
  let message =
    Printf.sprintf "Returned quote should be one of %s but was '%s'"
      (String.concat ", " expected_quotes)
      result
  in
  assert_bool message (List.mem result expected_quotes)

let test_get_only_quote _ =
  assert_equal "only"
    (get_random_quote "for_testing/test3")
    ~msg:"Should test only possible quote returns"

let suite =
  "Mood.ml tests"
  >::: [
         "validate_happiness" >:: test_validate_happiness;
         "get_random_quote_empty" >:: test_get_random_quote_empty;
         "get_random_quote_valid" >:: test_get_random_quote_valid;
         "get_only_quote" >:: test_get_only_quote;
       ]

let () = run_test_tt_main suite
