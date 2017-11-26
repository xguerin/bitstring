open OUnit2

let () =
  [
    BitstringLegacyTest.suite;
    BitstringParserTest.suite;
    BitstringQualifierTest.suite;
  ]
  |> List.iter (fun t -> run_test_tt_main t)
