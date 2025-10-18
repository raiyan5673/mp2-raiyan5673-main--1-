open OUnit2

let () =
  run_test_tt_main
    ("mp2" >::: [Test_arith.suite; Test_lc.suite])
