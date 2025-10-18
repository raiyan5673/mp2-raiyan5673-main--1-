open OUnit2
open Mp2.Arith

let sample_expr = Add (Pow (Var "x", 2), Mul (Const 3., Var "x"))

let test_to_string _ =
  assert_equal "((x^2) + (3. * x))" (pretty_print sample_expr) ~printer:(fun x -> x)

let test_diff_structure _ =
  let expected =
    Add
      ( Mul (Const 2., Mul (Pow (Var "x", 1), Const 1.)),
        Add (Mul (Const 0., Var "x"), Mul (Const 3., Const 1.)) )
  in
  assert_equal expected (diff "x" sample_expr) ~printer:pretty_print

let test_simplify_removes_neutral _ =
  let expr = Add (Const 0., Mul (Const 1., Var "x")) in
  assert_equal (Var "x") (simplify expr) ~printer:pretty_print

let test_map_expr_scaling _ =
  let scaled = map_expr (fun c -> c *. 2.) sample_expr in
  let expected = Add (Pow (Var "x", 2), Mul (Const 6., Var "x")) in
  assert_equal expected scaled ~printer:pretty_print

let test_fold_expr_sums_constants _ =
  let total_constants =
    fold_expr
      (fun c -> c)
      (fun _ -> 0.)
      ( +. )
      ( +. )
      (fun base _ -> base)
      sample_expr
  in
  assert_equal 3. total_constants ~printer:string_of_float

let suite =
  "arith"
  >::: [
         "to_string formats nested expression" >:: test_to_string;
         "diff generates expected structure" >:: test_diff_structure;
         "simplify collapses neutral elements" >:: test_simplify_removes_neutral;
         "map_expr scales constants only" >:: test_map_expr_scaling;
         "fold_expr aggregates constants" >:: test_fold_expr_sums_constants;
       ]
