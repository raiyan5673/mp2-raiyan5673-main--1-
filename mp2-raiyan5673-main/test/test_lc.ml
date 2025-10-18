open OUnit2
open Mp2.Lc

let string_list_printer strs = "[" ^ String.concat "; " strs ^ "]"

let test_free_vars_abs_application _ =
  let expr = Abs ("x", App (Var "x", Var "y")) in
  assert_equal ["y"] (free_vars expr) ~printer:string_list_printer

let test_free_vars_app_duplicates _ =
  let expr = App (Var "x", App (Var "x", Var "z")) in
  assert_equal ["x"; "z"] (free_vars expr) ~printer:string_list_printer

let test_subst_var_replacement _ =
  let result = subst (Var "z") "x" (Var "x") in
  assert_equal (Var "z") result ~printer:pp

let test_subst_avoids_capture _ =
  let body = Abs ("y", App (Var "x", Var "y")) in
  let result = subst (Var "y") "x" body in
  let expected = Abs ("y1", App (Var "y", Var "y1")) in
  assert_equal expected result ~printer:pp

let test_step_applicative_beta_value _ =
  let expr = App (Abs ("x", Var "x"), Abs ("y", Var "y")) in
  match step_applicative expr with
  | Some reduced -> assert_equal (Abs ("y", Var "y")) reduced ~printer:pp
  | None -> assert_failure "Expected a reduction step"

let test_step_applicative_evaluates_argument _ =
  let arg = App (Abs ("u", Abs ("v", Var "u")), Abs ("w", Var "w")) in
  let expr = App (Abs ("x", Var "x"), arg) in
  let expected =
    App (Abs ("x", Var "x"), Abs ("v", Abs ("w", Var "w")))
  in
  match step_applicative expr with
  | Some reduced -> assert_equal expected reduced ~printer:pp
  | None -> assert_failure "Expected argument to take a step"

let test_eval_applicative_identity _ =
  let expr = App (Abs ("x", Var "x"), Abs ("y", Var "y")) in
  let result = eval_applicative expr in
  assert_equal (Abs ("y", Var "y")) result ~printer:pp

let test_eval_applicative_nested _ =
  let inner =
    App (Abs ("u", Abs ("v", Var "u")), Abs ("w", Var "w"))
  in
  let expr = App (Abs ("x", Var "x"), inner) in
  let result = eval_applicative expr in
  let expected = Abs ("v", Abs ("w", Var "w")) in
  assert_equal expected result ~printer:pp

let suite =
  "lc"
  >::: [
         "free_vars removes bound identifiers" >:: test_free_vars_abs_application;
         "free_vars drops duplicates" >:: test_free_vars_app_duplicates;
         "subst replaces matching variable" >:: test_subst_var_replacement;
         "subst performs alpha_conversion when needed" >:: test_subst_avoids_capture;
         "step_applicative reduces beta when argument value" >:: test_step_applicative_beta_value;
         "step_applicative reduces argument first" >:: test_step_applicative_evaluates_argument;
         "eval_applicative reduces simple identity application" >:: test_eval_applicative_identity;
         "eval_applicative reduces nested application" >:: test_eval_applicative_nested;
       ]
