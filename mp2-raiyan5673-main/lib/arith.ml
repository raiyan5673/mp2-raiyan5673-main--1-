(* ===== Part 1: Symbolic Arithmetic Expressions ===== *)

type expr =
  | Const of float
  | Var of string
  | Add of expr * expr
  | Mul of expr * expr
  | Pow of expr * int

(* ---------------- Exercise 1: Pretty Printing ---------------- *)

let rec pretty_print (e : expr) : string =
  match e with
  | Const c -> string_of_float c
  | Var v -> v
  | Add (a, b) -> "(" ^ pretty_print a ^ " + " ^ pretty_print b ^ ")"
  | Mul (a, b) -> "(" ^ pretty_print a ^ " * " ^ pretty_print b ^ ")"
  | Pow (base, n) -> "(" ^ pretty_print base ^ "^" ^ string_of_int n ^ ")"

(* ---------------- Exercise 2: Symbolic Differentiation ---------------- *)

let rec diff (x : string) (e : expr) : expr =
  match e with
  | Const _ -> Const 0.
  | Var y -> if y = x then Const 1. else Const 0.
  | Add (u, v) -> Add (diff x u, diff x v)
  | Mul (u, v) -> Add (Mul (diff x u, v), Mul (u, diff x v))
  | Pow (u, n) ->
      let n' = float_of_int n in
      (* Build: n * (u^(n-1) * u')  to match the test's expected structure *)
      Mul (Const n', Mul (Pow (u, n - 1), diff x u))

(* ---------------- Exercise 3: Simplification ---------------- *)

let rec simplify (e : expr) : expr =
  let s = simplify in
  match e with
  | Const _ | Var _ -> e
  | Add (a, b) ->
      let a' = s a and b' = s b in
      begin match (a', b') with
      | Const 0., e' -> e'
      | e', Const 0. -> e'
      | _ -> Add (a', b')
      end
  | Mul (a, b) ->
      let a' = s a and b' = s b in
      begin match (a', b') with
      | Const 0., _ -> Const 0.
      | _, Const 0. -> Const 0.
      | Const 1., e' -> e'
      | e', Const 1. -> e'
      | _ -> Mul (a', b')
      end
  | Pow (base, n) ->
      let base' = s base in
      if n = 1 then base' else Pow (base', n)

(* ---------------- Exercise 4: map_expr and fold_expr ---------------- *)

let rec map_expr (f : float -> float) (e : expr) : expr =
  match e with
  | Const c -> Const (f c)
  | Var v -> Var v
  | Add (a, b) -> Add (map_expr f a, map_expr f b)
  | Mul (a, b) -> Mul (map_expr f a, map_expr f b)
  | Pow (base, n) -> Pow (map_expr f base, n)

let rec fold_expr
    (f_const : float -> 'a)
    (f_var   : string -> 'a)
    (f_add   : 'a -> 'a -> 'a)
    (f_mul   : 'a -> 'a -> 'a)
    (f_pow   : 'a -> int -> 'a)
    (e       : expr) : 'a =
  match e with
  | Const c -> f_const c
  | Var v -> f_var v
  | Add (a, b) ->
      let a' = fold_expr f_const f_var f_add f_mul f_pow a in
      let b' = fold_expr f_const f_var f_add f_mul f_pow b in
      f_add a' b'
  | Mul (a, b) ->
      let a' = fold_expr f_const f_var f_add f_mul f_pow a in
      let b' = fold_expr f_const f_var f_add f_mul f_pow b in
      f_mul a' b'
  | Pow (base, n) ->
      let base' = fold_expr f_const f_var f_add f_mul f_pow base in
      f_pow base' n
