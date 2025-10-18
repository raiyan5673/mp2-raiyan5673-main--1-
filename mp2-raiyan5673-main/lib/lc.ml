(* ===== Part 2: Lambda Calculus ===== *)

type expr =
  | Var of string
  | App of expr * expr
  | Abs of string * expr

(* Provided helper in the handout; a simple compatible version here: *)
let fresh_like (base : string) (in_use : string list) : string =
  let rec loop i =
    let cand = if i = 0 then base else base ^ string_of_int i in
    if List.mem cand in_use then loop (i + 1) else cand
  in
  loop 0

  
let rec pp (t : expr) : string =
  match t with
  | Var x -> x
  | App (t1, t2) -> "(" ^ pp t1 ^ " " ^ pp t2 ^ ")"
  | Abs (x, body) -> "(\\" ^ x ^ "." ^ pp body ^ ")"

let uniq (xs : string list) : string list =
  let rec go seen = function
    | [] -> List.rev seen
    | h :: t -> if List.mem h seen then go seen t else go (h :: seen) t
  in
  go [] xs

let union (a : string list) (b : string list) : string list =
  uniq (a @ b)

let remove (x : string) (xs : string list) : string list =
  List.filter (fun y -> y <> x) xs

(* ---------------- Exercise 5: Free Variables ---------------- *)

let rec free_vars (t : expr) : string list =
  match t with
  | Var x -> [x]
  | App (t1, t2) -> union (free_vars t1) (free_vars t2)
  | Abs (x, body) -> remove x (free_vars body)

(* ---------------- Exercise 6: Substitution (capture-avoiding) ---------------- *)
(* subst v x t  ==  replace free occurrences of variable x in t with v *)

let rec subst (v : expr) (x : string) (t : expr) : expr =
  match t with
  | Var y -> if x = y then v else t
  | App (t1, t2) -> App (subst v x t1, subst v x t2)
  | Abs (y, body) ->
      if x = y then t
      else
        let fv_v = free_vars v in
        if List.mem y fv_v then
          let in_use = union (free_vars body) (union fv_v [x; y]) in
          let y' = fresh_like y in_use in
          let body' = subst (Var y') y body in
          Abs (y', subst v x body')
        else
          Abs (y, subst v x body)

(* ---------------- Exercise 7: Applicative-Order Evaluation ---------------- *)

let is_value = function
  | Abs _ -> true
  | _ -> false

let rec step_applicative (t : expr) : expr option =
  match t with
  | Var _ -> None
  | Abs _ -> None
  | App (t1, t2) ->
      begin match t1 with
      | Abs (x, body) ->
          if is_value t2 then Some (subst t2 x body)
          else (match step_applicative t2 with
                | None -> None
                | Some t2' -> Some (App (t1, t2')))
      | _ ->
          (match step_applicative t1 with
           | None -> None
           | Some t1' -> Some (App (t1', t2)))
      end

let rec eval_applicative (t : expr) : expr =
  match step_applicative t with
  | None -> t
  | Some t' -> eval_applicative t'
