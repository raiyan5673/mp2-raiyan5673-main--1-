# MP2: Symbolic Manipulation

## Overview

In this two-part assignment, you’ll explore how to represent and manipulate
symbolic structures -- first in the context of arithmetic expressions, and then
in the lambda calculus.

In Part 1, you’ll work with symbolic arithmetic expressions. You’ll write
functions to print, simplify, and differentiate expressions, as well as to map
and reduce them using higher-order functions.

In Part 2, you’ll extend these ideas to abstract syntax trees that represent
lambda calculus expressions. You’ll implement substitution with capture
avoidance and applicative-order evaluation.

Together, these two parts bridge the gap between syntax and semantics: you’ll
first manipulate the structure of expressions, then implement the rules that
give them meaning, mirroring how real interpreters and compilers operate.

## Part 1: Symbolic Arithmetic Expressions

In this part you’ll implement a collection of functions that operate on
tree-like representations of mathematical formulae built from constants,
variables, addition, multiplication, and integer powers.

The following recursive algebraic data type (ADT) is used to construct these
arithmetic expressions:

```ocaml
type expr =
  | Const of float
  | Var of string
  | Add of expr * expr
  | Mul of expr * expr
  | Pow of expr * int
```

Here's an example of an expression which represents $x^2 + 3x$:

```ocaml
let e = Add (Pow (Var "x", 2), Mul (Const 3., Var "x"))
```

The goal of this part is to help you become comfortable representing and
transforming symbolic data recursively, using pattern matching and algebraic
data types -- key techniques used in the implementation of interpreters and
compilers.

All your code for the exercises in part 1 will go into "lib/arith.ml".

### Exercise 1: Pretty Printing

Define a function to convert an expression into a human-readable string.

```ocaml
val pretty_print : expr -> string
```

Example:

```ocaml
# pretty_print (Add (Pow (Var "x", 2), Mul (Const 3., Var "x")));;
- : string = "(x^2 + (3. * x))"

# pretty_print (Add (Const 1., Add (Const 2., Const 3.)));;
- : string = "(1. + (2. + 3.))"
```

- You may *fully parenthesize* sub-expressions. I.e., you needn't try to
  minimize the levels of parentheses used in the returned string.

### Exercise 2: Symbolic Differentiation

Implement symbolic differentiation of expression with respect to a variable
(usually "x"):

```ocaml
val diff : string -> expr -> expr
```

Below are the fundamental differentiation rules you’ll need. Each corresponds to
one of the constructors of the `expr` type.

| Expression       | Rule                             |
| ---------------- | -------------------------------- |
| Constant $c$     | $\frac{d}{dx} c = 0$             |
| Variable $x$     | $\frac{d}{dx} x = 1$             |
| Variable $y ≠ x$ | $\frac{d}{dx} y = 0$             |
| Addition         | $\frac{d}{dx} (u + v) = u' + v'$ |
| Multiplication   | $\frac{d}{dx} (uv) = u'v + uv'$  |
| Power            | $\frac{d}{dx} u^n = nu^(n-1)u'$  |

Example:

```ocaml
# diff "x" (Add (Pow (Var "x", 2), Mul (Const 3., Var "x")));;

- : expr = Add (Mul (Const 2., Mul (Pow (Var "x", 1), Const 1.)),
             Add (Mul (Const 0., Var "x"), Mul (Const 3., Const 1.)))
```

- i.e., $\frac{d}{dx} (x^2 + 3x) = (2)(x^1)(1) + ((0)(x) + (3)(1))$
  - which simplifies to $2x + 3$

### Exercise 3: Simplification

Implement simplification of trivial sub-expressions.

```ocaml
val simplify : expr -> expr
```

Patterns of simplifiable expressions along with their reduced forms are listed
below (in the case of `Add` and `Mul`, the mirrored versions should also be
simplified).

- `Add (Const 0., e)` → `e`
- `Mul (Const 1., e)` → `e`
- `Mul (Const 0., e)` → `Const 0.`
- `Pow (e, 1)` → `e`
- `Sub (e, Const 0.)` → `e`

Example:

```ocaml
# let e = diff "x" (Add (Pow (Var "x", 2), Mul (Const 3., Var "x")));;
val e : expr =
  Add (Mul (Const 2., Mul (Pow (Var "x", 1), Const 1.)),
   Add (Mul (Const 0., Var "x"), Mul (Const 3., Const 1.)))

# e |> simplify;;
- : expr = Add (Mul (Const 2., Var "x"), Add (Const 0., Const 3.))

# e |> simplify |> simplify;;
- : expr = Add (Mul (Const 2., Var "x"), Const 3.)
```

- Each call to `simplify` recursively simplifies the entire expression tree.
  However, if simplification produces new subexpressions that can themselves be
  simplified (for example, when simplification of one node exposes a new
  constant term), you may need to call `simplify` multiple times to reach a
  fully reduced form. This behavior is expected.

### Exercise 4: Map and Fold

For this exercise you are to implement two higher-order functions on
expressions, described below:

- `map_expr` applies a given function to *every constant* (i.e., `Const` value)
  in an expression, while leaving the structure unchanged.

  ```ocaml
  val map_expr : (float -> float) -> expr -> expr
  ```

  **Example:**

  ```ocaml
  # let e = Add (Const 2., Mul (Var "x", Const 3.));;
  # map_expr (fun c -> c *. 2.) e;;
  - : expr = Add (Const 4., Mul (Var "x", Const 6.))
  ```

- `fold_expr` abstracts over the recursive structure of `expr` to accumulate a
  result, based on function arguments corresponding to each of the possible
  values.

  ```ocaml
  val fold_expr :
    (float -> 'a) ->        (* handle Const *)
    (string -> 'a) ->       (* handle Var *)
    ('a -> 'a -> 'a) ->     (* handle Add *)
    ('a -> 'a -> 'a) ->     (* handle Mul *)
    ('a -> int -> 'a) ->    (* handle Pow *)
    expr -> 'a
  ```

  `fold_expr` performs a structural, *post-order* traversal of the expression
  tree. This means it first recursively visits and processes the subexpressions,
  and then combines their results using the function associated with the current
  constructor.

  For example, for the expression `Add (Const 1., Mul (Const 2., Var "x"))`, the
  traversal and combination order looks like this:

  ```
       Add
      /   \
  Const   Mul
   1.     / \
       Const Var
        2.    x
  ```

  The evaluation proceeds as:

  1. Visit `Const 1.` -> apply `f_const`
  2. Visit `Const 2.` -> apply `f_const`
  3. Visit `Var "x"` -> apply `f_var`
  4. Combine results of visiting `Const 2.` and `Var "x"` -> apply `f_mul`
  5. Combine results of visiting `Const 1.` and `Mul(...)` -> apply `f_add`

  Here's how we can use `fold_expr` to write some expression-processing
  functions:

  Count nodes:

  ```ocaml
  let count_nodes e =
    fold_expr
      (fun _ -> 1)
      (fun _ -> 1)
      (fun a b -> 1 + a + b)
      (fun a b -> 1 + a + b)
      (fun n _ -> n + 1)
      e
  ```

  Obtaining a string representation:

  ```ocaml
  let to_string e =
    fold_expr
      string_of_float
      (fun v -> v)
      (fun a b -> "(" ^ a ^ " + " ^ b ^ ")")
      (fun a b -> "(" ^ a ^ " * " ^ b ^ ")")
      (fun a n -> "(" ^ a ^ "^" ^ string_of_int n ^ ")")
      e
  ```

## Part 2: Lambda Calculus Interpreter

While the first part of the assignment focused on *manipulating structure*, this
second part focuses on *manipulating semantics*, by asking you to fix up and
complete the lambda calculus interpreter introduced in class. To that end, you
will implement substitution, deal with variable capture, and build an
applicative-order evaluator for lambda calculus expressions.

Your work will go into the "lib/lc.ml" source file.

### Exercise 5: Free Variables

As a precursor to substitution, you are to implement a function which returns a
list of free variable names in a given lambda calculus expression:

```ocaml
val free_vars : expr -> string list
```

E.g.,

```ocaml
# free_vars (Var "x");;
- : string list = ["x"]

# free_vars (App (Var "x", Var "y"));;
- : string list = ["x"; "y"]

# free_vars (Abs ("x", App (Var "x", Var "y")));;
- : string list = ["y"]
```

### Exercise 6: Substitution

In class, we implemented the following substitution function:

```ocaml
let rec subst v x t = match t with
  | Var y -> if x = y then v else t
  | App (t1, t2) -> App (subst v x t1, subst v x t2)
  | Abs (y, body) -> if x = y then t
                     else Abs (y, subst v x body)
```

While the first two patterns are handled correctly, the third can lead to
*variable capture*. Consider:

```ocaml
# subst (Var "y") "x" (Abs ("y", Var "x"));;
- : expr = Abs ("y", Var "y")
```

You should fix `subst` so that:

1. It detects when variable capture would occur. This requires checking the
   bound variable against all free variables in the replacement expr.
2. If variable capture would occur, it comes up with a *fresh* variable name
   (i.e., one that won't conflict with the aforementioned free variables) and
   uses it to rename the bound variable and all its occurrences.

A properly working `subst` would work as follows:

```ocaml
# subst (Var "y") "x" (Abs ("y", Var "x"));;
- : expr = Abs ("y1", Var "y")
```

A more interesting example is:

```ocaml
# subst (Var "y") "x" (Abs ("y", (App (Var "y", Var "x"))));;
- : expr = Abs ("y1", App (Var "y1", Var "y"))
```

We provide you with the function `fresh_like`, which takes a variable name *x*
and a list of variables in use, and returns a new variable name starting with
*x* but with a unique integer value appended to it. You should use `fresh_like`
to come up with fresh variable names in your implementation.

E.g.,

```ocaml
# fresh_like "y" ["x"; "y"; "y1"];;
- : string = "y2"
```

### Exercise 7: Applicative-Order Evaluation

In class we completed the implementation of the `step_normal` and `eval_normal`
functions, which effectively implement single-step and multi-step normal-order
evaluation of lambda calculus expressions. For this exercise you are to
implement `step_applicative` and `eval_applicative`, which implement single-step
and multi-step applicative-order evaluation.

```ocaml
val step_applicative : expr -> expr option

val eval_applicative : expr -> expr
```

As with the `step_normal` covered in class (which is included in "lib/lc.ml"),
`step_applicative` takes an expression `e` and returns `Some e'` if there is a
redex, otherwise it returns `None`. `eval_applicative` should take an expression
`e` and perform `step_applicative` on it until there are no more redexes,
returning the final expression `e'`.

Recall that applicative-order evaluates arguments (from left to right) *before*
passing them to functions. We provide sample expressions in `t_lazy` and
`t_eager` that you can use to check your implementation.

Example evaluation order:

```
# eval_normal t_lazy;;
(λx.y) ((λn.λf.λx.f (n f x)) (λf.λx.f (f x)))
y

# eval_applicative t_lazy;;
(λx.y) ((λn.λf.λx.f (n f x)) (λf.λx.f (f x)))
(λx.y) (λf.λx.f ((λf.λx.f (f x)) f x))
y

# eval_normal t_eager;;
(λx.x x) ((λx.x) (λx.x))
(λx.x) (λx.x) ((λx.x) (λx.x))
(λx.x) ((λx.x) (λx.x))
(λx.x) (λx.x)
λx.x

# eval_applicative t_eager;;
(λx.x x) ((λx.x) (λx.x))
(λx.x x) (λx.x)
(λx.x) (λx.x)
λx.x
```

## Score Breakdown

Part 1:

- 6 points: E1. Pretty Printing
- 6 points: E2. Symbolic Differentiation
- 8 points: E3. Simplification
- 8 points: E4. Map and Fold

Part 2:

- 8 points: E5. Free Variables
- 12 points: E6. Substitution
- 12 points: E7. Applicative-Order Evaluation

Total: 60 points

## Testing

We've included tests in "test/test_arith.ml" and "test/test_lc.ml". To run all
tests, do `dune runtest`. Note that we may use different test cases when
evaluating your work after submission.

## Submission

Be sure to complete your self-report in REPORT.md.

Next, commit all your changes and push them to our shared private GitHub
repository. Remember that you can check if your submission was successful by
simply viewing your GitHub repository page.
