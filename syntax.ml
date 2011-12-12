type t =
  | Const of int
  | Var of string
  | Add of t * t
  | Mul of t * t
  | Div of t * int
  | Pow of t * int
  | CLE of t
  | CL of t
  | CMod of int * t
  | Sum of string * t

let neg a = Mul(Const(-1), a)
let sub a b = Add(a, neg b)
let cle a b = CLE(sub a b)
let cge a b = cle b a
let cl a b = CL(sub a b)
let cg a b = cl b a
let ceq a b = Mul(cle a b, cle b a)
let cor a b = sub (Add(a, b)) (Mul(a, b))

let rec string_of_t = function
  | Const(c) -> string_of_int c
  | Var(id) -> id
  | Add(e1, e2) -> Format.sprintf "(%s + %s)" (string_of_t e1) (string_of_t e2)
  | Mul(e1, e2) -> Format.sprintf "(%s %s)" (string_of_t e1) (string_of_t e2)
  | Div(e, n) -> Format.sprintf "%s / %d" (string_of_t e) n
  | Pow(e, n) -> Format.sprintf "%s^%d" (string_of_t e) n
  | CLE(e) -> Format.sprintf "[%s <= 0]" (string_of_t e)
  | CL(e) -> Format.sprintf "[%s < 0]" (string_of_t e)
  | CMod(m, e) -> Format.sprintf "[%d | %s]" m (string_of_t e)
  | Sum(id, e) -> Format.sprintf "($%s %s)" id (string_of_t e)
