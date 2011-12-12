open Util
open Emit

(* long *)

let rec pow x n =
  if n = 1 then x
  else Format.sprintf "%s * %s" (pow x (n - 1)) x

let rec string_of_term fs =
  let ss = List.map
    (function
      | Var(x, n) -> pow x n
      | Exp(e) -> Format.sprintf "%s" (string_of_exp' e)
    ) fs in
  String.concat " * " ss

and string_of_exp = function
  | [] -> "0"
  | (t, n) :: ts ->
      let s =
        if t = [] then Format.sprintf "%d" n
        else if n = 1 then string_of_term t
        else if n = -1 then Format.sprintf "-%s" (string_of_term t)
        else Format.sprintf "%d * %s" n (string_of_term t) in
      let ss = List.map
        (fun (t, n) ->
	        if t = [] && n > 0 then Format.sprintf " + %d" n
	        else if t = [] then Format.sprintf " - %d" (-n)
	        else if n = 1 then Format.sprintf " + %s" (string_of_term t)
	        else if n = -1 then Format.sprintf " - %s" (string_of_term t)
          else if n > 0 then Format.sprintf " + %d * %s" n (string_of_term t)
	        else Format.sprintf " - %d * %s" (-n) (string_of_term t)
        ) ts in
      s ^ (String.concat "" ss)

and string_of_exp' e =
  if (List.length e) <= 1 then string_of_exp e
  else Format.sprintf "(%s)" (string_of_exp e)

let string_of_expd (e, d) =
  if d = 1 then
    string_of_exp e
  else
    Format.sprintf "%s / %d" (string_of_exp' e) d

let string_of_conds cs =
  let ss = List.map
    (function
      | CLE(e1, e2) -> Format.sprintf "%s <= %s" (string_of_exp e1) (string_of_exp e2)
      | CL(e1, e2) -> Format.sprintf "%s < %s" (string_of_exp e1) (string_of_exp e2)
      | CMod(e, m) -> Format.sprintf "%s %% %d == 0" (string_of_exp' e) m
    ) cs in
  String.concat " && " ss

let string_of_prog p =
  match p with
    | [] -> Format.sprintf "return 0;\n"
    | [[], e] -> Format.sprintf "return %s;\n" (string_of_expd e)
    | es ->
        let (s, es) =
          if List.mem_assoc [] es then
            let e = List.assoc [] es in
            (Format.sprintf "long res = %s;\n" (string_of_expd e), remove [] es)
          else
            (Format.sprintf "long res = 0;\n", es) in
        let ss = List.map
          (fun (cs, e) ->
            Format.sprintf "if (%s) res += %s;\n" (string_of_conds cs) (string_of_expd e)
          ) es in
        Format.sprintf "%s%sreturn res;\n" s (String.concat "" ss)
