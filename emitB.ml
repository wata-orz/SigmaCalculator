open Util
open Emit

(* BigInteger *)

let pow x n =
  if n = 1 then x
  else Format.sprintf "%s.pow(%d)" x n

let rec string_of_term fs =
  let ss = List.map
    (function
      | Var(x, n) -> pow x n
      | Exp(e) -> Format.sprintf "%s" (string_of_exp e)
    ) fs in
  List.fold_left
    (fun s t ->
      Format.sprintf "%s.multiply(%s)" s t
    ) (List.hd ss) (next ss)

and string_of_exp = function
  | [] -> "ZERO"
  | (t, n) :: ts ->
      let s =
        if t = [] && n = 1 then "ONE"
        else if t = [] then Format.sprintf "valueOf(%d)" n
        else if n = 1 then string_of_term t
        else if n = -1 then Format.sprintf "%s.negate()" (string_of_term t)
        else Format.sprintf "valueOf(%d).multiply(%s)" n (string_of_term t) in
      let ss = List.map
        (fun (t, n) ->
          if t = [] && n = 1 then ".add(ONE)"
	        else if t = [] && n > 0 then Format.sprintf ".add(valueOf(%d))" n
          else if t = [] && n = -1 then ".subtract(ONE)"
	        else if t = [] then Format.sprintf ".subtract(valueOf(%d))" (-n)
	        else if n = 1 then Format.sprintf ".add(%s)" (string_of_term t)
	        else if n = -1 then Format.sprintf ".subtract(%s)" (string_of_term t)
          else if n > 0 then Format.sprintf ".add(valueOf(%d).multiply(%s))" n (string_of_term t)
	        else Format.sprintf ".subtract(valueOf(%d).multiply(%s))" (-n) (string_of_term t)
        ) ts in
      s ^ (String.concat "" ss)

let string_of_expd (e, d) =
  if d = 1 then
    string_of_exp e
  else
    Format.sprintf "%s.divide(valueOf(%d))" (string_of_exp e) d

let string_of_conds cs =
  let ss = List.map
    (function
      | CLE(e1, e2) -> Format.sprintf "%s.compareTo(%s) <= 0" (string_of_exp e1) (string_of_exp e2)
      | CL(e1, e2) -> Format.sprintf "%s.compareTo(%s) < 0" (string_of_exp e1) (string_of_exp e2)
      | CMod(e, m) -> Format.sprintf "%s.mod(valueOf(%d)).signum() == 0" (string_of_exp e) m
    ) cs in
  String.concat " && " ss

let string_of_prog p =
  match p with
    | [] -> Format.sprintf "return ZERO;\n"
    | [[], e] -> Format.sprintf "return %s;\n" (string_of_expd e)
    | es ->
        let (s, es) =
          if List.mem_assoc [] es then
            let e = List.assoc [] es in
            (Format.sprintf "BigInteger res = %s;\n" (string_of_expd e), remove [] es)
          else
            (Format.sprintf "BigInteger res = ZERO;\n", es) in
        let ss = List.map
          (fun (cs, e) ->
            Format.sprintf "if (%s) res = res.add(%s);\n" (string_of_conds cs) (string_of_expd e)
          ) es in
        Format.sprintf "%s%sreturn res;\n" s (String.concat "" ss)
