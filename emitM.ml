open Util
open Emit

(* mod *)

let pow x n =
  if n = 1 then x
  else Format.sprintf "pow(%s, %d)" x n

let rec string_of_term fs =
  let ss = List.map
    (function
      | Var(x, n) -> pow x n
      | Exp(e) -> Format.sprintf "%s" (string_of_exp' e)
    ) fs in
  List.fold_left
    (fun s t ->
      Format.sprintf "mod(%s * %s)" s t
    ) (List.hd ss) (next ss)

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
  match e with
    | [t, 1] | [t, -1] -> string_of_exp e
	  | _ -> Format.sprintf "mod(%s)" (string_of_exp e)

let string_of_expd (e, d) =
  if d = 1 then
    string_of_exp e
  else
    Format.sprintf "%s * inv(%d)" (string_of_exp' e) d

let string_of_expd' (e, d) =
  if d = 1 then
    Format.sprintf "mod(%s)" (string_of_exp e)
  else
    Format.sprintf "mod(%s * inv(%d))" (string_of_exp' e) d

let string_of_conds cs =
  let ss = List.map
    (function
      | CLE(e1, e2) -> Format.sprintf "%s <= %s" (EmitL.string_of_exp e1) (EmitL.string_of_exp e2)
      | CL(e1, e2) -> Format.sprintf "%s < %s" (EmitL.string_of_exp e1) (EmitL.string_of_exp e2)
      | CMod(e, m) -> Format.sprintf "%s %% %d == 0" (EmitL.string_of_exp' e) m
    ) cs in
  String.concat " && " ss

let string_of_prog p =
  match p with
    | [] -> Format.sprintf "return 0;\n"
    | [[], e] -> Format.sprintf "return %s;\n" (string_of_expd' e)
    | es ->
        let (s, es) =
          if List.mem_assoc [] es then
            let e = List.assoc [] es in
            (Format.sprintf "long res = %s;\n" (string_of_expd' e), remove [] es)
          else
            (Format.sprintf "long res = 0;\n", es) in
        let ss = List.map
          (fun (cs, e) ->
            Format.sprintf "if (%s) res = mod(res + %s);\n" (string_of_conds cs) (string_of_expd e)
          ) es in
        Format.sprintf "%s%sreturn res;\n" s (String.concat "" ss)
