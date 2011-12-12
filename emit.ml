open Util
open Sigma

type exp = (term * int) list
and term = factor list
and factor = Var of string * int | Exp of exp
type cond = CLE of exp * exp | CL of exp * exp | CMod of exp * int
type prog = (cond list * (exp * int)) list

let mul e1 e2 =
  match e1, e2 with
    | [t1, n1], [t2, n2] -> [t1 @ t2, n1 * n2]
    | _ -> [[Exp(e1); Exp(e2)], 1]

let get_var ts =
	let v' = List.fold_left
	(fun v' (fs, _) ->
	  List.fold_left
	    (fun v' (f, _) ->
	      match v' with
	        | None -> Some(f)
	        | Some(v) when v > f -> Some(f)
	        | _ -> v'
	    ) v' fs
	) None ts in
  match v' with
    | None -> assert false
    | Some(v) -> v

let f_vs vs =
  List.map
    (fun (v, n) -> match v with
      | EPS -> assert false
      | C -> ([], n)
      | ID(id) -> ([Var(id, 1)], n)
    ) vs

let f_cond c =
	if c.m = 0 then
    let pos, neg = List.partition (fun (_, n) -> n > 0) c.vs in
    if (get EPS pos) > 0 then
		  CL(f_vs (remove EPS pos), f_vs (mul_assoc (-1) neg))
		else
		  CLE(f_vs pos, f_vs (mul_assoc (-1) neg));
	else
	  CMod(f_vs c.vs, c.m)

let rec f_terms' x n = function
  | [] -> []
  | (n', ts) :: tss when n' = n ->
      (f_terms ts) @ (f_terms' x n' tss)
  | (n', ts) :: tss ->
      mul [[Var(x, n' - n)], 1] ((f_terms ts) @ (f_terms' x n' tss))

and f_terms = function
  | [] -> assert false
  | [[], n] -> [[], n]
  | ts ->
      let x = get_var ts in
      let tss = List.map
        (fun (fs, n) ->
          (get x fs, [remove x fs, n])
        ) ts in
      let tss = normal (@) [] tss in
	    f_terms' x 0 tss

let f (Prog(es)) =
  List.map
    (fun (Conds(cs), e) ->
      let cs = List.map f_cond cs in
      let ts = List.map (function (Term(fs), n) -> (fs, n)) e.ts in
      (cs, (f_terms ts, e.d))
    ) es

let g (Prog(es)) =
  List.map
    (fun (Conds(cs), e) ->
      let cs = List.map f_cond cs in
      let e' = List.map
        (function (Term(fs), n) ->
          (List.map (fun (v, n) -> Var(v, n)) fs, n)
        ) e.ts in
      (cs, (e', e.d))
    ) es
