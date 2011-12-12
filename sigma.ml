open Util
open Syntax

let poly_table = [|
[|1; 1|];
[|2; 1; 1|];
[|6; 1; 3; 2|];
[|4; 0; 1; 2; 1|];
[|30; -1; 0; 10; 15; 6|];
[|12; 0; -1; 0; 5; 6; 2|];
[|42; 1; 0; -7; 0; 21; 21; 6|];
[|24; 0; 2; 0; -7; 0; 14; 12; 3|];
[|90; -3; 0; 20; 0; -42; 0; 60; 45; 10|];
[|20; 0; -3; 0; 10; 0; -14; 0; 15; 10; 2|];
[|66; 5; 0; -33; 0; 66; 0; -66; 0; 55; 33; 6|];
[|24; 0; 10; 0; -33; 0; 44; 0; -33; 0; 22; 12; 2|];
[|2730; -691; 0; 4550; 0; -9009; 0; 8580; 0; -5005; 0; 2730; 1365; 210|];
[|420; 0; -691; 0; 2275; 0; -3003; 0; 2145; 0; -1001; 0; 455; 210; 30|];
[|90; 105; 0; -691; 0; 1365; 0; -1287; 0; 715; 0; -273; 0; 105; 45; 6|];
[|48; 0; 420; 0; -1382; 0; 1820; 0; -1287; 0; 572; 0; -182; 0; 60; 24; 3|];
|]

type var = EPS | C | ID of string
type cond = { m : int; vs : (var * int) list }
type conds = Conds of cond list
type term = Term of (string * int) list
type exp = { ts : (term * int) list; d : int }
type prog = Prog of (conds * exp) list

let czero = { m = 0; vs = [C, 1] }
let cone = { m = 0; vs = [] }
let csone = Conds([])
let cszero = Conds([czero])
let tone = Term([])
let ezero = { ts = []; d = 1 }
let eone = { ts = [tone, 1]; d = 1 }
let pzero = Prog([])
let pone = Prog([csone, eone])

let string_of_vs vs =
  if vs = [] then "0"
  else
		let ss = List.map
	    (fun (v, n) -> match v with
        | EPS -> assert false
        | C -> Format.sprintf "%d" n
        | ID(id) when n = 1 -> id
        | ID(id) -> Format.sprintf "%d%s" n id
	    ) vs in
	  String.concat "+" ss

let string_of_cond c =
  if c.m = 0 then
    let pos, neg = List.partition (fun (_, n) -> n > 0) c.vs in
    if (get EPS pos) > 0 then
      Format.sprintf "[%s<%s]" (string_of_vs (remove EPS pos)) (string_of_vs (mul_assoc (-1) neg))
    else
      Format.sprintf "[%s<=%s]" (string_of_vs pos) (string_of_vs (mul_assoc (-1) neg))
  else
    Format.sprintf "[%d|%s]" c.m (string_of_vs c.vs)

let string_of_conds (Conds(cs)) =
  String.concat "" (List.map string_of_cond cs)

let string_of_term (Term(fs)) =
  let fs = List.map
    (fun (f, n) ->
      if n = 1 then f
      else Format.sprintf "%s^%d" f n
    ) fs in
  String.concat "" fs

let string_of_exp e =
  let ts = List.map
    (fun (t, n) ->
      if t = tone then Format.sprintf "%+d" n
      else if n = 1 then Format.sprintf "+%s" (string_of_term t)
      else if n = -1 then Format.sprintf "-%s" (string_of_term t)
      else Format.sprintf "%+d%s" n (string_of_term t)
    ) e.ts in
  if e.d = 1 then Format.sprintf "(%s)" (String.concat "" ts)
  else Format.sprintf "(%s)/%d" (String.concat "" ts) e.d

let string_of_prog (Prog(es)) =
  if es = [] then "0"
  else
	  let es = List.map
	    (fun (cs, e) ->
	      Format.sprintf "%s%s" (string_of_conds cs) (string_of_exp e)
	    ) es in
	  String.concat "\n+" es

let size (Prog(es)) =
  List.length es

let cond(m, vs) =
  let m = abs m in
  let vs = normal (+) 0 vs in
  let eps = get EPS vs in
  let vs =
    if m = 0 then remove EPS vs
    else normal (+) 0 (map_assoc (fun n -> modulo n m) vs) in
  let d = List.fold_left (fun d (v, n) -> gcd d n) m (remove C vs) in
  if m > 0 && modulo (get C vs) d <> 0 then czero
  else
    let d = gcd d (get C vs) in
    let d = if d = 0 then 1 else d in
	  let vs = map_assoc (fun n -> n / d) vs in
	  let m = m / d in
    if m = 0 then
      if (remove C vs) <> [] then { m = 0; vs = if eps > 0 then (EPS, 1) :: vs else vs }
      else if (get C vs) + (if eps > 0 then 1 else 0) <= 0 then cone
      else czero
	  else if (remove C vs) <> [] && m > 1 then
      let vs = loop
        (fun us i ->
          if gcd i m = 1 then
            let ws = map_assoc (fun n -> modulo (n * i) m) vs in
            if (remove C ws) < (remove C us) then ws
            else us
          else us
        ) vs m in
      { m = m; vs = vs }
	  else if (get C vs) = 0 || m = 1 then cone
	  else czero

let trunc c =
  if c.m > 0 then c
  else
    let vs =
      if (get EPS c.vs) > 0 then normal (+) 0 ((C, 1) :: (remove EPS c.vs))
      else remove EPS c.vs in
	  let d = List.fold_left (fun d (v, n) -> gcd d n) 0 (remove C vs) in
    let d = if d = 0 then 1 else d in
    { m = 0; vs = normal (+) 0 (map_assoc (fun n -> cdiv n d) vs) }

let conds(cs) =
  let cs = List.fold_left
    (fun cs c ->
      let c1 = trunc c in
      if c1 = czero then [czero]
      else if c1 = cone then cs
      else if c1.m = 0 && List.exists
        (fun c' -> c'.m = 0 && let c2 = trunc c' in
          (remove C c2.vs) = mul_assoc (-1) (remove C c1.vs) && (get C c2.vs) + (get C c1.vs) > 0
        ) cs then [czero]
      else
        match (List.partition
          (fun c' -> let c2 = trunc c' in (remove C c2.vs) = (remove C c1.vs) && c1.m = c2.m)) cs with
	        | [], cs -> c :: cs
	        | [c'], cs ->
	            if c'.m > 0 then
                if (get C c1.vs) = (get C c'.vs) then c :: cs
                else [czero]
	            else
	              if (get C c1.vs) > (get C (trunc c').vs) then c :: cs
	              else c' :: cs
	        | _ -> assert false
    ) [] cs in
  if List.mem czero cs then cszero
  else Conds(sort cs)

let term(fs) =
  Term(normal (+) 0 fs)

let exp(ts, d) =
  let ts = normal (+) 0 ts in
  let m = List.fold_left (fun m (_, n) -> gcd m n) d ts in
  let m = if d < 0 then -m else m in
  { ts = map_assoc (fun n -> n / m) ts; d = d / m }

let add_exp e1 e2 =
  let m = gcd e1.d e2.d in
  exp(mul_assoc (e2.d / m) e1.ts @ mul_assoc (e1.d / m) e2.ts, e1.d / m * e2.d)

let sub_exp e1 e2 =
  add_exp e1 (exp(e2.ts, -e2.d))

let mul_exp e1 e2 =
  let ts = cross
    (fun (Term(t1), n1) (Term(t2), n2) ->
      (term(t1 @ t2), n1 * n2)
    ) e1.ts e2.ts in
  exp(ts, e1.d * e2.d)

let rec pow_exp e n =
  if n = 0 then eone
  else mul_exp e (pow_exp e (n - 1))

let prog(es) =
  Prog(remove cszero (normal add_exp ezero es))

let add_prog (Prog(es1)) (Prog(es2)) =
  prog(es1 @ es2)

let mul_prog (Prog(es1)) (Prog(es2)) =
  let es = cross
    (fun (Conds(cs1), e1) (Conds(cs2), e2) ->
      (conds(cs1 @ cs2), mul_exp e1 e2)
    ) es1 es2 in
  prog(es)

let rec pow_prog p n =
  if n = 0 then pone
  else mul_prog p (pow_prog p (n - 1))

let linear e =
	List.fold_left
    (fun vs -> function
      | (Term([]), n) -> (C, n) :: vs
      | (Term([v, 1]), n) -> (ID(v), n) :: vs
      | _ -> assert false
    ) [] e.ts

let linear_prog = function
  | Prog([]) -> ([], 1)
  | Prog([Conds([]), e]) -> (linear e, e.d)
  | _ -> assert false

let exp1 vs d =
  let ts = List.map
    (fun (v, n) -> match v with
      | EPS -> assert false
      | C -> (tone, n)
      | ID(v) -> (term([v, 1]), n)
    ) vs in
  exp(ts, d)

let assign_cond id vs d c =
  let n = get id c.vs in
  if n = 0 then c
  else
    let (d, n) =
      if d < 0 then (-d, -n)
      else (d, n) in
    let vs = mul_assoc n vs in
    cond(c.m * d, vs @ (mul_assoc d (remove id c.vs)))

let assign_exp id e e' =
  List.fold_left
    (fun e2 (Term(fs), n) ->
      let deg = get id fs in
      let fs = remove id fs in
      let e = pow_exp e deg in
      add_exp e2 (mul_exp (exp([term(fs), n], e'.d)) e)
    ) ezero e'.ts

let sum id mult vs d cs es e =
  loop
    (fun es i ->
      let vs = (C, i) :: vs in
      let e1 = exp1 vs (-mult * d) in
      let cs = conds(cond(d, vs) :: cs) in
      let e' = List.fold_left
        (fun e' (Term(fs), n) ->
          let deg = get id fs in
          let e2 = loop
            (fun e' k ->
              let e2 = exp([tone, poly_table.(deg).(k + 1)], 1) in
              add_exp e' (mul_exp e2 (pow_exp e1 (k + 1)))
            ) ezero (deg + 1) in
          add_exp e' (mul_exp (exp([term(remove id fs), n], mult * e.d * poly_table.(deg).(0))) e2)
        ) ezero e.ts in
      (cs, e') :: es
    ) es d

let rec sum_exp id es (Conds(cs), e) =
  let id' = ID(id) in
  let ass = List.filter
    (fun c -> c.m = 0 && (get id' c.vs) <> 0 && List.mem (cond(0, mul_assoc (-1) c.vs)) cs
    ) cs in
  if ass <> [] then
    let a = List.hd ass in
    let vs = remove id' a.vs in
    let d = -(get id' a.vs) in
    let cs = conds(cond(d, vs) :: List.map (assign_cond id' vs d) cs) in
    (cs, assign_exp id (exp1 vs d) e) :: es
  else match List.filter (fun c -> c.m > 0 && (get id' c.vs) <> 0) cs with
    | mcond :: _ ->
		    let a = get id' mcond.vs in
		    let b = remove id' mcond.vs in
		    let c = mcond.m in
		    let (m, _, d) = exgcd a c in
        let vs = (id', c) :: mul_assoc (-m) b in
        let cs = conds(cond(d, b) :: List.map (assign_cond id' vs d) (rm mcond cs)) in
        if cs = cszero then es
        else sum_exp id es (cs, (assign_exp id (exp1 vs d) e))
    | [] ->
			  let lbs = List.filter (fun cond -> (get id' cond.vs) < 0) cs in
			  let ubs = List.filter (fun cond -> (get id' cond.vs) > 0) cs in
        let cc = List.filter (fun cond -> (get id' cond.vs) = 0) cs in
        assert ((List.length lbs) > 0 && (List.length ubs) > 0);
        fold
          (fun es lb i ->
            let a = -(get id' lb.vs) in
            let cs = fold
              (fun cs lb' i' ->
                let b = -(get id' lb'.vs) in
                if i' < i then
                  cond(0, (EPS, 1) :: (mul_assoc (-b) lb.vs) @ (mul_assoc a lb'.vs)) :: cs
                else if i' > i then
                  cond(0, (mul_assoc (-b) lb.vs) @ (mul_assoc a lb'.vs)) :: cs
                else cs
              ) cc lbs in
		        fold
		          (fun es ub j ->
                let b = get id' ub.vs in
                let cs = fold
                  (fun cs ub' j' ->
		                let a = get id' ub'.vs in
                    if j' < j then
  		                cond(0, (EPS, 1) :: (mul_assoc (-a) ub.vs) @ (mul_assoc b ub'.vs)) :: cs
                    else if j' > j then
  		                cond(0, (mul_assoc (-a) ub.vs) @ (mul_assoc b ub'.vs)) :: cs
                    else cs
                  ) cs ubs in
                let c = cond(0, (mul_assoc b lb.vs) @ (mul_assoc a ub.vs)) in
                let c =
                  if (get EPS lb.vs) > 0 && (get EPS ub.vs) > 0 then c
                  else cond(0, remove EPS c.vs) in
		            let cs = c :: cs in
                let lb = trunc lb in
                let a = -(get id' lb.vs) in
                let es = sum id (-1) ((C, -a) :: remove id' lb.vs) a cs es e in
                let ub = trunc ub in
                let b = get id' ub.vs in
                sum id 1 (remove id' ub.vs) b cs es e
		          ) es ubs
          ) es lbs

let sum_prog id (Prog(es)) =
  let es = List.fold_left (sum_exp id) [] es in
  prog(es)

let trunc_all (Prog(es)) =
  prog(List.map (fun (Conds(cs), e) -> (conds(List.map trunc cs), e)) es)

let rec g = function
  | Const(c) -> prog([csone, exp([tone, c], 1)])
  | Var(id) -> prog([csone, exp([term([id, 1]), 1], 1)])
  | Add(e1, e2) -> add_prog (g e1) (g e2)
  | Mul(e1, e2) -> mul_prog (g e1) (g e2)
  | Div(e, n) -> mul_prog (g e) (prog([(csone, exp([tone, 1], n))]))
  | Pow(e, n) -> pow_prog (g e) n
  | CLE(e) -> prog([conds([cond(0, fst (linear_prog (g e)))]), eone])
  | CL(e) -> prog([conds([cond(0, (EPS, 1) :: fst (linear_prog (g e)))]), eone])
  | CMod(m, e) ->
      assert (m <> 0);
      let (vs, d) = linear_prog (g e) in
      prog([conds([cond(m * d, vs)]), eone])
  | Sum(id, e) ->
      let p = g e in
      Printf.eprintf "Sum@%s\n%d -> " id (size p);
      flush stderr;
      let p = sum_prog id p in
      Printf.eprintf "%d\n" (size p);
      flush stderr;
      p

let h assum (Prog(es)) =
  let cs = List.map
    (function
		  | CLE(e) -> cond(0, fst (linear_prog (g e)))
		  | CL(e) -> cond(0, (EPS, 1) :: fst (linear_prog (g e)))
		  | CMod(m, e) ->
		      assert (m <> 0);
		      let (vs, d) = linear_prog (g e) in
		      cond(m * d, vs)
      | _ -> assert false
    ) assum in
  let cs = List.map trunc cs in
  let es = List.map
    (fun (Conds(cs'), e) ->
      let Conds(cs') = conds(cs' @ cs) in
      let cs' = List.filter (fun c -> not (List.mem c cs)) cs' in
      (conds(cs'), e)
    ) es in
  prog(es)

let f tr assum e =
  let p = g e in
  Printf.eprintf "Finished!\n%d -> " (size p);
  flush stderr;
  let p = if tr then trunc_all p else p in
  Printf.eprintf "%d -> " (size p);
  flush stderr;
  let p = h assum p in
  Printf.eprintf "%d\n" (size p);
  flush stderr;
  p
