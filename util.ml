let rec gcd a b =
  if b = 0 then abs a
  else gcd b (a mod b)

let rec exgcd' x y a0 b0 a1 b1 =
	if y = 0 then
		if x < 0 then (-a0, -b0, -x)
		else (a0, b0, x)
	else exgcd' y (x mod y) a1 b1 (a0 - x / y * a1) (b0 - x / y * b1)

let exgcd x y =
  exgcd' x y 1 0 0 1

let modulo x y =
  let z = x mod y in
  if z < 0 then z + y
  else z

let fdiv x y =
  let x' = x / y in
  if x' * y > x then x' - 1
  else x'

let cdiv x y =
  let x' = x / y in
  if x' * y < x then x' + 1
  else x'

let rm x xs =
  List.filter (fun x' -> x' <> x) xs

let get k kvs =
  if List.mem_assoc k kvs then List.assoc k kvs
  else 0

let remove k kvs =
  List.remove_assoc k kvs

let map_assoc f kvs =
  List.map (fun (k, v) -> (k, f v)) kvs

let mul_assoc n kvs =
  map_assoc (fun v -> assert (v * n / n = v); v * n) kvs

let sort ls =
  List.sort compare ls

let normal add zero kvs =
  List.fold_left
    (fun kvs (k, v) ->
      match kvs with
      | (k', v') :: kvs when k' = k ->
          let u = add v' v in
          if u = zero then kvs
          else (k', u) :: kvs
      | kvs when v = zero -> kvs
      | kvs -> (k, v) :: kvs
    ) [] (List.rev (sort kvs))

let next ls =
  if ls = [] then []
  else List.tl ls

let fold f a ls =
  let (a, _) = List.fold_left (fun (a, i) b -> (f a b i, i + 1)) (a, 0) ls in
  a

let loop f a n =
  let rec loop' f n =
    if n < 0 then a
    else f (loop' f (n - 1)) n
  in loop' f (n - 1)

let cross f vs us =
  List.fold_left
    (fun ts v ->
      List.fold_left
        (fun ts u ->
          f v u :: ts
        ) ts us
    ) [] vs
