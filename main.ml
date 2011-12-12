let debug = false
let trunc = ref true
let horner = ref false
let emitS = ref false
let emitL = ref false
let emitM = ref false
let emitB = ref false

let output first s =
  if !first then Printf.printf "%s" s
  else Printf.printf "\n%s" s;
  first := false

let lexbuf l =
  let first = ref true in
  let (e, assum) = (Parser.prog Lexer.token l) in
  if debug then output first ((Syntax.string_of_t e) ^ "\n");
  let p = Sigma.f !trunc assum e in
  let p' = if !horner then Emit.f p else Emit.g p in
  if !emitS then output first ((Sigma.string_of_prog p) ^ "\n");
  if !emitL then output first (EmitL.string_of_prog p');
  if !emitM then output first (EmitM.string_of_prog p');
  if !emitB then output first (EmitB.string_of_prog p')

let string s = lexbuf (Lexing.from_string s)

let _ =
  Arg.parse
    [("-t", Arg.Unit(fun () -> trunc := false), "without simplify");
     ("-h", Arg.Unit(fun () -> horner := true), "with Horner");
     ("-s", Arg.Unit(fun () -> emitS := true), "sigma (default)");
     ("-l", Arg.Unit(fun () -> emitL := true), "long");
     ("-m", Arg.Unit(fun () -> emitM := true), "mod");
     ("-b", Arg.Unit(fun () -> emitB := true), "BigInteger")]
    (fun s -> ())
    ("Sigma Calculator (C) wata\n" ^
     Printf.sprintf "usage: %s [-t] [-h] [-s] [-l] [-m] [-b] < input > output" Sys.argv.(0));
  if not (!emitS || !emitL || !emitM || !emitB) then emitS := true;
  lexbuf (Lexing.from_channel stdin)
