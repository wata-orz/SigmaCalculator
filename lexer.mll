{
open Parser
}
let space = [' ' '\t' '\r' '\n']
let integer = ['0'-'9']+
let ident = ['a'-'z' 'A'-'Z'] ['0'-'9']*

rule token = parse
  | space+        { token lexbuf }
  | '('           { LPAREN }
  | ')'           { RPAREN }
  | '['           { LBRACKET }
  | ']'           { RBRACKET }
  | '$'           { SUM }
  | integer as i  { CONST(int_of_string i) }
  | '+'           { PLUS }
  | '-'           { MINUS }
  | '*'           { MUL }
  | '/'           { DIV }
  | '^'           { POW }
  | "<="          { LESS_EQUAL }
  | ">="          { GREATER_EQUAL }
  | '<'           { LESS }
  | '>'           { GREATER }
  | '='           { EQUAL }
  | '|'           { BAR }
  | ','           { COMMA }
  | "&&"          { AND }
  | "||"          { OR }
  | '@'           { AT }
  | ident as s    { IDENT(s) }
  | eof           { EOF }
  | _             { failwith "Unknown token" }
