%{
open Syntax
open Util
let muls = function
  | hd :: tl -> List.fold_left (fun a b -> Mul(a, b)) hd tl
  | _ -> assert false
%}

%token LPAREN
%token RPAREN
%token LBRACKET
%token RBRACKET
%token SUM
%token PLUS
%token MINUS
%token MUL
%token DIV
%token POW
%token LESS_EQUAL
%token GREATER_EQUAL
%token LESS
%token GREATER
%token EQUAL
%token BAR
%token COMMA
%token AND
%token OR
%token AT
%token <string> IDENT
%token <int> CONST
%token EOF

%left OR
%left AND
%left LESS GREATER EQUAL LESS_EQUAL GREATER_EQUAL BAR
%left PLUS MINUS
%nonassoc prec_sum
%nonassoc LPAREN RPAREN LBRACKET RBRACKET SUM IDENT CONST COMMA AT
%left MUL DIV
%nonassoc prec_unary
%nonassoc POW

%type <Syntax.t * Syntax.t list> prog
%start prog

%%

prog:
  | exp                            { ($1, []) }
  | exp AT assum                   { ($1, $3) }

exp:
  | PLUS exp %prec prec_unary      { $2 }
  | MINUS exp %prec prec_unary     { neg $2 }
  | exp exp2 %prec MUL             { Mul($1, $2) }
  | LPAREN exp RPAREN              { $2 }
  | LBRACKET cond RBRACKET         { $2 }
  | SUM IDENT exp %prec prec_sum   { Sum($2, $3) }
  | exp PLUS exp                   { Add($1, $3) }
  | exp MINUS exp                  { sub $1 $3 }
  | exp MUL exp                    { Mul($1, $3) }
  | exp DIV CONST                  { Div($1, $3) }
  | exp POW CONST                  { Pow($1, $3) }
  | CONST                          { Const($1) }
  | IDENT                          { Var($1) }

exp2:
  | exp2 exp2 %prec MUL            { Mul($1, $2) }
  | LPAREN exp RPAREN              { $2 }
  | LBRACKET cond RBRACKET         { $2 }
  | SUM IDENT exp %prec prec_sum   { Sum($2, $3) }
  | exp2 MUL exp                   { Mul($1, $3) }
  | exp2 POW CONST                 { Pow($1, $3) }
  | CONST                          { Const($1) }
  | IDENT                          { Var($1) }

cond:
  | LPAREN cond RPAREN             { $2 }
  | conds                          { muls (snd $1) }
  | CONST BAR exp                  { CMod($1, $3) }
  | cond AND cond                  { Mul($1, $3) }
  | cond OR cond                   { cor $1 $3 }

conds:
  | exps EQUAL conds2              { ($1, cross ceq $1 (fst $3) @ snd $3) }
  | exps LESS conds2               { ($1, cross cl $1 (fst $3) @ snd $3) }
  | exps GREATER conds2            { ($1, cross cg $1 (fst $3) @ snd $3) }
  | exps LESS_EQUAL conds2         { ($1, cross cle $1 (fst $3) @ snd $3) }
  | exps GREATER_EQUAL conds2      { ($1, cross cge $1 (fst $3) @ snd $3) }

conds2:
  | exps                           { ($1, []) }
  | exps EQUAL conds2              { ($1, cross ceq $1 (fst $3) @ snd $3) }
  | exps LESS conds2               { ($1, cross cl $1 (fst $3) @ snd $3) }
  | exps GREATER conds2            { ($1, cross cg $1 (fst $3) @ snd $3) }
  | exps LESS_EQUAL conds2         { ($1, cross cle $1 (fst $3) @ snd $3) }
  | exps GREATER_EQUAL conds2      { ($1, cross cge $1 (fst $3) @ snd $3) }

exps:
  | exp                            { [$1] }
  | exp COMMA exps                 { $1 :: $3 }

assum:
  | conds                          { (snd $1) }
  | CONST BAR exp                  { [CMod($1, $3)] }
  | assum AND assum                { $1 @ $3 }
