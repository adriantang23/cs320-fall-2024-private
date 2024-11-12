%{
open Utils

let rec make e ex =
  match ex with
  | [] -> e
  | x :: ex -> make (App (e, x)) ex
%}

%token <int> NUM
%token <string> VAR
%token IF THEN ELSE LET IN FUN ARROW
%token ADD SUB MUL DIV MOD LT LTE GT GTE EQ NEQ AND OR
%token TRUE FALSE
%token LPAREN RPAREN EOF

%left OR
%left AND
%left LT LTE GT GTE EQ NEQ
%left ADD SUB
%left MUL DIV MOD

%start <Utils.prog> prog

%%

prog:
  | e = expr EOF { e }

expr:
  | IF cond=expr THEN thn=expr ELSE els=expr { If (cond,thn,els) }
  | LET var=VAR EQ value=expr IN body=expr { Let (var,value,body) }
  | FUN param=VAR ARROW body=expr { Fun (param,body) }
  | e=expr2 { e }

expr2:
  | left=expr2 operator=bop right=expr2 { Bop (operator,left,right) }
  | e=expr3 args=expr3_list { make e args }
  | e=expr3 { e }

expr3_list:
  | h=expr3 t=expr3_list { h :: t }
  | { [] }

expr3:
  | LPAREN RPAREN { Unit }
  | TRUE { True }
  | FALSE { False }
  | num=NUM { Num num }
  | var=VAR { Var var }
  | LPAREN inner=expr RPAREN {inner}

%inline bop:
  | ADD { Add }
  | SUB { Sub }
  | MUL { Mul }
  | DIV { Div }
  | MOD { Mod }
  | LT { Lt }
  | LTE { Lte }
  | GT { Gt }
  | GTE { Gte }
  | EQ { Eq }
  | NEQ { Neq }
  | AND { And }
  | OR { Or }

