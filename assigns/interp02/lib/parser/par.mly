%{
open Utils
%}

%token <string> VAR
%token <int> NUM
%token EOF
%token FUN LET REC IN IF THEN ELSE ASSERT TRUE FALSE UNIT LPAREN RPAREN
%token INTTY BOOLTY UNITTY COLON ARROW
%token ADD SUB MUL DIV MOD 
%token LT LTE GT GTE EQ NEQ AND OR

%right ARROW
%right OR
%right AND
%left LT LTE GT GTE EQ NEQ
%left ADD SUB
%left MUL DIV MOD

%start <Utils.prog> prog

%%

prog:
  | ls = toplet* EOF { ls }

toplet:
  | LET REC x = VAR arg = arg ; args = arg* COLON ty = ty EQ e = expr
  { { is_rec = true; name = x; args = arg :: args; ty; value = e } }
  | LET x = VAR args = arg* COLON ty = ty EQ e = expr
  { { is_rec = false; name = x; args = args; ty; value = e } }

arg:
  | LPAREN x = VAR COLON ty = ty RPAREN { (x, ty) }

ty:
  | LPAREN t = ty RPAREN { t }
  | t1 = ty ARROW t2 = ty { FunTy(t1, t2) }
  | UNITTY { UnitTy }
  | BOOLTY { BoolTy }
  | INTTY { IntTy }

expr:
  | IF e1 = expr THEN e2 = expr ELSE e3 = expr
  { SIf(e1, e2, e3) }
  | FUN arg = arg ; args = arg* ARROW e = expr
  { SFun { arg = arg; args = args; body = e } }
  | LET REC x = VAR arg = arg ; args = arg* COLON ty = ty EQ e1 = expr IN e2 = expr
  { SLet { is_rec = true; name = x; args = arg :: args; ty; value = e1; body = e2 } }
  | LET x = VAR args = arg* COLON ty = ty EQ e1 = expr IN e2 = expr
  { SLet { is_rec = false; name = x; args = args; ty; value = e1; body = e2 } }
  | e = expr2 { e }

expr2:
  | ASSERT e = expr3 { SAssert e }
  | e1 = expr3 es = expr3* { List.fold_left (fun e1 e2 -> SApp(e1, e2)) e1 es }
  | e1 = expr2; op = bop; e2 = expr2 { SBop (op, e1, e2) }

expr3:
  | LPAREN e = expr RPAREN { e }
  | x = VAR { SVar x }
  | n = NUM { SNum n }
  | FALSE { SFalse }
  | TRUE { STrue }
  | UNIT { SUnit }

%inline bop:
  | AND { And }
  | OR { Or }
  | EQ { Eq }
  | NEQ { Neq }
  | LT { Lt }
  | LTE { Lte }
  | GT { Gt }
  | GTE { Gte }
  | ADD { Add }
  | SUB { Sub }
  | MUL { Mul }
  | DIV { Div }
  | MOD { Mod }