%{
open Utils
%}

%token <string> VAR
%token <int> NUM
%token EOF
%token FUN LET REC IN IF THEN ELSE ASSERT TRUE FALSE UNIT LPAREN RPAREN
%token INTTY BOOLTY UNITTY COLON ARROW
%token ADD SUB MUL DIV MOD LT LTE GT GTE EQ NEQ AND OR

%right ARROW
%right OR
%right AND
%left LT LTE GT GTE EQ NEQ
%left ADD SUB
%left MUL DIV MOD

%start <Utils.prog> prog

%%

prog:
  | toplet_list = toplet* EOF { toplet_list }

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
  | ASSERT e_assert = expr3 { SAssert e_assert }
  | e_first = expr3 e_list = expr3* { List.fold_left (fun acc e -> SApp(acc, e)) e_first e_list }
  | e_left = expr2 op = bop e_right = expr2 { SBop (op, e_left, e_right) }

expr3:
  | LPAREN e_nest = expr RPAREN { e_nest }
  | var_name = VAR { SVar var_name }
  | num_value = NUM { SNum num_value }
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
