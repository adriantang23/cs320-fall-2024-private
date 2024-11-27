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
  | top_lets = toplet+ EOF { top_lets }

toplet:
  | LET REC x = VAR args = arg_list_opt ty = ty_opt EQ e = expr
    { 
      { is_rec = true; name = x; args; ty; value = e }
    }
  | LET x = VAR args = arg_list_opt ty = ty_opt EQ e = expr
    {
      { is_rec = false; name = x; args; ty; value = e }
    }

arg_list_opt:
  | { [] }
  | args = arg+ { args }

ty_opt:
  | { UnitTy }  (* Default to `UnitTy` if no type is provided *)
  | COLON ty = ty { ty }

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
  | FUN arg = arg args = arg_list_opt ARROW e = expr
    { SFun { arg; args; body = e } }
  | LET REC x = VAR args = arg_list_opt ty = ty_opt EQ e1 = expr IN e2 = expr
    { SLet { is_rec = true; name = x; args; ty; value = e1; body = e2 } }
  | LET x = VAR args = arg_list_opt ty = ty_opt EQ e1 = expr IN e2 = expr
    { SLet { is_rec = false; name = x; args; ty; value = e1; body = e2 } }
  | e = expr1 { e }

expr1:
  | e1 = expr1 OR e2 = expr2 { SBop(Or, e1, e2) }
  | e1 = expr1 AND e2 = expr2 { SBop(And, e1, e2) }
  | e = expr2 { e }

expr2:
  | ASSERT e = expr3 { SAssert e }
  | e1 = expr3 es = expr3* { List.fold_left (fun e1 e2 -> SApp(e1, e2)) e1 es }
  | e1 = expr2 op = bop e2 = expr2 { SBop(op, e1, e2) }

expr3:
  | LPAREN e = expr RPAREN { e }
  | x = VAR { SVar x }
  | n = NUM { SNum n }
  | FALSE { SFalse }
  | TRUE { STrue }
  | UNIT { SUnit }

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
