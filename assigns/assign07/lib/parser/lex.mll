{
open Par
}

let whitespace = [' ' '\t' '\n' '\r']+
let num = '-'? ['0'-'9']+
let var = ['a'-'z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']*

rule read =
  parse
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "let" { LET }
  | "in" { IN }
  | "fun" { FUN }
  | "true" { TRUE }
  | "false" { FALSE }
  | "->" { ARROW }
  | "(" { LPAREN }
  | ")" { RPAREN }

  | "+" { ADD }
  | "-" { SUB }
  | "*" { MUL }
  | "/" { DIV }
  | "mod" { MOD }
  | "<=" { LTE }
  | "<" { LT }
  | ">=" { GTE }
  | ">" { GT }
  | "=" { EQ }
  | "<>" { NEQ }
  | "&&" { AND }
  | "||" { OR }

  | num { NUM (int_of_string (Lexing.lexeme lexbuf)) }
  | var { VAR (Lexing.lexeme lexbuf) }
  | whitespace { read lexbuf }
  | eof { EOF }
