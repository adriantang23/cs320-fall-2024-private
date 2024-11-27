{
open Par
}

let whitespace = [' ' '\t' '\n' '\r']+
let num = '-'? ['0'-'9']+
let var = ['a'-'z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']*

rule read =
  parse
  | whitespace { read lexbuf }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "let" { LET }
  | "in" { IN }
  | "fun" { FUN }
  | "rec" { REC }
  | "true" { TRUE }
  | "false" { FALSE }
  | "->" { ARROW }
  | "unit" { UNITTY }
  | "int" { INTTY }
  | "bool" { BOOLTY }

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
  | ":" { COLON }
  | "(" { LPAREN }
  | ")" { RPAREN }

  | num { NUM (int_of_string (Lexing.lexeme lexbuf)) }
  | var { VAR (Lexing.lexeme lexbuf) }
  | eof { EOF }

  | _ { failwith ("Unexpected character: " ^ Lexing.lexeme lexbuf) }
