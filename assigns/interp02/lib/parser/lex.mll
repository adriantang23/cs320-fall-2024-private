{
open Par
}

let whitespace = [' ' '\t' '\n' '\r']+
let num = '-'? ['0'-'9']+
let var = ['a'-'z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']*

rule read = parse
| whitespace { read lexbuf }

| "let" { LET }
| "rec" { REC }
| "in" { IN }
| "if" { IF }
| "then" { THEN }
| "else" { ELSE }
| "fun" { FUN }

| "true" { TRUE }
| "false" { FALSE }
| "assert" { ASSERT }

| "int" { INTTY }
| "bool" { BOOLTY }
| "unit" { UNITTY }

| "()" { UNIT }
| ":" { COLON }
| "->" { ARROW }
| '(' { LPAREN }
| ')' { RPAREN }

| '+' { ADD }
| '-' { SUB }
| '*' { MUL }
| '/' { DIV }
| "mod" { MOD }

| '<' { LT }
| "<=" { LTE }
| '>' { GT }
| ">=" { GTE }
| '=' { EQ }
| "<>" { NEQ }

| "&&" { AND }
| "||" { OR }

| num { NUM (int_of_string (Lexing.lexeme lexbuf)) }
| var { VAR (Lexing.lexeme lexbuf) }
| eof { EOF }
| _ as char { failwith (Printf.sprintf "Unexpected character: %c" char) }