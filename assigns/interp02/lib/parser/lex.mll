{
open Par
}

let whitespace = [' ' '\t' '\n' '\r']+
let num = '-'? ['0'-'9']+
let var = ['a'-'z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']*

rule read = 
  parse
  | "let"    { LET }
  | "rec"    { REC }
  | "in"     { IN }
  | "if"     { IF }
  | "then"   { THEN }
  | "else"   { ELSE }
  | "fun"    { FUN }
  | "assert" { ASSERT }
  | "()"     { UNIT }
  | "true"   { TRUE }
  | "false"  { FALSE }
  | "int"    { INT }
  | "bool"   { BOOL }
  | "unit"   { UNIT_TY }
  | "("      { LPAREN }
  | ")"      { RPAREN }
  | ":"      { COLON }
  | "->"     { ARROW }
  | "+"      { ADD }
  | "-"      { SUB }
  | "*"      { MUL }
  | "/"      { DIV }
  | "mod"    { MOD }
  | "<="     { LTE }
  | "<>"     { NEQ }
  | ">="     { GTE }
  | "<"      { LT }
  | ">"      { GT }
  | "="      { EQ }
  | "&&"     { AND }
  | "||"     { OR }
  | num      { NUM (int_of_string (Lexing.lexeme lexbuf)) }
  | var      { VAR (Lexing.lexeme lexbuf) }
  | whitespace { read lexbuf }
  | eof      { EOF }