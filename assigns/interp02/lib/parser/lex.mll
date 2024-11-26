{
open Par
open Lexing

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }
}

let whitespace = [' ' '\t' '\n' '\r']+
let num = '-'? ['0'-'9']+
let var = ['a'-'z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']*

rule read = parse
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
  | "="      { EQUALS }
  | "->"     { ARROW }
  | "+"      { ADD }
  | "-"      { SUB }
  | "*"      { MUL }
  | "/"      { DIV }
  | "mod"    { MOD }
  | "<"      { LT }
  | "<="     { LTE }
  | ">"      { GT }
  | ">="     { GTE }
  | "="      { EQ }
  | "<>"     { NEQ }
  | "&&"     { AND }
  | "||"     { OR }
  | var      { VAR (Lexing.lexeme lexbuf) }
  | whitespace { read lexbuf }
  | num      { NUM (int_of_string (Lexing.lexeme lexbuf)) }
  | eof      { EOF }