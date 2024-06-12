
(* The type of tokens. *)

type token = 
  | WHILE
  | VAR
  | TRUE
  | STAR
  | SEMICOLON
  | RPAREN
  | RBRACE
  | PLUS
  | NUMBER of (int)
  | MINUS
  | LPAREN
  | LE
  | LBRACE
  | IF
  | IDENT of (string)
  | FOR
  | FALSE
  | EQ
  | EOF
  | ENOT
  | ELSE
  | COMMA
  | ASSIGN
  | ASSERT
  | AND

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val start: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Syntax.pgm)
