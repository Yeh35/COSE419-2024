
(* The type of tokens. *)

type token = 
  | WHILE
  | STAR
  | SORTED
  | SLASH
  | SKIP
  | SEMICOLON
  | RPAREN
  | RETURN
  | RBRACK
  | RBRACE
  | PRE
  | POST
  | PLUS
  | PARTITIONED
  | OR
  | NUMBER of (int)
  | NEQ
  | MINUS
  | MID
  | LT
  | LPAREN
  | LE
  | LBRACK
  | LBRACE
  | INT
  | IMPLY
  | IFF
  | IF
  | IDENT of (string)
  | HASH
  | GT
  | GE
  | FORALL
  | FOR
  | FNOT
  | EXISTS
  | EQ
  | EOF
  | ENOT
  | ELSE
  | DOT
  | DO
  | COMMA
  | COLON
  | CALL
  | BREAK
  | BOOLEAN of (bool)
  | BOOL
  | ASSIGN
  | ASSERT
  | AND

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val start: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Syntax.pgm)
