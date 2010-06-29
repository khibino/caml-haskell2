

type pos = {
  line : int;
  col  : int;
}

let pos line col = {
  line = line;
  col  = col;
}

let string_of_pos pos =
  string_of_int pos.line ^ ":" ^ string_of_int pos.col

type region = {
  start_p : pos;
  end_p   : pos;
}

let string_of_region region =
  string_of_pos region.start_p ^ " -- " ^ string_of_pos region.end_p

let region start_p end_p = {
  start_p = start_p;
  end_p = end_p;
}

type typ =
  | SP_LEFT_PAREN
  | SP_RIGHT_PAREN
  | SP_COMMA
  | SP_SEMI
  | SP_LEFT_BRACKET
  | SP_RIGHT_BRACKET
  | SP_B_QUOTE
  | SP_LEFT_BRACE
  | SP_RIGHT_BRACE
  | K_CASE
  | K_CLASS
  | K_DATA
  | K_DEFAULT
  | K_DERIVING
  | K_DO
  | K_ELSE
  | K_IF
  | K_IMPORT
  | K_IN
  | K_INFIX
  | K_INFIXL
  | K_INFIXR
  | K_INSTANCE
  | K_LET
  | K_MODULE
  | K_NEWTYPE
  | K_OF
  | K_THEN
  | K_TYPE
  | K_WHERE
  | K_WILDCARD
  | KS_DOTDOT
  | KS_COLON
  | KS_2_COLON
  | KS_EQ
  | KS_B_SLASH
  | KS_BAR
  | KS_L_ARROW
  | KS_R_ARROW
  | KS_AT
  | KS_TILDE
  | KS_R_W_ARROW
  | K_AS
  | K_QUALIFIED
  | K_HIDING
  | KS_PLUS
  | KS_MINUS
  | KS_EXCLAM
  | T_MOD_CONSYM of (Symbol.t * Symbol.t)
  | T_MOD_CONID of (Symbol.t * Symbol.t)
  | T_MOD_CLSID of (Symbol.t * Symbol.t)
  | T_CONSYM of Symbol.t
  | T_CONID of Symbol.t
  | T_CLSID of Symbol.t
  | T_MOD_VARSYM of (Symbol.t * Symbol.t)
  | T_MOD_VARID of (Symbol.t * Symbol.t)
  | T_VARSYM of Symbol.t
  | T_VARID of Symbol.t
  | L_CHAR of int
  | L_STRING of int array
  | L_INTEGER of int64
  | L_FLOAT of float
  | WS_WHITE
  | WS_NEWLINE
  | BLK_OPEN of int
  | BLK_LEVEL of int
  | EOF

type t = (typ * region)
