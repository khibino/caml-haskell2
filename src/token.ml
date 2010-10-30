

type pos = {
  line : int;
  col  : int;
}

let pos line col = {
  line = line;
  col  = col;
}

let (<.) a b = (a.line < b.line) || (a.line = b.line) && a.col < b.col
let (>.) a b = (a.line > b.line) || (a.line = b.line) && a.col > b.col

let former a b = if a <. b then a else b
let later  a b = if a <. b then b else a

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

let offset region = region.start_p.col

let other_line_p reg0 reg1 =
  (reg1.start_p.line - reg0.end_p.line) > 0

let cover_region reg0 reg1 = region (former reg0.start_p reg1.start_p) (later reg0.end_p reg1.end_p)

let form_prepend (_, reg0) (form, reg1) =
  (form, cover_region reg0 reg1)

let form_append (form, reg0) (_, reg1) =
  (form, cover_region reg0 reg1)

let form_between (_, reg0) form (_, reg1) =
  (form, cover_region reg0 reg1)

let syms_of_qstring s =
  let idx = String.rindex s '.' in
  (Symbol.intern (Str.string_before s idx),
   Symbol.intern (Str.string_after s (idx + 1)))

let syms_to_qstring (q, n) = Symbol.name q ^ "." ^ Symbol.name n

let u8string_of_cp cp = Utf8.from_int_array [|cp|] 0 1

let char_to_string cp =
  if Glib.Unichar.isprint cp then Format.sprintf "%s" (u8string_of_cp cp)
  else Format.sprintf "\\u%04x" cp

let show_char cp = "'" ^ char_to_string cp ^ "'"

let show_string cpl =
  "\"" ^ String.concat "" (List.map char_to_string (Array.to_list cpl)) ^ "\""

type type_ =
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
  | K_FOREIGN
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
  | K_EXPORT
  | KS_PLUS
  | KS_MINUS
  | KS_EXCLAM
  | T_MOD_CONSYM of (Symbol.t * Symbol.t)
  (* | T_MODID of Symbol.t *)
  (* | T_MOD_CONID of (Symbol.t * Symbol.t) *)
  | T_DOT_CONID of Symbol.t
  (* | T_MOD_CLSID of (Symbol.t * Symbol.t) *)
  | T_CONSYM of Symbol.t
  | T_CONID of Symbol.t
  (* | T_CLSID of Symbol.t *)
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

type t = (type_ * region)

(* let type_ : t -> type_ = fst *)
(* let region : t -> region = snd *)

let with_region_just v (_, reg) = (v, reg)
let with_region : ('a -> 'b) -> ('a * region) -> ('b * region) = Data.with_snd

let type_to_string = function
  | SP_LEFT_PAREN  -> "("
  | SP_RIGHT_PAREN -> ")"
  | SP_COMMA -> ","
  | SP_SEMI  -> ";"
  | SP_LEFT_BRACKET -> "["
  | SP_RIGHT_BRACKET -> "]"
  | SP_B_QUOTE -> "`"
  | SP_LEFT_BRACE -> "{"
  | SP_RIGHT_BRACE -> "}"
  | K_CASE -> "case"
  | K_CLASS -> "class"
  | K_DATA -> "data"
  | K_DEFAULT  -> "default"
  | K_DERIVING -> "deriving"
  | K_DO -> "do"
  | K_ELSE -> "else"
  | K_FOREIGN -> "foreign"
  | K_IF -> "if"
  | K_IMPORT -> "import"
  | K_IN -> "in"
  | K_INFIX -> "infix"
  | K_INFIXL -> "infixl"
  | K_INFIXR -> "infixr"
  | K_INSTANCE -> "instance"
  | K_LET -> "let"
  | K_MODULE -> "module"
  | K_NEWTYPE -> "newtype"
  | K_OF -> "of"
  | K_THEN -> "then"
  | K_TYPE -> "type"
  | K_WHERE -> "where"
  | K_WILDCARD -> "_"
  | KS_DOTDOT -> ".."
  | KS_COLON -> ":"
  | KS_2_COLON -> "::"
  | KS_EQ -> "="
  | KS_B_SLASH -> "\\"
  | KS_BAR -> "|"
  | KS_L_ARROW -> "<-"
  | KS_R_ARROW -> "->"
  | KS_AT -> "@"
  | KS_TILDE -> "~"
  | KS_R_W_ARROW -> "=>"
  | K_AS -> "as"
  | K_QUALIFIED -> "qualified"
  | K_HIDING -> "hiding"
  | K_EXPORT -> "export"
  | KS_PLUS -> "+"
  | KS_MINUS -> "-"
  | KS_EXCLAM -> "!"
  | T_VARID(n) -> (Symbol.name n)
  | T_CONID(n) -> (Symbol.name n)
  (* | T_CLSID(n) -> "<class>:" ^ (Symbol.name n) *)
  | T_VARSYM(n) -> (Symbol.name n)
  | T_CONSYM(n) -> (Symbol.name n)
  | T_MOD_VARID(syms) -> syms_to_qstring syms
  (* | T_MODID(n)  -> (Symbol.name n) *)
  (* | T_MOD_CONID(syms) -> syms_to_qstring syms *)
  | T_DOT_CONID(n)  -> (Symbol.name n)
  (* | T_MOD_CLSID(syms) -> "<class>:" ^ syms_to_qstring syms *)
  | T_MOD_VARSYM(syms) -> syms_to_qstring syms
  | T_MOD_CONSYM(syms) -> syms_to_qstring syms
  | L_CHAR(cp) ->         show_char cp
  | L_STRING(cpl) ->      show_string cpl
  | L_INTEGER(i64) -> Int64.to_string i64
  | L_FLOAT(f) -> string_of_float f
  | WS_WHITE   -> "<WS>"
  | WS_NEWLINE -> "<NEWLINE>"

  | BLK_OPEN(lv) -> Format.sprintf "{%d}" lv
  | BLK_LEVEL(lv) -> Format.sprintf "<%d>" lv
  | EOF -> "<EOF>"

let to_string : t -> string =
  fun (tk, reg) -> type_to_string tk ^ "(" ^ string_of_region reg ^ ")"
