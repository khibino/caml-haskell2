
module S = String
module F = Printf

module LX = Ulexing

module TK = Token

type position = {
  line : int;
  curp : int;
  bol  : int;
}

let tk_pos pos = TK.pos pos.line (pos.curp - pos.bol + 1)

type context = {
  pos : position;
  lexbuf : LX.lexbuf;
}

let make_context lexbuf =
  { pos  = { line = 1;
             curp = 0;
             bol  = 0;
           };
    lexbuf = lexbuf
  }

(*
let context_from_channel chan =
  make_context (LX.from_utf8_channel chan)

let context_from_string str =
  make_context (LX.from_utf8_string str)
*)

let u8char_list_lexeme lexbuf =
  List.map
    (fun cp -> (TK.u8string_of_cp cp, cp))
    (Array.to_list (LX.lexeme lexbuf))

let u8char_list_to_string cl = S.concat "" (List.map fst cl)

let next_position context =
  let str = u8char_list_lexeme context.lexbuf in

  let newline pos =
    { line = pos.line + 1;
      curp = pos.curp + 1;
      bol  = pos.curp + 1;
    } in

  let tab pos =
    { pos with
      curp = pos.curp + 8 - (pos.curp - pos.bol) mod 8
    } in

  let other pos =
    { pos with
      curp = pos.curp + 1
    } in

  let rec fix_pos_rec pos str =
    match str with
      | [] -> pos
      | ("\r", _) :: ("\n", _) :: rest
      | (("\r" | "\n" | "\x0c"), _)     :: rest -> fix_pos_rec (newline pos) rest
      | ("\t", _)              :: rest -> fix_pos_rec (tab pos)     rest
      |  _                     :: rest -> fix_pos_rec (other pos)   rest
  in
  { context with
    pos = fix_pos_rec context.pos str
  }

let decode_cexpr err = function
  | (fchar::escexp) ->
    let escexp_str = u8char_list_to_string escexp in
    let retc c = Some (Char.code c) in
    let retis is = Some (int_of_string is) in
    let fmatch exp str = Str.string_match (Str.regexp exp) str 0 in
    if fst fchar = "\\" then
      match escexp_str with
        | "NUL"          -> retc '\x00'
        | "SOH" | "^A"   -> retc '\x01'
        | "STX" | "^B"   -> retc '\x02'
        | "ETX" | "^C"   -> retc '\x03'
        | "EOT" | "^D"   -> retc '\x04'
        | "ENQ" | "^E"   -> retc '\x05'
        | "ACK" | "^F"   -> retc '\x06'
          
        | "BEL" | "^G" | "a"  -> retc '\x07'
        | "BS"  | "^H" | "b"  -> retc '\x08'
        | "HT"  | "^I" | "t"  -> retc '\t'
        | "LF"  | "^J" | "n"  -> retc '\n'
        | "VT"  | "^K" | "v"  -> retc '\x0b'
        | "FF"  | "^L" | "f"  -> retc '\x0c'
        | "CR"  | "^M" | "r"  -> retc '\r'
        | "SO"  | "^N"   -> retc '\x0e'
        | "SI"  | "^O"   -> retc '\x0f'
        | "DLE" | "^P"   -> retc '\x10'
          
        | "DC1" | "^Q"   -> retc '\x11'
        | "DC2" | "^R"   -> retc '\x12'
        | "DC3" | "^S"   -> retc '\x13'
        | "DC4" | "^T"   -> retc '\x14'
        | "NAK" | "^U"   -> retc '\x15'
        | "SYN" | "^V"   -> retc '\x16'
        | "ETB" | "^W"   -> retc '\x17'
        | "CAN" | "^X"   -> retc '\x18'
          
        | "EM"  | "^Y"   -> retc '\x19'
        | "SUB" | "^Z"   -> retc '\x1a'
        | "ESC" | "^["   -> retc '\x1b'
        | "FS"  | "^\\"  -> retc '\x1c'
        | "GS"  | "^]"   -> retc '\x1d'
        | "RS"  | "^^"   -> retc '\x1e'
        | "US"  | "^_"   -> retc '\x1f'
        | "SP"           -> retc ' '

        | "\\"           -> retc '\\'
        | "\""           -> retc '"'
        | "'"            -> retc '\''

        | "DEL"          -> retc '\x7f'

        | _ when fmatch "^[0-9]+$" escexp_str
            -> retis escexp_str
        | _ when fmatch "^[xX][0-9a-zA-Z]+$" escexp_str
            -> retis ("0" ^ escexp_str)
        | _ when fmatch "^[oO][0-7]+$" escexp_str
            -> retis ("0" ^ escexp_str)

        | _ -> None

    else Some (snd fchar)

  | [] -> Some (err "Zero length char expression!")

let decode_char clist err =
  match decode_cexpr err clist with
    | Some c -> c
    | None   -> err (F.sprintf "Unkown char expression %s" (u8char_list_to_string clist))

module NComment = struct
  type t =
    | Open
    | Close
    | Char of int
    | Eof
end

module LChar = struct
  type t =
    | Char of int
    | Quote
end

module LStr = struct
  type t =
    | Nul
    | Char of int
    | Gap  of int array
    | Quote
end

let decode_lchar_char lexbuf err =
  LChar.Char (decode_char (u8char_list_lexeme lexbuf) err)

let decode_lstr_char lexbuf err =
  match u8char_list_lexeme lexbuf with
    | ("\\", _) :: ("&", _) :: _ -> LStr.Nul
    | cl                         -> LStr.Char (decode_char cl err)

let regexp special = ['(' ')' ',' ';' '[' ']' '`' '{' '}']

let regexp space = ' '
let regexp newline = ("\r\n"|['\n' '\r' '\x0c' (* formfeed *) ])
let regexp tab = '\t'

let regexp dashes = '-' '-' '-'*

let regexp ascSmall = ['a'-'z']
let regexp small = ascSmall | '_'
let regexp ascLarge = ['A'-'Z']
let regexp large = ascLarge

let regexp plus = '+'
let regexp minus = '-'
let regexp exclamation = '!'
let regexp ascSymbol_nbs = [ '!' '#' '$' '%' '&' '*' '+' '.' '/' '<' '=' '>' '?' '@' '^' '|' '-' '~' ]
let regexp ascSymbol = ascSymbol_nbs | '\\'
let regexp symbol = ascSymbol

let regexp ascDigit = ['0'-'9']
let regexp digit = ascDigit

let regexp octit = ['0'-'7']
let regexp hexit = ascDigit | ['a'-'z' 'A'-'Z']

let regexp decimal = (digit)+
let regexp octal = (octit)+
let regexp hexadecimal = (hexit)+

let regexp exponent = ['e' 'E'] ['+' '-']? decimal
let regexp float = decimal '.' decimal exponent? | decimal exponent

let regexp graphic_common = small | large | symbol | digit | special | ':'
let regexp graphic = graphic_common | ['"' '\'']
let regexp any = graphic | space | tab

let regexp comment = dashes ((space | tab | small | large | digit | special | [':' '"' '\'']) (any) * )? newline

let regexp opencom  = "{-"
let regexp closecom = "-}"

let regexp whitechar = newline | space | tab
let regexp whitestuff = whitechar | comment 
let regexp whitespace = (whitestuff)+

let regexp l_any = graphic | whitechar

let regexp char_gr = graphic_common | '"'
let regexp str_gr  = graphic_common | '\''

let regexp charesc = ['a' 'b' 'f' 'n' 'r' 't' 'v' '\\' '"' '\'']
let regexp str_charesc = charesc | '&'
let regexp cntrl = ascLarge | ['@' '[' '\\' ']' '^' '_']
let regexp gap = '\\' (whitechar)+ '\\'

let regexp ascii = ('^' cntrl) | "NUL" | "SOH" | "STX" | "ETX" | "EOT" | "ENQ" | "ACK"
  | "BEL" | "BS" | "HT" | "LF" | "VT" | "FF" | "CR" | "SO" | "SI" | "DLE"
  | "DC1" | "DC2" | "DC3" | "DC4" | "NAK" | "SYN" | "ETB" | "CAN"
  | "EM" | "SUB" | "ESC" | "FS" | "GS" | "RS" | "US" | "SP" | "DEL"

let regexp escape = '\\' ( charesc | ascii | decimal | 'o' octal | 'x' hexadecimal )
let regexp str_escape = '\\' ( str_charesc | ascii | decimal | 'o' octal | 'x' hexadecimal )

let regexp char = '\'' (char_gr | space | escape) '\''
let regexp string = '"' (str_gr | space | str_escape | gap)* '"'

let regexp varid = small (small | large | digit | '\'')*
let regexp conid = large (small | large | digit | '\'')*

let regexp varsym = symbol (symbol | ':')*
let regexp consym = ':' (symbol | ':')*

let regexp modid = ( conid '.' )* conid


module SYM = Symbol

let rec lex_haskell context =
  let next_context () = next_position context in
  let region () =
    let next_context = next_context () in
    TK.region (tk_pos context.pos) (tk_pos next_context.pos), next_context
  in
  let err msg = failwith (TK.string_of_region (fst (region ())) ^ ": " ^ msg) in

  let tk token =
    let (region, next_context) = region () in
    ((token , region), next_context)
  in

  let u8lexeme () = LX.utf8_lexeme context.lexbuf in
  let lexsym ()   = SYM.intern (u8lexeme ()) in
  let lexqsym ()  = TK.syms_of_qstring (u8lexeme ()) in

  let skip () = lex_haskell (next_context ()) in

  let lex_ncomment context =
    let ncomment_lexer =
      (lexer
          | opencom  -> NComment.Open
          | closecom -> NComment.Close
          | l_any    -> NComment.Char ((LX.lexeme context.lexbuf).(0))
          | eof      -> NComment.Eof)
    in

    let start_p = tk_pos context.pos in

    let err context msg =
      let end_p   = tk_pos context.pos in
      let region  = TK.region start_p end_p in
      failwith ((TK.string_of_region region) ^ ": " ^ msg)
    in

    let rec parse_nest context level =
      let rec parse context =
        let token = ncomment_lexer context.lexbuf in
        let context = next_position context in
        match token with
          | NComment.Open   ->
            let (context, _) = parse_nest context (level + 1) in
            parse context
          | NComment.Close  -> (context, level)
          | NComment.Char _ -> parse context
          | NComment.Eof    -> err context "Lexing error duaring nested comment."
      in
      parse context
    in

    let (context, level) = parse_nest context 0 in
    if level != 0 then err context "Invalid state nested comment?"
    else context
  in

  let lex_char context =
    let lexbuf = LX.from_utf8_string (LX.utf8_lexeme context.lexbuf) in
    let char_lexer =
      (lexer
          | char_gr | space | escape -> decode_lchar_char lexbuf err
          | '\''                     -> LChar.Quote
      )
    in
    let qb   = char_lexer lexbuf in
    let char = char_lexer lexbuf in
    let qe   = char_lexer lexbuf in
    match (qb, char, qe) with
      | (LChar.Quote, LChar.Char cp, LChar.Quote) -> tk (TK.L_CHAR cp)
      | _                              -> err "Lexing failed duaring char. "
  in

  let lex_string context =
    let lexbuf = LX.from_utf8_string (LX.utf8_lexeme context.lexbuf) in
    let string_lexer =
      (lexer
          | str_gr | space | str_escape  -> decode_lstr_char lexbuf err
          | gap                          -> LStr.Gap (LX.lexeme lexbuf)
          | '"'                          -> LStr.Quote
          | _                            -> err "Lexing failed duaring string."
      )
    in
    let rec parse rv =
      let token = string_lexer lexbuf in
      match token with
        | LStr.Quote   -> (Array.of_list (List.rev rv))
        | LStr.Nul     -> parse rv
        | LStr.Char cp -> parse (cp :: rv)
        | LStr.Gap _   -> parse rv
    in
    if string_lexer lexbuf != LStr.Quote then
      err "Invalid state. lex_string called at not beginning of string ?"
    else tk (TK.L_STRING (parse []))
  in

  let hs_lexer = lexer
  | '('  -> tk TK.SP_LEFT_PAREN
  | ')'  -> tk TK.SP_RIGHT_PAREN
  | ','  -> tk TK.SP_COMMA
  | ';'  -> tk TK.SP_SEMI
  | '['  -> tk TK.SP_LEFT_BRACKET
  | ']'  -> tk TK.SP_RIGHT_BRACKET
  | '`'  -> tk TK.SP_B_QUOTE
  | '{'  -> tk TK.SP_LEFT_BRACE
  | '}'  -> tk TK.SP_RIGHT_BRACE
  (** special tokens *)

  | "case"     -> tk TK.K_CASE
  | "class"    -> tk TK.K_CLASS
  | "data"     -> tk TK.K_DATA
  | "default"  -> tk TK.K_DEFAULT
  | "deriving" -> tk TK.K_DERIVING
  | "do"       -> tk TK.K_DO
  | "else"     -> tk TK.K_ELSE
  | "foreign"  -> tk TK.K_FOREIGN
  | "if"       -> tk TK.K_IF
  | "import"   -> tk TK.K_IMPORT
  | "in"       -> tk TK.K_IN
  | "infix"    -> tk TK.K_INFIX
  | "infixl"   -> tk TK.K_INFIXL
  | "infixr"   -> tk TK.K_INFIXR
  | "instance" -> tk TK.K_INSTANCE
  | "let"      -> tk TK.K_LET
  | "module"   -> tk TK.K_MODULE
  | "newtype"  -> tk TK.K_NEWTYPE
  | "of"       -> tk TK.K_OF
  | "then"     -> tk TK.K_THEN
  | "type"     -> tk TK.K_TYPE
  | "where"    -> tk TK.K_WHERE
  | "_"        -> tk TK.K_WILDCARD
  (** reservedid *)

  | ".."       -> tk TK.KS_DOTDOT
  | ":"        -> tk TK.KS_COLON
  | "::"       -> tk TK.KS_2_COLON
  | "="        -> tk TK.KS_EQ
  | "\\"       -> tk TK.KS_B_SLASH
  | "|"        -> tk TK.KS_BAR
  | "<-"       -> tk TK.KS_L_ARROW
  | "->"       -> tk TK.KS_R_ARROW
  | "@"        -> tk TK.KS_AT
  | "~"        -> tk TK.KS_TILDE
  | "=>"       -> tk TK.KS_R_W_ARROW
  (** reservedop *)

  | modid '.' varid   -> tk (TK.T_MOD_VARID (lexqsym ()))
  | modid '.' conid   -> tk (TK.T_DOT_CONID (lexsym ()))
  | modid '.' varsym  -> tk (TK.T_MOD_VARSYM (lexqsym ()))
  | modid '.' consym  -> tk (TK.T_MOD_CONSYM (lexqsym ()))
  (** qualified xx *)

  | "as"              -> tk TK.K_AS  (** maybe varid *)
  | "qualified"       -> tk TK.K_QUALIFIED  (** maybe varid *)
  | "hiding"          -> tk TK.K_HIDING  (** maybe varid *)
  | "export"          -> tk TK.K_EXPORT  (** maybe varid *)
  | varid      -> tk (TK.T_VARID (lexsym ()))
  | conid      -> tk (TK.T_CONID (lexsym ()))
  (** identifiers or may be qualified ones *)

  | opencom     -> lex_haskell (lex_ncomment context) (** skipping nested comment *)
  | whitespace  -> skip () (** comment begining with dashes is not varsym *)
  (** white spaces *)

  | plus       -> tk TK.KS_PLUS  (** maybe varsym *)
  | minus      -> tk TK.KS_MINUS (** maybe varsym *)
  | exclamation  -> tk TK.KS_EXCLAM (** maybe varsym *)
  | varsym     -> tk (TK.T_VARSYM (lexsym ()))
  | consym     -> tk (TK.T_CONSYM (lexsym ()))
  (** symbols or may be qualified ones *)

  | char      -> lex_char context
  | string    -> lex_string context

  | decimal | ('0' ['o' 'O'] octal) | ('0' ['x' 'X'] hexadecimal)
    -> tk (TK.L_INTEGER (Int64.of_string(u8lexeme ())))

  | float      -> tk (TK.L_FLOAT (float_of_string(u8lexeme ())))

  | eof        -> tk TK.EOF

  | _          -> err "Invalid pattern duaring lex."

  in
  hs_lexer context.lexbuf

let make_lazy_list lexbuf = 
  let next_token (cxt, prev) =
    if prev = Some (TK.EOF) then None
    else let ((tk, _) as tkwl, cxt) = lex_haskell cxt in
         Some (tkwl, (cxt, Some tk))
  in
  LazyList.unfold (make_context lexbuf, None) next_token

let lazy_list_of_channel chan = make_lazy_list (LX.from_utf8_channel chan)
let lazy_list_of_string  str  = make_lazy_list (LX.from_utf8_string str)
