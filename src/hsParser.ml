
module Combinator = Simple.Combinator(Token)
(* module Combinator = Simple.DebugCombinator(Token) *)
open Combinator

module L = List

module TK = Token
module HSY = HsSyntax
module HPST = HsParserState

let call = call_parser

let (|.|) f g x = f (g x)

let pred_tk : string -> (TK.type_ -> bool) -> TK.t parser =
  fun name f -> pred name (f |.| fst)
let just_tk eq = pred_tk (TK.type_to_string eq) ((=) eq)
let untag_tk : string -> (TK.type_ -> 'a option) -> ('a * TK.region) parser =
  fun name f ->
    untag name (fun (tk, reg) ->
      match f tk with
        | Some v -> Some (v, reg)
        | None   -> None)

let qual_id_tk name f =
  TK.with_region HSY.qual_id *<$> untag_tk name f

let pos_dummy = TK.pos (-1) (-1)
let region_dummy = TK.region pos_dummy pos_dummy
let pure_with_dummy_region v = pure (v, region_dummy)

let pos_fix_later = pos_dummy
let region_fix_later = TK.region pos_fix_later pos_fix_later
let p_fix_later : (unit * TK.region) parser = pure ((), region_fix_later)

let tk_module = just_tk TK.K_MODULE
let tk_import = just_tk TK.K_IMPORT
let tk_export = just_tk TK.K_EXPORT
let let_  = just_tk TK.K_LET
let where = just_tk TK.K_WHERE
let comma = just_tk TK.SP_COMMA
let semi  = just_tk TK.SP_SEMI
let eq      = just_tk TK.KS_EQ
let minus   = just_tk TK.KS_MINUS
let exclam  = just_tk TK.KS_EXCLAM
let two_colon = just_tk TK.KS_2_COLON
let dotdot  = just_tk TK.KS_DOTDOT
let bar = just_tk TK.KS_BAR
let l_arrow = just_tk TK.KS_L_ARROW
let r_arrow = just_tk TK.KS_R_ARROW
let r_w_arrow = just_tk TK.KS_R_W_ARROW
let l_paren = just_tk TK.SP_LEFT_PAREN
let r_paren = just_tk TK.SP_RIGHT_PAREN
let l_brace = just_tk TK.SP_LEFT_BRACE
let r_brace = just_tk TK.SP_RIGHT_BRACE

let match_or_shift_rb = match_or_shift r_brace

let opt_semi = ~?semi
let opt_exclam = ~?exclam

(* 汎用の構成子 *)
(*   構文構造の並び、挟まれる構造、0以上のリスト、1以上のリスト *)

let form_prepend a p = TK.form_prepend *<$> a *<*> p
let form_append  p a = TK.form_append  *<$> p *<*> a

let ( **|> ) = form_prepend
let ( **<| ) = form_append

let form_between a b p = TK.form_between *<$> a *<*> p *<*> b
let between a b = form_between a b |.| (lift_a fst)

let form_parened    p = form_between l_paren                      r_paren   p
let form_bracketed  p = form_between (just_tk TK.SP_LEFT_BRACKET) (just_tk TK.SP_RIGHT_BRACKET) p
let form_braced     p = form_between l_brace                      r_brace   p
let form_shift_braced  p = form_between l_brace                   match_or_shift_rb   p
let form_backquoted p = form_between (just_tk TK.SP_B_QUOTE)      (just_tk TK.SP_B_QUOTE)       p

let parened    p = form_parened    ((lift_a fst) p)
let bracketed  p = form_bracketed  ((lift_a fst) p)
let braced     p = form_braced     ((lift_a fst) p)
let shift_braced  p = form_shift_braced  ((lift_a fst) p)
let backquoted p = form_backquoted ((lift_a fst) p)

module Raw = struct
  (* 長さ1以上のリスト 意図的に0以上のリストと型が異なるようにしている *)
  let l1_some p = Data.l1_list_cons *<$> p *<*> many p

  let rec l1_separated a d =
    Data.l1_cons *<$> a *<*> d **> ~$(fun () -> l1_separated a d)
    <|> Data.l1_cons_nil *<$> a

  let l1_separated_2 a d =
    Data.l1_cons *<$> a *<*> d **> l1_separated a d

  (* リストとなる構文要素 - 長さ0でも可 - 位置情報付き *)
  let some = Combinator.some
  let many = Combinator.many

  let separated a d =
    Data.l1_list *<$> l1_separated a d <|> pure []
end

(* リストとなる構文要素 - 長さ0でも可 - 位置情報付き *)
let list_form pl =
  (function
    | []         -> ([], region_dummy)
    | [(e, reg)] -> ([e], reg)
    | (er :: _) as ers -> TK.form_between er (L.map fst ers) (L.hd (Data.last' ers)))
  *<$> pl

let some' x = list_form (Raw.some x)
let many' x = list_form (Raw.many x)
let separated a d = list_form (Raw.separated a d)

let cons_nil x = list_form (Data.cons_nil *<$> x)

let l1_form pl1 =
  (function
    | ((e, reg), []) -> ((e, []), reg)
    | (er, ers)  -> TK.form_between er (fst er, (L.map fst ers)) (L.hd (Data.last' ers)))
  *<$> pl1

let l1_some x = l1_form (Raw.l1_some x)
let l1_separated a d = l1_form (Raw.l1_separated a d)

let l1_separated_2 a d = l1_form (Raw.l1_separated_2 a d)

let l1_list_form pl1 = TK.with_region Data.l1_list *<$> l1_form pl1

(* conid 		(constructors)      *)
(* conid は doted_conid から使われている *)
(* conid is used by doted_conid above *)
(* Qualified constructor or module id is ambiguous *)
let conid = untag_tk "conid" (function | TK.T_CONID s -> Some s | _ -> None)
let doted_conid = untag_tk "doted_conid" (function | TK.T_DOT_CONID s  -> Some s | _ -> None) <|> conid

(* 10.2  Lexical Syntax *)

(* literal 	 → 	integer | float | char | string      *)
let literal = untag_tk "literal" (function
  | TK.L_CHAR cp   -> Some (HSY.Char cp)
  | TK.L_STRING ca -> Some (HSY.Str ca)
  | TK.L_INTEGER i -> Some (HSY.Int i)
  | TK.L_FLOAT f   -> Some (HSY.Flo f)
  | _              -> None)

(* varsym 	 → 	( symbol⟨:⟩ {symbol} )⟨reservedop | dashes⟩      *)
let varsym = untag_tk "varsym" (function
  | TK.T_VARSYM s -> Some s
  | TK.KS_PLUS    -> Some HSY.sym_plus
  | TK.KS_MINUS   -> Some HSY.sym_minus
  | TK.KS_EXCLAM  -> Some HSY.sym_exclam
  | _             -> None)
(* consym 	→ 	( : {symbol}){reservedop}      *)
let consym = untag_tk "consym" (function | TK.T_CONSYM s -> Some s | _ -> None)


(* varid 	 	 (variables)      *)
let varid = untag_tk "varid" (function
  | TK.T_VARID s   -> Some s
  | TK.K_AS        -> Some HSY.sym_as
  | TK.K_QUALIFIED -> Some HSY.sym_qualified
  | TK.K_HIDING    -> Some HSY.sym_hiding
  | TK.K_EXPORT    -> Some HSY.sym_export
  | _              -> None)

(* tyvar 	→ 	varid     	(type variables) *)
let tyvar = varid
(* tycon 	→ 	conid     	(type constructors) *)
let tycon = conid
(* tycls 	→ 	conid     	(type classes) *)
let tycls = conid
(* modid 	→ 	{conid .} conid     	(modules) *)
let modid = doted_conid

(* qvarid 	→ 	[ modid . ] varid      *)
let qvarid = qual_id_tk "qvarid" (function
  | TK.T_MOD_VARID p  -> Some p
  | _ -> None)
  <|> HPST.q_not_qual *<$> varid
(* qconid 	→ 	[ modid . ] conid      *)
let qconid  = HPST.sym_to_qconid *<$> doted_conid
(* qtycon 	→ 	[ modid . ] tycon      *)
let qtycon  = qual_id_tk "qtycon" (function
  | TK.T_DOT_CONID s  -> Some (TK.syms_of_qstring (Symbol.name s))
  | _ -> None)
  <|> HPST.q_not_qual *<$> tycon
(* qtycls 	→ 	[ modid . ] tycls      *)
let qtycls  = qual_id_tk "qtycls" (function
  | TK.T_DOT_CONID s  -> Some (TK.syms_of_qstring (Symbol.name s))
  | _ -> None)
  <|> HPST.q_not_qual *<$> tycls
(* qvarsym 	→ 	[ modid . ] varsym      *)
let qvarsym = qual_id_tk "qvarsym" (function
  | TK.T_MOD_VARSYM p -> Some p
  | _ -> None)
  <|> HPST.q_not_qual *<$> varsym
(* qconsym 	→ 	[ modid . ] consym      *)
let qconsym = qual_id_tk "qconsym" (function
  | TK.T_MOD_CONSYM p -> Some p
  | _ -> None)
  <|> HPST.q_not_qual *<$> consym

let string  = untag_tk "string" (function | TK.L_STRING ca -> Some ca | _ -> None)
let integer = untag_tk "integer" (function | TK.L_INTEGER i -> Some i | _ -> None)
let float   = untag_tk "float" (function | TK.L_FLOAT f -> Some f | _ -> None)

let fixity_int = Data.with_snd Int64.to_int *<$> untag_tk "int0-9" (function
  | TK.L_INTEGER i when 0L <= i && i <= 9L -> Some i
  | TK.L_INTEGER i -> (* generate error infomation ; *) None
  | _ -> None)

(* 10.5  Context-Free Syntax
   -- 変数とシンボルの定義は他から参照されるので先に定義 -- 
   -- variable and symbol section -- 
*)

(* var 	→ 	varid | ( varsym )     	(variable) *)
let var = varid <|> parened varsym

(* qvar 	→ 	qvarid | ( qvarsym )     	(qualified variable) *)
let qvar = qvarid <|> parened qvarsym

(* con 	→ 	conid | ( consym )     	(constructor) *)
let con = conid <|> parened consym

(* gconsym 	→ 	: | qconsym      *)
(* gconsym は qcon から使われている *)
(* gconsym is used by qcon above *)
let gconsym = HSY.tk_id_colon *<$> just_tk TK.KS_COLON <|> qconsym
(* qcon 	→ 	qconid | ( gconsym )     	(qualified constructor) *)
let qcon = qconid <|> parened gconsym

(* varop 	→ 	varsym | `  varid `     	(variable operator) *)
let varop = varsym <|> backquoted varid
(* qvarop 	→ 	qvarsym | `  qvarid `     	(qualified variable operator) *)
let qvarop = qvarsym <|> backquoted qvarid
(* conop 	→ 	consym | `  conid `     	(constructor operator) *)
let conop = consym <|> backquoted conid
(* qconop 	→ 	gconsym | `  qconid `     	(qualified constructor operator) *)
let qconop = gconsym <|> backquoted qconid
(* op 	→ 	varop | conop     	(operator) *)
let op = varop <|> conop
(* qop 	→ 	qvarop | qconop     	(qualified operator) *)
let qop = qvarop <|> qconop
 
(* gcon 	→ 	()      *)
(* 	| 	[]      *)
(* 	| 	(,{,})      *)
(* 	| 	qcon      *)
let rec commas1 () = comma **> (succ *<$> ~$commas1 <|> pure 1)
let gcon =
  form_parened (pure HSY.id_unit) <|> form_bracketed (pure HSY.id_null_list)
    <|> form_parened (HSY.id_tuple *<$> ~$commas1)
      <|> qcon


(* ops 	→ 	op1 , … , opn     	(n ≥ 1) *)
let ops = l1_separated op comma

(* vars 	→ 	var1 , …, varn     	(n ≥ 1) *)
let vars = l1_separated var comma

(* fixity 	→ 	infixl | infixr | infix      *)
let fixity = untag_tk "fixity" (function
  | TK.K_INFIX  -> Some HSY.I_infix
  | TK.K_INFIXL -> Some HSY.I_left
  | TK.K_INFIXR -> Some HSY.I_right
  | _           -> None)

(* 10.5  Context-Free Syntax
   -- type から参照される要素を先に定義 -- 
   -- define elements which referred by type element. -- 
*)

(* gtycon 	→ 	qtycon      *)
(* 	| 	()     	(unit type) *)
(* 	| 	[]     	(list constructor) *)
(* 	| 	(->)     	(function constructor) *)
(* 	| 	(,{,})     	(tupling constructors) *)
let     gtycon =
  HSY.gt_qtycon *<$> qtycon
  <|> form_parened (pure HSY.GT_Unit)
    <|> form_bracketed (pure HSY.GT_List)
      <|> HSY.gt_arrow *<$> parened r_arrow
        <|> HSY.gt_tuple *<$> form_parened ~$commas1

(* type 	→ 	btype [-> type]     	(function type) *)
let rec type_ ()   =
  HSY.type_of_btype_list
  *<$> l1_separated ~$btype r_arrow
 
(* btype 	→ 	[btype] atype     	(type application) *)
and     btype () =
  HSY.btype_of_atype_list
  *<$> l1_some ~$atype  (* btypeの左再帰を除去 *)

 
(* atype 	→ 	gtycon      *)
(* 	| 	tyvar      *)
(* 	| 	( type1 , … , typek )     	(tuple type, k ≥ 2) *)
(* 	| 	[ type ]     	(list type) *)
(* 	| 	( type )     	(parenthesized constructor) *)
and     atype () =
  HSY.at_gtc *<$> gtycon
  <|> HSY.at_tyvar *<$> tyvar
    <|> parened (HSY.at_tuple
                 *<$> l1_separated_2 ~$type_ comma)
      <|> bracketed (HSY.at_list *<$> ~$type_)
        <|> parened (HSY.at_paren *<$> ~$type_)

(* class 	→ 	qtycls tyvar      *)
(* 	| 	qtycls ( tyvar atype1 … atypen )     	(n ≥ 1) *)
let class_ =
  HSY.class_tval *<$> qtycls *<*> tyvar
  <|> HSY.class_tapp *<$> qtycls *<*> l_paren **> tyvar *<*> l1_some ~$atype **< r_paren

(* context 	→ 	class      *)
(* 	| 	( class1 , … , classn )     	(n ≥ 0) *)
let context =  cons_nil class_
  <|> parened (separated class_ comma)

(* [context =>] *)
let may_be_context = ~?(form_append context r_w_arrow)

(* simpleclass	→ 	qtycls tyvar      *)
let simpleclass = HSY.simpleclass *<$> qtycls *<*> tyvar

(* scontext 	→ 	simpleclass      *)
(* 	| 	( simpleclass1 , … , simpleclassn )     	(n ≥ 0) *)
let scontext = cons_nil simpleclass
  <|> parened (separated simpleclass comma)

(* [scontext =>] *)
let may_be_scontext = ~?(form_append scontext r_w_arrow)
 
(* simpletype 	→ 	tycon tyvar1 … tyvark     	(k ≥ 0) *)
let simpletype = HSY.simpletype *<$> tycon *<*> many' tyvar

(* (btype | ! atype) *)
let constr_arg = HSY.ca_satype *<$> exclam **|> ~$atype <|> HSY.ca_btype *<$> ~$btype

(* fielddecl 	→ 	vars :: (type | ! atype)      *)
let fielddecl =
  HSY.fielddecl *<$> vars *<*> two_colon **> (HSY.cf_type *<$> ~$type_
                                              <|> HSY.cf_satype *<$> ~$atype)

(* constr 	→ 	con [!] atype1 … [!] atypek     	(arity con  =  k, k ≥ 0) *)
(* 	| 	(btype | ! atype) conop (btype | ! atype)     	(infix conop) *)
(* 	| 	con { fielddecl1 , … , fielddecln }     	(n ≥ 0) *)
let constr =
  HSY.co_con *<$> con *<*> many' (HSY.may_banana_atype *<$> opt_exclam *<*> ~$atype)
  <|> HSY.co_bin *<$> constr_arg *<*> conop *<*> constr_arg
    <|> HSY.co_rec *<$> con *<*> separated fielddecl comma

(* constrs 	→ 	constr1 | … | constrn     	(n ≥ 1) *)
let constrs = l1_separated constr bar

(* newconstr 	→ 	con atype      *)
(* 	| 	con { var :: type }      *)
let newconstr =
  HSY.nc_con *<$> con *<*> ~$atype
  <|> HSY.nc_rec *<$> con *<*> l_brace **> var *<*> two_colon **> ~$type_ **< r_brace

(* dclass 	→ 	qtycls      *)
let dclass = qtycls
 
(* deriving 	→ 	deriving (dclass | (dclass1, … , dclassn))     	(n ≥ 0) *)
let deriving = just_tk TK.K_DERIVING **|> (cons_nil dclass
                                           <|> parened (separated dclass comma))

(* inst 	→ 	gtycon      *)
(* 	| 	( gtycon tyvar1 … tyvark )     	(k ≥ 0, tyvars distinct) *)
(* 	| 	( tyvar1 , … , tyvark )     	(k ≥ 2, tyvars distinct) *)
(* 	| 	[ tyvar ]      *)
(* 	| 	( tyvar1 -> tyvar2 )     	tyvar1 and tyvar2 distinct *)
let inst =
  HSY.in_tyapp_zero *<$> gtycon
  <|> parened (HSY.in_tyapp *<$> gtycon *<*> many' tyvar)
    <|> parened (HSY.in_tuple *<$> l1_separated tyvar comma)
      <|> bracketed (HSY.in_list *<$> tyvar)
        <|> parened (HSY.in_fun *<$> tyvar *<*> r_arrow **> tyvar)
 
(* fatype 	→ 	qtycon atype1 … atypek     	(k  ≥  0) *)
let fatype = HSY.fatype *<$> qtycon *<*> many' ~$atype

(* frtype 	→ 	fatype      *)
(* 	| 	()      *)
let frtype = HSY.frt_fa *<$> fatype <|> form_parened (pure HSY.FRT_unit)

(* ftype 	→ 	frtype      *)
(* 	| 	fatype  →  ftype      *)
let rec ftype () =
  HSY.ft_fun *<$> fatype *<*> r_arrow **> ~$ftype
  <|> HSY.ft_fr *<$> frtype
 
(* callconv 	→ 	ccall | stdcall | cplusplus     	(calling convention) *)
(* 	| 	jvm | dotnet      *)
(* 	| 	 system-specific calling conventions      *)
let conv_words = L.map Symbol.intern
  ["ccall"; "stdcall"; "cplusplus"; "jvm"; "dotnet"]

let safety_words = L.map Symbol.intern
  ["unsafe";  "safe"]

let unsafe = Symbol.intern
let safe   = Symbol.intern "safe"

let callconv = untag_tk "callconv" (function
  | TK.T_VARID s when L.memq s conv_words -> Some s
  | _                                     -> None)

(* impent 	→ 	[string]     	(see Section 8.5.1) *)
let impent = ~?string

(* expent 	→ 	[string]     	(see Section 8.5.1) *)
let expent = ~?string

(* safety 	→ 	unsafe | safe      *)
let safety = untag_tk "safety" (function
  | TK.T_VARID s when L.memq s safety_words -> Some s
  | _                                       -> None)

(* fdecl 	→ 	import callconv [safety] impent var :: ftype     	(define varibale) *)
(* 	| 	export callconv expent var :: ftype     	(expose variable) *)
let fdecl =
  HSY.fo_import *<$> tk_import **|> callconv
    *<*> ~?safety *<*> impent *<*> var *<*> two_colon **> ~$ftype
    <|> HSY.fo_export *<$> tk_export **|> callconv
      *<*> expent *<*> var *<*> two_colon **> ~$ftype


(* 10.5  Context-Free Syntax
   -- expression から参照される要素を先に定義 -- 
   --- パターン
   -- define elements which referred by expression elements. -- 
   --- Pattern
*)

(* pat 	→ 	lpat qconop pat     	(infix constructor) *)
(* 	| 	- (integer | float)     	(negative literal) *)
(* 	| 	lpat      *)
let rec pat () =
  HSY.p_infix *<$> ~$lpat *<*> qconop *<*> ~$pat
  <|> minus **> (HSY.p_neg_int *<$> integer
                               <|> HSY.p_neg_float *<$> float)
    <|> HSY.p_lpat *<$> ~$lpat
 
(* lpat 	→ 	apat      *)
(* 	| 	- (integer | float)     	(negative literal) *)
(* 	| 	gcon apat1 … apatk     	(arity gcon  =  k, k ≥ 1) *)
and     lpat () =
  HSY.lp_gcon *<$> gcon *<*> l1_some ~$apat
  <|> HSY.lp_apat *<$> ~$apat
    <|> minus **> (HSY.lp_neg_int *<$> integer
                   <|> HSY.lp_neg_float *<$> float)

(* apat 	→ 	var [ @ apat]     	(as pattern) *)
(* 	| 	gcon     	(arity gcon  =  0) *)
(* 	| 	qcon { fpat1 , … , fpatk }     	(labeled pattern, k ≥ 0) *)
(* 	| 	literal      *)
(* 	| 	_     	(wildcard) *)
(* 	| 	( pat )     	(parenthesized pattern) *)
(* 	| 	( pat1 , … , patk )     	(tuple pattern, k ≥ 2) *)
(* 	| 	[ pat1 , … , patk ]     	(list pattern, k ≥ 1) *)
(* 	| 	~ apat     	(irrefutable pattern) *)
and     comma_patl_1 () = l1_separated ~$pat comma
and     comma_patl_2 () = l1_separated_2 ~$pat comma
and     apat () =
  HSY.ap_var *<$> var *<*> ~?(just_tk TK.KS_AT **> ~$apat)
  <|> HSY.ap_qcon *<$> qcon *<*> braced (separated ~$fpat comma)
    (* Simpler syntax must be tried later *)
    (* より単純な文法は後にチェックしなければならない *)
    (* * gcon は labeled pattern の qcon にマッチしてしまう *)
    <|> HSY.ap_gcon *<$> gcon
      <|> HSY.ap_lit *<$> literal
        <|> HSY.ap_all *<$> just_tk TK.K_WILDCARD
          <|> HSY.ap_paren *<$> parened ~$pat
            <|> HSY.ap_tuple *<$> parened ~$comma_patl_2
              <|> HSY.ap_list *<$> bracketed ~$comma_patl_1
                <|> HSY.ap_irr *<$> just_tk TK.KS_TILDE **> ~$apat
 
(* fpat 	→ 	qvar = pat      *)
and     fpat () = HSY.fpat *<$> qvar *<*> eq **> ~$pat


(* funlhs 	→ 	var apat { apat }      *)
(* 	| 	pat varop pat      *)
(* 	| 	( funlhs ) apat { apat }      *)
let rec funlhs () =
  HSY.fl_var *<$> var *<*> l1_some ~$apat
  <|> HSY.fl_op *<$> ~$pat *<*> varop *<*> ~$pat
    <|> HSY.fl_nest *<$> parened ~$funlhs *<*> l1_some ~$apat


(* 10.5  Context-Free Syntax
   -- top から参照される要素を先に定義 -- 
   --- exp
   -- define elements which referred by top elements. -- 
   --- exp
*)

(* [where decls] *)
let rec opt_where_decls () = ~?(where **> ~$decls)
and     let_decls () = let_ **|> ~$decls

(* rhs 	→ 	= exp [where decls]      *)
(* 	| 	gdrhs [where decls]      *)
and     rhs () =
  HSY.rhs_exp *<$> eq **> ~$exp *<*> ~$opt_where_decls
  <|> HSY.rhs_gd *<$> ~$gdrhs *<*> ~$opt_where_decls
 
(* gdrhs 	→ 	guards = exp [gdrhs]      *)
and     gdrhs () = HSY.gdrhs *<$> l1_some (HSY.gdrhs_pair
                                           *<$> ~$guards
                                           *<*> eq **> ~$exp)
 
(* guards 	→ 	| guard1, …, guardn     	(n ≥ 1) *)
and     guards () =
  bar **> l1_separated ~$guard comma
  
(* guard 	→ 	pat <- infixexp     	(pattern guard) *)
(* 	| 	let decls     	(local declaration) *)
(* 	| 	infixexp     	(boolean guard) *)
and     guard () =
  HSY.gu_pat *<$> ~$pat *<*> l_arrow **> ~$infixexp
  <|> HSY.gu_let *<$> ~$let_decls
    <|> HSY.gu_exp *<$> ~$infixexp

(* exp 	→ 	infixexp :: [context =>] type     	(expression type signature) *)
(* 	| 	infixexp      *)
and     exp () =
  HSY.exp
  *<$> ~$infixexp
  *<*> ~?(two_colon **|> (HSY.exp_type *<$> may_be_context *<*> ~$type_))
 
(* infixexp 	→ 	lexp qop infixexp     	(infix operator application) *)
(* 	| 	- infixexp     	(prefix negation) *)
(* 	| 	lexp      *)
and     infixexp () = HSY.op_app *<$> ~$lexp *<*> qop *<*> ~$infixexp
  <|> HSY.neg *<$> minus **|> ~$infixexp <|> HSY.lexp *<$> ~$lexp

(* lexp 	→ 	\ apat1 … apatn -> exp     	(lambda abstraction, n ≥ 1) *)
(* 	| 	let decls in exp     	(let expression) *)
(* 	| 	if exp [;] then exp [;] else exp     	(conditional) *)
(* 	| 	case exp of { alts }     	(case expression) *)
(* 	| 	do { stmts }     	(do expression) *)
(* 	| 	fexp      *)
and     lexp () =
  HSY.lambda
  *<$> just_tk TK.KS_B_SLASH **|> l1_some ~$apat *<*> r_arrow **> ~$exp
    <|> HSY.let_ *<$> ~$let_decls *<*> just_tk TK.K_IN **|> ~$exp
      <|> just_tk TK.K_IF **|> (HSY.if_ *<$> ~$exp **< opt_semi
                                *<*> just_tk TK.K_THEN **|> ~$exp **< opt_semi
                                *<*> just_tk TK.K_ELSE **|> ~$exp)
        <|> just_tk TK.K_CASE **|> (HSY.case *<$>
                                      ~$exp *<*>
                                      just_tk TK.K_OF **|> shift_braced ~$alts)
          <|> HSY.do_ *<$> just_tk TK.K_DO **|> shift_braced ~$stmts
            <|> HSY.fexp *<$> ~$fexp

(* fexp 	→ 	[fexp] aexp     	(function application) *)
and     fexp () = HSY.fexp_of_aexp_list *<$> l1_some ~$aexp
 
(* aexp 	→ 	qvar     	(variable) *)
(* 	| 	gcon     	(general constructor) *)
(* 	| 	literal      *)
(* 	| 	( exp )     	(parenthesized expression) *)
(* 	| 	( exp1 , … , expk )     	(tuple, k ≥ 2) *)
(* 	| 	[ exp1 , … , expk ]     	(list, k ≥ 1) *)
(* 	| 	[ exp1 [, exp2] .. [exp3] ]     	(arithmetic sequence) *)
(* 	| 	[ exp | qual1 , … , qualn ]     	(list comprehension, n ≥ 1) *)
(* 	| 	( infixexp qop )     	(left section) *)
(* 	| 	( qop⟨-⟩ infixexp )     	(right section) *)
(* 	| 	qcon { fbind1 , … , fbindn }     	(labeled construction, n ≥ 0) *)
(* 	| 	aexp⟨qcon⟩ { fbind1 , … , fbindn }     	(labeled update, n  ≥  1) *)
and     comma_expl_1 () = l1_separated ~$exp comma
and     comma_expl_2 () = l1_separated_2 ~$exp comma
and     aexp_without_lu () = 
  HSY.lbl_cons *<$> qcon *<*> braced (separated ~$fbind comma)
  (* <|> (HSY.lbl_upd *<$> (~! qcon **> ~$aexp) *<*> braced (l1_separated (~$fbind) comma)) *)
  (* Simpler syntax must be tried later *)
  (* 他の文法の prefix になっている文法は後にチェックしなければならない *)
  (* * gcon は labeled construction の qcon にマッチしてしまう *)
  (* * var は labeled construction の qcon にマッチしてしまう
       が そもそも左再帰なので除去するために labeled update だけ分けた   *)
  <|> HSY.var *<$> qvar <|> HSY.con *<$> gcon <|> HSY.lit *<$> literal
    <|> HSY.paren *<$> parened ~$exp
      <|> HSY.tuple *<$> parened ~$comma_expl_2
        <|> HSY.list *<$> bracketed ~$comma_expl_1
          <|> bracketed (HSY.aseq *<$> ~$exp *<*> ~?(comma **> ~$exp) *<*> dotdot **> ~? ~$exp)
            <|> bracketed (HSY.comp
                           *<$> ~$exp
                           *<*> bar **|> l1_separated ~$qual comma)
              <|> parened (HSY.left_sec *<$> ~$infixexp *<*> qop)
                <|> parened (HSY.right_sec *<$> ~!minus **> qop *<*> ~$infixexp)


and    braced_fbind_list_1 () = braced (l1_separated (~$fbind) comma)

and    aexp () =
  HSY.lbl_upd_of_fbinds_list
  *<$> ~!qcon **> ~$aexp_without_lu
  *<*> some ~$braced_fbind_list_1
  <|> ~$aexp_without_lu

(* qual 	→ 	pat <- exp     	(generator) *)
(* 	| 	let decls     	(local declaration) *)
(* 	| 	exp     	(guard) *)
and     qual () =
  HSY.q_gen *<$> ~$pat *<*> l_arrow **> ~$exp
  <|> HSY.q_let *<$> ~$let_decls
    <|> HSY.q_exp *<$> ~$exp
 
(* alts 	→ 	alt1 ; … ; altn     	(n ≥ 1) *)
and     alts () = l1_separated ~$alt semi
(* alt 	→ 	pat -> exp [where decls]      *)
(* 	| 	pat gdpat [where decls]      *)
(* 	| 	    	(empty alternative) *)
and     alt () =
  HSY.al_pat *<$> ~$pat *<*> r_arrow **> ~$exp *<*> ~$opt_where_decls
  <|> HSY.al_gdpat *<$> ~$pat *<*> ~$gdpat *<*> ~$opt_where_decls
    <|> pure_with_dummy_region HSY.AL_empty

(* gdpat 	→ 	guards -> exp [ gdpat ]      *)
and     gdpat () = l1_some (HSY.gp_gdpat *<$> ~$guards *<*> r_arrow **> ~$exp)
 
(* stmts 	→ 	stmt1 … stmtn exp [;]     	(n ≥ 0) *)
and     stmts () =
  HSY.stmts_cons *<$> ~$stmt *<*> ~$stmts
  <|> HSY.stmts_cons_nil *<$> ~$exp **< opt_semi

(* stmt 	→ 	exp ;      *)
(* 	| 	pat <- exp ;      *)
(* 	| 	let decls ;      *)
(* 	| 	;     	(empty statement) *)
and     stmt () =
  (HSY.st_act *<$> ~$pat *<*> l_arrow **> ~$exp
   <|> HSY.st_exp *<$> ~$exp
     <|> HSY.st_let_ *<$> ~$let_decls) **< semi
  <|> HSY.st_empty *<$> semi
 
(* fbind 	→ 	qvar = exp      *)
and     fbind () = HSY.fbind *<$> qvar *<*> eq **> ~$exp
 

(* 10.5  Context-Free Syntax *)

(* gendecl 	→ 	vars :: [context =>] type     	(type signature) *)
(* 	| 	fixity [integer] ops     	(fixity declaration) *)
(* 	| 	    	(empty declaration) *)
and     gendecl () =
  HSY.gd_vars *<$> vars *<*> two_colon **> may_be_context *<*> ~$type_
  <|> HSY.gd_fixity *<$> fixity *<*> ~?fixity_int *<*> ops
    <|> pure_with_dummy_region HSY.GD_empty **< ~&(semi <|> r_brace)

(* Decls *)

(* decls 	→ 	{ decl1 ; … ; decln }     	(n ≥ 0) *)
(***
    gendecl の (empty declaration) 対策
    against (empty declaration) of gendecl
    decl の後にあるのは semi か r_brace
    decl is followed by semi or r_brace
***)
and     decls () = shift_braced (separated ~$decl semi) 

(* decl 	→ 	gendecl      *)
(* 	| 	(funlhs | pat) rhs      *)
and     decl () =
  HSY.d_val
  *<$> (HSY.lhs_fun *<$> ~$funlhs <|> HSY.lhs_pat *<$> ~$pat)
  *<*> ~$rhs
  <|> HSY.d_gen *<$> ~$gendecl (* gendeclにはemptyがあるので後 *)

(* cdecl 	→ 	gendecl      *)
(* 	| 	(funlhs | var) rhs      *)
let     cdecl () =
  HSY.cd_val
  *<$> (HSY.lhs_fun *<$> ~$funlhs <|> HSY.lhs_pat *<$> ~$pat)
  *<*> ~$rhs
  <|> HSY.cd_gen *<$> ~$gendecl (* gendeclにはemptyがあるので後 *)

(* cdecls 	→ 	{ cdecl1 ; … ; cdecln }     	(n ≥ 0) *)
(***
    gendecl の (empty declaration) 対策
    against (empty declaration) of gendecl
    cdecl の後にあるのは semi か r_brace
    cdecl is followed by semi or r_brace
***)
let cdecls = braced (separated ~$cdecl semi)

(* idecl 	→ 	(funlhs | var) rhs      *)
(* 	| 	    	(empty) *)
let     idecl () =
  HSY.id_val
  *<$> (HSY.lhs_fun *<$> ~$funlhs <|> HSY.lhs_pat *<$> ~$pat)
  *<*> ~$rhs
  <|> pure_with_dummy_region HSY.ID_empty
 
(* idecls 	→ 	{ idecl1 ; … ; idecln }     	(n ≥ 0) *)
let idecls = braced (separated ~$idecl semi)


(* 10.5  Context-Free Syntax *)
(* Top level *)

(* cname 	→ 	var | con      *)
let cname = HSY.cn_var *<$> var <|> HSY.cn_con *<$> con
 
(* export 	→ 	qvar      *)
(* 	| 	qtycon [(..) | ( cname1 , … , cnamen )]     	(n ≥ 0) *)
(* 	| 	qtycls [(..) | ( qvar1 , … , qvarn )]     	(n ≥ 0) *)
(* 	| 	module modid      *)
let export =
  HSY.ex_var *<$> qvar
  <|> HSY.ex_con
    *<$> qtycon
    *<*> ~? (parened (HSY.exf_all *<$> dotdot <|> HSY.exf_list *<$> separated cname comma))
    <|> HSY.ex_cls
      *<$> qtycls
      *<*> ~? (parened (HSY.exf_all *<$> dotdot <|> HSY.exf_list *<$> separated qvar comma))
      <|> HSY.ex_mod *<$> tk_module **|> modid

 
(* exports 	→ 	( export1 , … , exportn [ , ] )     	(n ≥ 0) *)
let exports = parened (separated export comma **< ~?comma)

(* import 	→ 	var      *)
(* 	| 	tycon [ (..) | ( cname1 , … , cnamen )]     	(n ≥ 0) *)
(* 	| 	tycls [(..) | ( var1 , … , varn )]     	(n ≥ 0) *)
let import =
  HSY.im_var *<$> var
  <|> HSY.im_con
    *<$> tycon
    *<*> ~?(parened (HSY.exf_all *<$> dotdot <|> HSY.exf_list *<$> separated cname comma))
    <|> HSY.im_cls
      *<$> tycls
      *<*> ~? (parened (HSY.exf_all *<$> dotdot <|> HSY.exf_list *<$> separated var comma))

(* impspec 	→ 	( import1 , … , importn [ , ] )     	(n ≥ 0) *)
(* 	| 	hiding ( import1 , … , importn [ , ] )     	(n ≥ 0) *)
let impspec =
  HSY.is_imp *<$> parened (separated import comma **< ~?comma)
  <|> HSY.is_hide *<$> just_tk TK.K_HIDING **> parened (separated import comma **< ~?comma)

(* impdecl 	→ 	import [qualified] modid [as modid] [impspec]      *)
(* 	| 	    	(empty declaration) *)
let impdecl =
  tk_import **|> (HSY.imd_imp
                  *<$> ~?(just_tk TK.K_QUALIFIED)
                  *<*> modid
                  *<*> ~?(just_tk TK.K_AS **|> modid)
                  *<*> ~?impspec)
    <|> pure_with_dummy_region HSY.IMD_empty **< ~&(semi <|> r_brace)

(* impdecls 	→ 	impdecl1 ; … ; impdecln     	(n ≥ 1) *)
let impdecls = l1_separated impdecl semi

(* topdecl 	→ 	type simpletype = type      *)
(* 	| 	data [context =>] simpletype [= constrs] [deriving]      *)
(* 	| 	newtype [context =>] simpletype = newconstr [deriving]     *)
(* 	| 	class [scontext =>] tycls tyvar [where cdecls]      *)
(* 	| 	instance [scontext =>] qtycls inst [where idecls]      *)
(* 	| 	default (type1 , … , typen)     	(n ≥ 0) *)
(* 	| 	foreign fdecl      *)
(* 	| 	decl      *)
let topdecl =
  HSY.td_type *<$> just_tk TK.K_TYPE **|> simpletype *<*> eq **> ~$type_
  <|> just_tk TK.K_DATA **|> (HSY.td_data *<$> may_be_context
                              *<*> simpletype *<*> ~?(eq **> constrs) *<*> ~?deriving)
    <|> just_tk TK.K_NEWTYPE **|> (HSY.td_newtype *<$> may_be_context
                                   *<*> simpletype *<*> eq **> newconstr *<*> ~?deriving)
      <|> just_tk TK.K_CLASS **|> (HSY.td_class *<$> may_be_scontext
                                   *<*> tycls *<*> tyvar *<*> ~?(where **> cdecls))
        <|> just_tk TK.K_INSTANCE **|> (HSY.td_instance *<$> may_be_scontext
                                        *<*> qtycls *<*> inst *<*> ~?(where **> idecls))
          <|> HSY.td_default *<$> just_tk TK.K_DEFAULT **|> separated ~$type_ comma
            <|> HSY.td_foreign *<$> just_tk TK.K_FOREIGN **|> fdecl
              <|> HSY.td_decl *<$> ~$decl

(* topdecls 	→ 	topdecl1 ; … ; topdecln     	(n ≥ 0) *)
(***
    gendecl の (empty declaration) 対策
    against (empty declaration) of gendecl
    topdecl の後にあるのは semi か r_brace
    topdecl is followed by semi or r_brace
***)
let topdecls = separated topdecl semi

(* body 	→ 	{ impdecls ; topdecls }      *)
(* 	| 	{ impdecls }      *)
(* 	| 	{ topdecls }      *)
(***
    gendecl の (empty declaration) 対策
    against (empty declaration) of gendecl
    topdecls の後にあるのは semi か r_brace
    topdecls is followed by semi or r_brace
***)
let body =
  braced (HSY.body *<$> impdecls *<*> semi **> topdecls
          <|> HSY.body_no_top *<$> impdecls
            <|> HSY.body_no_imp *<$> topdecls)

(* module 	 → 	module modid [exports] where body       *)
(* 	| 	body      *)
let module_ =
  (HSY.module_ HPST.begin_parse_module
  *<$> tk_module **|> modid *<*> ~?exports *<*> where **> body
    <|> HSY.module_main HPST.begin_parse_module *<$> body)
  **< just_tk TK.EOF

let drop_any    = pred_tk "drop_any" (fun _ -> true)

let test_any : TK.t parser = any

let test_any2 : TK.t parser = any **> any
let test_any3 : TK.t parser = any **> any **> any
let test_any4 : TK.t parser = any **> any **> any **> any
let test_any5 : TK.t parser = any **> any **> any **> any **> any

let test_anys : (TK.type_ list * TK.region) parser = some' (any)

let test_id = drop_any *>
  some (qvar <|> gconsym <|> qconop <|> qvarop)

(*  *)
let test_decl : (HSY.infexp HSY.decl * TK.region) parser =
  ~$decl

let test_funlhs : (HSY.funlhs * TK.region) parser =
  ~$funlhs

let test_rhs : (HSY.infexp HSY.rhs * TK.region) parser =
  ~$rhs

let test_exp : (HSY.infexp HSY.exp * TK.region) parser =
  ~$exp

let test_lexp : (HSY.infexp HSY.lexp * TK.region) parser =
  ~$lexp

let test_type : (HSY.type_ * TK.region) parser =
  ~$type_

let test_apat : (HSY.pat HSY.apat * TK.region) parser =
  ~$apat

let test_opt_where_decls = ~$opt_where_decls

let test_decls : (HSY.infexp HSY.decls * TK.region) parser =
  ~$decls

let test_impdecls_semi = impdecls **< semi

let test_imptop =
  HSY.body *<$> impdecls *<*> semi **> topdecls
  <|> HSY.body_no_top *<$> impdecls
    <|> HSY.body_no_imp *<$> topdecls

let test_lbr_imptop =
  l_brace **> test_imptop

let test_lbr_imptop_rbr =
  test_lbr_imptop **< r_brace

let test_decls_cont : (HSY.infexp HSY.decls * TK.region) parser =
  separated ~$decl semi

let test_bid = braced (qvar <|> gconsym <|> qconop <|> qvarop)

let test_bbody = braced body

