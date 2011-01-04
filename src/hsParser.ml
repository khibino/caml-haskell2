
module TK = Token

module L = List

module HSY = HsSyntax
module HPST = HsParserState

module Combinator = ParserDriver.Combinator(Token)
open Combinator

module D = ParserDerive

type 'e dparser = (D.deriv, 'e exp) parser

let any = D.token

let pred : (token -> bool) -> token dparser =
  to_satisfy any

let just : token -> token dparser =
  fun tk d -> to_just any tk d

let untag : (token -> 'e option)-> 'e dparser =
  fun tk d -> to_untag any tk d

let qual_id_tk f =
  HSY.qual_id *<$> untag f

let pure' v = pure (return' v)

let raw_tk_module = just TK.K_MODULE
let raw_tk_import = just TK.K_IMPORT
let raw_tk_export = just TK.K_EXPORT
let raw_let_  = just TK.K_LET
let raw_where = just TK.K_WHERE
let raw_comma = just TK.SP_COMMA
let raw_semi  = just TK.SP_SEMI
let raw_eq      = just TK.KS_EQ
let raw_minus   = just TK.KS_MINUS
let raw_exclam  = just TK.KS_EXCLAM
let raw_two_colon = just TK.KS_2_COLON
let raw_dotdot  = just TK.KS_DOTDOT
let raw_bar = just TK.KS_BAR
let raw_l_arrow = just TK.KS_L_ARROW
let raw_r_arrow = just TK.KS_R_ARROW
let raw_r_w_arrow = just TK.KS_R_W_ARROW
let raw_l_paren = just TK.SP_LEFT_PAREN
let raw_r_paren = just TK.SP_RIGHT_PAREN
let raw_l_brace = just TK.SP_LEFT_BRACE
let raw_r_brace = just TK.SP_RIGHT_BRACE

(* let match_or_shift_rb = match_or_shift r_brace *)

let raw_opt_semi = ~?D.semi
let raw_opt_exclam = ~?D.exclam

(* 汎用の構成子 *)
(*   構文構造の並び、挟まれる構造、0以上のリスト、1以上のリスト *)

let between a b p = a **> p **< b

let parened    p = between D.l_paren                    D.r_paren   p
let bracketed  p = between (just TK.SP_LEFT_BRACKET) (just TK.SP_RIGHT_BRACKET) p
let braced     p = between D.l_brace                    D.r_brace   p
(* let shift_braced  p = between l_brace                   match_or_shift_rb   p *)
let backquoted p = between (just TK.SP_B_QUOTE)      (just TK.SP_B_QUOTE)       p

  (* 長さ1以上のリスト 意図的に0以上のリストと型が異なるようにしている *)
let l1_some : (D.deriv, 'e exp) parser -> (D.deriv, 'e Data.l1_list exp) parser =
  fun p -> Data.l1_list_cons *<$> p *<*> many p

let rec l1_separated :
    (D.deriv, 'e0 exp) parser -> (D.deriv, 'e1 exp) parser
    -> (D.deriv, 'e0 Data.l1_list exp) parser = fun a d ->
  Data.l1_cons *<$> a *<*> d **> ~$(fun () -> l1_separated a d)
  <|> Data.l1_cons_nil *<$> a

let l1_separated_2 a d =
  Data.l1_cons *<$> a *<*> d **> l1_separated a d

  (* リストとなる構文要素 - 長さ0でも可 - 位置情報付き *)
let some = Combinator.some
let many = Combinator.many

let separated a d =
  Data.l1_list *<$> l1_separated a d <|> pure' []

(* conid 		(constructors)      *)
(* conid は dotted_conid から使われている *)
(* conid is used by dotted_conid above *)
(* Qualified constructor or module id is ambiguous *)
let raw_conid = untag (function | TK.T_CONID s -> Some s | _ -> None)
let raw_dotted_conid = untag (function | TK.T_DOT_CONID s  -> Some s | _ -> None) <|> D.conid

(* 10.2  Lexical Syntax *)

(* literal 	 → 	integer | float | char | string      *)
let raw_literal = untag (function
  | TK.L_CHAR cp   -> Some (HSY.Char cp)
  | TK.L_STRING ca -> Some (HSY.Str ca)
  | TK.L_INTEGER i -> Some (HSY.Int i)
  | TK.L_FLOAT f   -> Some (HSY.Flo f)
  | _              -> None)

(* varsym 	 → 	( symbol⟨:⟩ {symbol} )⟨reservedop | dashes⟩      *)
let raw_varsym = untag (function
  | TK.T_VARSYM s -> Some s
  | TK.KS_PLUS    -> Some HSY.sym_plus
  | TK.KS_MINUS   -> Some HSY.sym_minus
  | TK.KS_EXCLAM  -> Some HSY.sym_exclam
  | _             -> None)
(* consym 	→ 	( : {symbol}){reservedop}      *)
let raw_consym = untag (function | TK.T_CONSYM s -> Some s | _ -> None)


(* varid 	 	 (variables)      *)
let raw_varid = untag (function
  | TK.T_VARID s   -> Some s
  | TK.K_AS        -> Some HSY.sym_as
  | TK.K_QUALIFIED -> Some HSY.sym_qualified
  | TK.K_HIDING    -> Some HSY.sym_hiding
  | TK.K_EXPORT    -> Some HSY.sym_export
  | _              -> None)

(* tyvar 	→ 	varid     	(type variables) *)
let raw_tyvar = D.varid
(* tycon 	→ 	conid     	(type constructors) *)
let raw_tycon = D.conid
(* tycls 	→ 	conid     	(type classes) *)
let raw_tycls = D.conid
(* modid 	→ 	{conid .} conid     	(modules) *)
let raw_modid = D.dotted_conid

(* qvarid 	→ 	[ modid . ] varid      *)
let raw_qvarid = qual_id_tk (function
  | TK.T_MOD_VARID p  -> Some p
  | _ -> None)
  <|> HPST.q_not_qual *<$> D.varid
(* qconid 	→ 	[ modid . ] conid      *)
let raw_qconid  = HPST.sym_to_qconid *<$> D.dotted_conid
(* qtycon 	→ 	[ modid . ] tycon      *)
let raw_qtycon  = qual_id_tk (function
  | TK.T_DOT_CONID s  -> Some (TK.syms_of_qstring (Symbol.name s))
  | _ -> None)
  <|> HPST.q_not_qual *<$> D.tycon
(* qtycls 	→ 	[ modid . ] tycls      *)
let raw_qtycls  = qual_id_tk (function
  | TK.T_DOT_CONID s  -> Some (TK.syms_of_qstring (Symbol.name s))
  | _ -> None)
  <|> HPST.q_not_qual *<$> D.tycls
(* qvarsym 	→ 	[ modid . ] varsym      *)
let raw_qvarsym = qual_id_tk (function
  | TK.T_MOD_VARSYM p -> Some p
  | _ -> None)
  <|> HPST.q_not_qual *<$> D.varsym
(* qconsym 	→ 	[ modid . ] consym      *)
let raw_qconsym = qual_id_tk (function
  | TK.T_MOD_CONSYM p -> Some p
  | _ -> None)
  <|> HPST.q_not_qual *<$> D.consym

let raw_string  = untag (function | TK.L_STRING ca -> Some ca | _ -> None)
let raw_integer = untag (function | TK.L_INTEGER i -> Some i | _ -> None)
let raw_float   = untag (function | TK.L_FLOAT f -> Some f | _ -> None)

let raw_fixity_int = Int64.to_int *<$> untag (function
  | TK.L_INTEGER i when 0L <= i && i <= 9L -> Some i
  | TK.L_INTEGER i -> (* generate error infomation ; *) None
  | _ -> None)

(* 10.5  Context-Free Syntax
   -- 変数とシンボルの定義は他から参照されるので先に定義 -- 
   -- variable and symbol section -- 
*)

(* var 	→ 	varid | ( varsym )     	(variable) *)
let raw_var = D.varid <|> parened D.varsym

(* qvar 	→ 	qvarid | ( qvarsym )     	(qualified variable) *)
let raw_qvar = D.qvarid <|> parened D.qvarsym

(* con 	→ 	conid | ( consym )     	(constructor) *)
let raw_con = D.conid <|> parened D.consym

(* gconsym 	→ 	: | qconsym      *)
(* gconsym は qcon から使われている *)
(* gconsym is used by qcon above *)
let raw_gconsym = HSY.tk_id_colon *<$> just TK.KS_COLON <|> D.qconsym
(* qcon 	→ 	qconid | ( gconsym )     	(qualified constructor) *)
let raw_qcon = D.qconid <|> parened D.gconsym

(* varop 	→ 	varsym | `  varid `     	(variable operator) *)
let raw_varop = D.varsym <|> backquoted D.varid
(* qvarop 	→ 	qvarsym | `  qvarid `     	(qualified variable operator) *)
let raw_qvarop = D.qvarsym <|> backquoted D.qvarid
(* conop 	→ 	consym | `  conid `     	(constructor operator) *)
let raw_conop = D.consym <|> backquoted D.conid
(* qconop 	→ 	gconsym | `  qconid `     	(qualified constructor operator) *)
let raw_qconop = D.gconsym <|> backquoted D.qconid
(* op 	→ 	varop | conop     	(operator) *)
let raw_op = D.varop <|> D.conop
(* qop 	→ 	qvarop | qconop     	(qualified operator) *)
let raw_qop = D.qvarop <|> D.qconop

(* gcon 	→ 	()      *)
(* 	| 	[]      *)
(* 	| 	(,{,})      *)
(* 	| 	qcon      *)
let     raw_commas1 = D.comma **> (succ *<$> D.commas1 <|> pure' 1)
let raw_gcon =
  parened (pure' HSY.id_unit) <|> bracketed (pure' HSY.id_null_list)
    <|> parened (HSY.id_tuple *<$> D.commas1)
      <|> D.qcon


(* ops 	→ 	op1 , … , opn     	(n ≥ 1) *)
let raw_ops = l1_separated D.op D.comma

(* vars 	→ 	var1 , …, varn     	(n ≥ 1) *)
let raw_vars = l1_separated D.var D.comma

(* fixity 	→ 	infixl | infixr | infix      *)
let raw_fixity = untag (function
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
let     raw_gtycon =
  HSY.gt_qtycon *<$> D.qtycon
  <|> parened (pure' HSY.GT_Unit)
    <|> bracketed (pure' HSY.GT_List)
      <|> parened (D.r_arrow **> pure' HSY.GT_Arrow)
        <|> HSY.gt_tuple *<$> parened D.commas1

(* type 	→ 	btype [-> type]     	(function type) *)
let     raw_type_    =
  HSY.type_of_btype_list
  *<$> l1_separated D.btype D.r_arrow
 
(* btype 	→ 	[btype] atype     	(type application) *)
let     raw_btype =
  HSY.btype_of_atype_list
  *<$> l1_some D.atype  (* btypeの左再帰を除去 *)

 
(* atype 	→ 	gtycon      *)
(* 	| 	tyvar      *)
(* 	| 	( type1 , … , typek )     	(tuple type, k ≥ 2) *)
(* 	| 	[ type ]     	(list type) *)
(* 	| 	( type )     	(parenthesized constructor) *)
let     raw_atype =
  HSY.at_gtc *<$> D.gtycon
  <|> HSY.at_tyvar *<$> D.tyvar
    <|> parened (HSY.at_tuple
                 *<$> l1_separated_2 D.type_ D.comma)
      <|> bracketed (HSY.at_list *<$> D.type_)
        <|> parened (HSY.at_paren *<$> D.type_)

(* class 	→ 	qtycls tyvar      *)
(* 	| 	qtycls ( tyvar atype1 … atypen )     	(n ≥ 1) *)
let raw_class_ =
  HSY.class_tval *<$> D.qtycls *<*> D.tyvar
  <|> HSY.class_tapp *<$> D.qtycls *<*> D.l_paren **> D.tyvar *<*> l1_some D.atype **< D.r_paren

(* context 	→ 	class      *)
(* 	| 	( class1 , … , classn )     	(n ≥ 0) *)
let raw_context =  Data.cons_nil *<$> D.class_
  <|> parened (separated D.class_ D.comma)

(* [context =>] *)
let raw_may_be_context = ~?(D.context **< D.r_w_arrow)

(* simpleclass	→ 	qtycls tyvar      *)
let raw_simpleclass = HSY.simpleclass *<$> D.qtycls *<*> D.tyvar

(* scontext 	→ 	simpleclass      *)
(* 	| 	( simpleclass1 , … , simpleclassn )     	(n ≥ 0) *)
let raw_scontext = Data.cons_nil *<$> D.simpleclass
  <|> parened (separated D.simpleclass D.comma)

(* [scontext =>] *)
let raw_may_be_scontext = ~?(D.scontext **< D.r_w_arrow)
 
(* simpletype 	→ 	tycon tyvar1 … tyvark     	(k ≥ 0) *)
let raw_simpletype = HSY.simpletype *<$> D.tycon *<*> many D.tyvar

(* (btype | ! atype) *)
let raw_constr_arg = HSY.ca_satype *<$> D.exclam **> D.atype <|> HSY.ca_btype *<$> D.btype

(* fielddecl 	→ 	vars :: (type | ! atype)      *)
let raw_fielddecl =
  HSY.fielddecl *<$> D.vars *<*> D.two_colon **> (HSY.cf_type *<$> D.type_
                                                  <|> HSY.cf_satype *<$> D.atype)

(* constr 	→ 	con [!] atype1 … [!] atypek     	(arity con  =  k, k ≥ 0) *)
(* 	| 	(btype | ! atype) conop (btype | ! atype)     	(infix conop) *)
(* 	| 	con { fielddecl1 , … , fielddecln }     	(n ≥ 0) *)
let raw_constr =
  HSY.co_con *<$> D.con *<*> many (HSY.may_banana_atype *<$> D.opt_exclam *<*> D.atype)
  <|> HSY.co_bin *<$> D.constr_arg *<*> D.conop *<*> D.constr_arg
    <|> HSY.co_rec *<$> D.con *<*> separated D.fielddecl D.comma

(* constrs 	→ 	constr1 | … | constrn     	(n ≥ 1) *)
let raw_constrs = l1_separated D.constr D.bar

(* newconstr 	→ 	con atype      *)
(* 	| 	con { var :: type }      *)
let raw_newconstr =
  HSY.nc_con *<$> D.con *<*> D.atype
  <|> HSY.nc_rec *<$> D.con *<*> D.l_brace **> D.var *<*> D.two_colon **> D.type_ **< D.r_brace

(* dclass 	→ 	qtycls      *)
let raw_dclass = D.qtycls
 
(* deriving 	→ 	deriving (dclass | (dclass1, … , dclassn))     	(n ≥ 0) *)
let raw_deriving = just TK.K_DERIVING **> (Data.cons_nil *<$> D.dclass
                                          <|> parened (separated D.dclass D.comma))

(* inst 	→ 	gtycon      *)
(* 	| 	( gtycon tyvar1 … tyvark )     	(k ≥ 0, tyvars distinct) *)
(* 	| 	( tyvar1 , … , tyvark )     	(k ≥ 2, tyvars distinct) *)
(* 	| 	[ tyvar ]      *)
(* 	| 	( tyvar1 -> tyvar2 )     	tyvar1 and tyvar2 distinct *)
let raw_inst =
  HSY.in_tyapp_zero *<$> D.gtycon
  <|> parened (HSY.in_tyapp *<$> D.gtycon *<*> many D.tyvar)
    <|> parened (HSY.in_tuple *<$> l1_separated D.tyvar D.comma)
      <|> bracketed (HSY.in_list *<$> D.tyvar)
        <|> parened (HSY.in_fun *<$> D.tyvar *<*> D.r_arrow **!> D.tyvar)
 
(* fatype 	→ 	qtycon atype1 … atypek     	(k  ≥  0) *)
let raw_fatype = HSY.fatype *<$> D.qtycon *<*> many D.atype

(* frtype 	→ 	fatype      *)
(* 	| 	()      *)
let raw_frtype = HSY.frt_fa *<$> D.fatype <|> parened (pure' HSY.FRT_unit)

(* ftype 	→ 	frtype      *)
(* 	| 	fatype  →  ftype      *)
let     raw_ftype =
  HSY.ft_fun *<$> D.fatype *<*> D.r_arrow **!> D.ftype
  <|> HSY.ft_fr *<$> D.frtype
 
(* callconv 	→ 	ccall | stdcall | cplusplus     	(calling convention) *)
(* 	| 	jvm | dotnet      *)
(* 	| 	 system-specific calling conventions      *)
let conv_words = L.map Symbol.intern
  ["ccall"; "stdcall"; "cplusplus"; "jvm"; "dotnet"]

let safety_words = L.map Symbol.intern
  ["unsafe";  "safe"]

let raw_callconv = untag (function
  | TK.T_VARID s when L.memq s conv_words -> Some s
  | _                                     -> None)

(* impent 	→ 	[string]     	(see Section 8.5.1) *)
let raw_impent = ~?D.string

(* expent 	→ 	[string]     	(see Section 8.5.1) *)
let raw_expent = ~?D.string

(* safety 	→ 	unsafe | safe      *)
let raw_safety = untag (function
  | TK.T_VARID s when L.memq s safety_words -> Some s
  | _                                       -> None)

(* fdecl 	→ 	import callconv [safety] impent var :: ftype     	(define varibale) *)
(* 	| 	export callconv expent var :: ftype     	(expose variable) *)
let raw_fdecl =
  HSY.fo_import *<$> D.tk_import **> D.callconv
    *<*> ~?D.safety *<*> D.impent *<*> D.var *<*> D.two_colon **> D.ftype
    <|> HSY.fo_export *<$> D.tk_export **> D.callconv
      *<*> D.expent *<*> D.var *<*> D.two_colon **> D.ftype


(* 10.5  Context-Free Syntax
   -- expression から参照される要素を先に定義 -- 
   --- パターン
   -- define elements which referred by expression elements. -- 
   --- Pattern
*)

(* pat 	→ 	lpat qconop pat     	(infix constructor) *)
(* 	| 	- (integer | float)     	(negative literal) *)
(* 	| 	lpat      *)
let     raw_pat =
  HSY.p_infix *<$> D.lpat *<*> D.qconop *<*> D.pat
  <|> D.minus **> (HSY.p_neg_int *<$> D.integer
                               <|> HSY.p_neg_float *<$> D.float)
    <|> HSY.p_lpat *<$> D.lpat
 
(* lpat 	→ 	apat      *)
(* 	| 	- (integer | float)     	(negative literal) *)
(* 	| 	gcon apat1 … apatk     	(arity gcon  =  k, k ≥ 1) *)
let     raw_lpat =
  HSY.lp_gcon *<$> D.gcon *<*> l1_some D.apat
  <|> HSY.lp_apat *<$> D.apat
    <|> D.minus **> (HSY.lp_neg_int *<$> D.integer
                   <|> HSY.lp_neg_float *<$> D.float)

(* apat 	→ 	var [ @ apat]     	(as pattern) *)
(* 	| 	gcon     	(arity gcon  =  0) *)
(* 	| 	qcon { fpat1 , … , fpatk }     	(labeled pattern, k ≥ 0) *)
(* 	| 	literal      *)
(* 	| 	_     	(wildcard) *)
(* 	| 	( pat )     	(parenthesized pattern) *)
(* 	| 	( pat1 , … , patk )     	(tuple pattern, k ≥ 2) *)
(* 	| 	[ pat1 , … , patk ]     	(list pattern, k ≥ 1) *)
(* 	| 	~ apat     	(irrefutable pattern) *)
let     raw_comma_patl_1 = l1_separated D.pat D.comma
let     raw_comma_patl_2 = l1_separated_2 D.pat D.comma
let     raw_apat =
  HSY.ap_var *<$> D.var *<*> ~?(just TK.KS_AT **> D.apat)
  <|> HSY.ap_qcon *<$> D.qcon *<*> braced (separated D.fpat D.comma)
    (* Simpler syntax must be tried later *)
    (* より単純な文法は後にチェックしなければならない *)
    (* * gcon は labeled pattern の qcon にマッチしてしまう *)
    <|> HSY.ap_gcon *<$> D.gcon
      <|> HSY.ap_lit *<$> D.literal
        <|> HSY.ap_all *<$> just TK.K_WILDCARD
          <|> HSY.ap_paren *<$> parened D.pat
            <|> HSY.ap_tuple *<$> parened D.comma_patl_2
              <|> HSY.ap_list *<$> bracketed D.comma_patl_1
                <|> HSY.ap_irr *<$> just TK.KS_TILDE **> D.apat
 
(* fpat 	→ 	qvar = pat      *)
let     raw_fpat = HSY.fpat *<$> D.qvar *<*> D.eq **> D.pat


(* funlhs 	→ 	var apat { apat }      *)
(* 	| 	pat varop pat      *)
(* 	| 	( funlhs ) apat { apat }      *)
let     raw_funlhs =
  HSY.fl_var *<$> D.var *<*> l1_some D.apat
  <|> HSY.fl_op *<$> D.pat *<*> D.varop *<*> D.pat
    <|> HSY.fl_nest *<$> parened D.funlhs *<*> l1_some D.apat


(* 10.5  Context-Free Syntax
   -- top から参照される要素を先に定義 -- 
   --- exp
   -- define elements which referred by top elements. -- 
   --- exp
*)

(* [where decls] *)
let     raw_opt_where_decls = ~?(D.where **> D.decls)
let     raw_let_decls = D.let_ **> D.decls

(* rhs 	→ 	= exp [where decls]      *)
(* 	| 	gdrhs [where decls]      *)
let     raw_rhs =
  HSY.rhs_exp *<$> D.eq **> D.exp *<*> D.opt_where_decls
  <|> HSY.rhs_gd *<$> D.gdrhs *<*> D.opt_where_decls
 
(* gdrhs 	→ 	guards = exp [gdrhs]      *)
let     raw_gdrhs = HSY.gdrhs *<$> l1_some (HSY.gdrhs_pair
                                           *<$> D.guards
                                           *<*> D.eq **> D.exp)
 
(* guards 	→ 	| guard1, …, guardn     	(n ≥ 1) *)
let     raw_guards =
  D.bar **> l1_separated D.guard D.comma
  
(* guard 	→ 	pat <- infixexp     	(pattern guard) *)
(* 	| 	let decls     	(local declaration) *)
(* 	| 	infixexp     	(boolean guard) *)
let     raw_guard =
  HSY.gu_pat *<$> D.pat *<*> D.l_arrow **> D.infixexp
  <|> HSY.gu_let *<$> D.let_decls
    <|> HSY.gu_exp *<$> D.infixexp

(* exp 	→ 	infixexp :: [context =>] type     	(expression type signature) *)
(* 	| 	infixexp      *)
let     raw_exp =
  HSY.exp
  *<$> D.infixexp
  *<*> ~?(D.two_colon **> (HSY.exp_type *<$> D.may_be_context *<*> D.type_))
 
(* infixexp 	→ 	lexp qop infixexp     	(infix operator application) *)
(* 	| 	- infixexp     	(prefix negation) *)
(* 	| 	lexp      *)
let     raw_infixexp = HSY.op_app *<$> D.lexp *<*> D.qop *<*> D.infixexp
  <|> HSY.neg *<$> D.minus **> D.infixexp <|> HSY.lexp *<$> D.lexp


(* fexp 	→ 	[fexp] aexp     	(function application) *)
let     raw_fexp = HSY.fexp_of_aexp_list *<$> l1_some D.aexp
 
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
let     raw_comma_expl_1 = l1_separated D.exp D.comma
let     raw_comma_expl_2 = l1_separated_2 D.exp D.comma
let     raw_aexp_without_lu = 
  HSY.lbl_cons *<$> D.qcon *<*> braced (separated D.fbind D.comma)
  (* <|> (HSY.lbl_upd *<$> (~! qcon **> D.aexp) *<*> braced (l1_separated (D.fbind) D.comma)) *)
  (* Simpler syntax must be tried later *)
  (* 他の文法の prefix になっている文法は後にチェックしなければならない *)
  (* * gcon は labeled construction の qcon にマッチしてしまう *)
  (* * var は labeled construction の qcon にマッチしてしまう
       が そもそも左再帰なので除去するために labeled update だけ分けた   *)
  <|> HSY.var *<$> D.qvar <|> HSY.con *<$> D.gcon <|> HSY.lit *<$> D.literal
    <|> HSY.paren *<$> parened D.exp
      <|> HSY.tuple *<$> parened D.comma_expl_2
        <|> HSY.list *<$> bracketed D.comma_expl_1
          <|> bracketed (HSY.aseq *<$> D.exp *<*> ~?(D.comma **> D.exp) *<*> D.dotdot **> ~? D.exp)
            <|> bracketed (HSY.comp
                           *<$> D.exp
                           *<*> D.bar **> l1_separated D.qual D.comma)
              <|> parened (HSY.left_sec *<$> D.infixexp *<*> D.qop)
                <|> parened (HSY.right_sec *<$> ~!D.minus **> D.qop *<*> D.infixexp)


let    raw_braced_fbind_list_1 = braced (l1_separated (D.fbind) D.comma)

let    raw_aexp =
  HSY.lbl_upd_of_fbinds_list
  *<$> ~!D.qcon **> D.aexp_without_lu
  *<*> some D.braced_fbind_list_1
  <|> D.aexp_without_lu

(* qual 	→ 	pat <- exp     	(generator) *)
(* 	| 	let decls     	(local declaration) *)
(* 	| 	exp     	(guard) *)
let     raw_qual =
  HSY.q_gen *<$> D.pat *<*> D.l_arrow **> D.exp
  <|> HSY.q_let *<$> D.let_decls
    <|> HSY.q_exp *<$> D.exp
 
(* alts 	→ 	alt1 ; … ; altn     	(n ≥ 1) *)
let     raw_alts = l1_separated D.alt D.semi
(* alt 	→ 	pat -> exp [where decls]      *)
(* 	| 	pat gdpat [where decls]      *)
(* 	| 	    	(empty alternative) *)
let     raw_alt =
  HSY.al_pat *<$> D.pat *<*> D.r_arrow **> D.exp *<*> D.opt_where_decls
  <|> HSY.al_gdpat *<$> D.pat *<*> D.gdpat *<*> D.opt_where_decls
    <|> pure' HSY.AL_empty

(* gdpat 	→ 	guards -> exp [ gdpat ]      *)
let     raw_gdpat = l1_some (HSY.gp_gdpat *<$> D.guards *<*> D.r_arrow **> D.exp)
 
(* stmts 	→ 	stmt1 … stmtn exp [;]     	(n ≥ 0) *)
let     raw_stmts =
  HSY.stmts_cons *<$> D.stmt *<*> D.stmts
  <|> HSY.stmts_cons_nil *<$> D.exp **< D.opt_semi

(* stmt 	→ 	exp ;      *)
(* 	| 	pat <- exp ;      *)
(* 	| 	let decls ;      *)
(* 	| 	;     	(empty statement) *)
let     raw_stmt =
  (HSY.st_act *<$> D.pat *<*> D.l_arrow **> D.exp
   <|> HSY.st_exp *<$> D.exp
     <|> HSY.st_let_ *<$> D.let_decls) **< D.semi
  <|> HSY.st_empty *<$> D.semi
 
(* fbind 	→ 	qvar = exp      *)
let     raw_fbind = HSY.fbind *<$> D.qvar *<*> D.eq **> D.exp
 

(* 10.5  Context-Free Syntax *)

(* gendecl 	→ 	vars :: [context =>] type     	(type signature) *)
(* 	| 	fixity [integer] ops     	(fixity declaration) *)
(* 	| 	    	(empty declaration) *)
let     raw_gendecl =
  HSY.gd_vars *<$> D.vars *<*> D.two_colon **> D.may_be_context *<*> D.type_
  <|> HSY.gd_fixity *<$> D.fixity *<*> ~?D.fixity_int *<*> D.ops
    <|> pure' HSY.GD_empty **< ~&(D.semi <|> D.r_brace)


(* decl 	→ 	gendecl      *)
(* 	| 	(funlhs | pat) rhs      *)
let     raw_decl =
  HSY.d_val
  *<$> (HSY.lhs_fun *<$> D.funlhs <|> HSY.lhs_pat *<$> D.pat)
  *<*> D.rhs
  <|> HSY.d_gen *<$> D.gendecl (* gendeclにはemptyがあるので後 *)

(* cdecl 	→ 	gendecl      *)
(* 	| 	(funlhs | var) rhs      *)
let     raw_cdecl =
  HSY.cd_val
  *<$> (HSY.lhs_fun *<$> D.funlhs <|> HSY.lhs_pat *<$> D.pat)
  *<*> D.rhs
  <|> HSY.cd_gen *<$> D.gendecl (* gendeclにはemptyがあるので後 *)

(* cdecls 	→ 	{ cdecl1 ; … ; cdecln }     	(n ≥ 0) *)
(***
    gendecl の (empty declaration) 対策
    against (empty declaration) of gendecl
    cdecl の後にあるのは semi か r_brace
    cdecl is followed by semi or r_brace
***)
let raw_cdecls = braced (separated D.cdecl D.semi)

(* idecl 	→ 	(funlhs | var) rhs      *)
(* 	| 	    	(empty) *)
let     raw_idecl =
  HSY.id_val
  *<$> (HSY.lhs_fun *<$> D.funlhs <|> HSY.lhs_pat *<$> D.pat)
  *<*> D.rhs
  <|> pure' HSY.ID_empty
 
(* idecls 	→ 	{ idecl1 ; … ; idecln }     	(n ≥ 0) *)
let raw_idecls = braced (separated D.idecl D.semi)


(* 10.5  Context-Free Syntax *)
(* Top level *)

(* cname 	→ 	var | con      *)
let raw_cname = HSY.cn_var *<$> D.var <|> HSY.cn_con *<$> D.con
 
(* export 	→ 	qvar      *)
(* 	| 	qtycon [(..) | ( cname1 , … , cnamen )]     	(n ≥ 0) *)
(* 	| 	qtycls [(..) | ( qvar1 , … , qvarn )]     	(n ≥ 0) *)
(* 	| 	module modid      *)
let raw_export =
  HSY.ex_var *<$> D.qvar
  <|> HSY.ex_con
    *<$> D.qtycon
    *<*> ~? (parened (HSY.exf_all *<$> D.dotdot <|> HSY.exf_list *<$> separated D.cname D.comma))
    <|> HSY.ex_cls
      *<$> D.qtycls
      *<*> ~? (parened (HSY.exf_all *<$> D.dotdot <|> HSY.exf_list *<$> separated D.qvar D.comma))
      <|> HSY.ex_mod *<$> D.tk_module **> D.modid

 
(* exports 	→ 	( export1 , … , exportn [ , ] )     	(n ≥ 0) *)
let raw_exports = parened (separated D.export D.comma **< ~?D.comma)

(* import 	→ 	var      *)
(* 	| 	tycon [ (..) | ( cname1 , … , cnamen )]     	(n ≥ 0) *)
(* 	| 	tycls [(..) | ( var1 , … , varn )]     	(n ≥ 0) *)
let raw_import =
  HSY.im_var *<$> D.var
  <|> HSY.im_con
    *<$> D.tycon
    *<*> ~?(parened (HSY.exf_all *<$> D.dotdot <|> HSY.exf_list *<$> separated D.cname D.comma))
    <|> HSY.im_cls
      *<$> D.tycls
      *<*> ~? (parened (HSY.exf_all *<$> D.dotdot <|> HSY.exf_list *<$> separated D.var D.comma))

(* impspec 	→ 	( import1 , … , importn [ , ] )     	(n ≥ 0) *)
(* 	| 	hiding ( import1 , … , importn [ , ] )     	(n ≥ 0) *)
let raw_impspec =
  HSY.is_imp *<$> parened (separated D.import D.comma **< ~?D.comma)
  <|> HSY.is_hide *<$> just TK.K_HIDING **> parened (separated D.import D.comma **< ~?D.comma)

(* impdecl 	→ 	import [qualified] modid [as modid] [impspec]      *)
(* 	| 	    	(empty declaration) *)
let raw_impdecl =
  D.tk_import **> (HSY.imd_imp
                   *<$> ~?(just TK.K_QUALIFIED)
                   *<*> D.modid
                   *<*> ~?(just TK.K_AS **> D.modid)
                   *<*> ~?D.impspec)
    <|> pure' HSY.IMD_empty **< ~&(D.semi <|> D.r_brace)

(* impdecls 	→ 	impdecl1 ; … ; impdecln     	(n ≥ 1) *)
let raw_impdecls = l1_separated D.impdecl D.semi

(* topdecl 	→ 	type simpletype = type      *)
(* 	| 	data [context =>] simpletype [= constrs] [deriving]      *)
(* 	| 	newtype [context =>] simpletype = newconstr [deriving]     *)
(* 	| 	class [scontext =>] tycls tyvar [where cdecls]      *)
(* 	| 	instance [scontext =>] qtycls inst [where idecls]      *)
(* 	| 	default (type1 , … , typen)     	(n ≥ 0) *)
(* 	| 	foreign fdecl      *)
(* 	| 	decl      *)
let raw_topdecl =
  HSY.td_type *<$> just TK.K_TYPE **> D.simpletype *<*> D.eq **> D.type_
  <|> just TK.K_DATA **> (HSY.td_data *<$> D.may_be_context
                              *<*> D.simpletype *<*> ~?(D.eq **> D.constrs) *<*> ~?D.deriving)
    <|> just TK.K_NEWTYPE **> (HSY.td_newtype *<$> D.may_be_context
                                   *<*> D.simpletype *<*> D.eq **> D.newconstr *<*> ~?D.deriving)
      <|> just TK.K_CLASS **> (HSY.td_class *<$> D.may_be_scontext
                                   *<*> D.tycls *<*> D.tyvar *<*> ~?(D.where **> D.cdecls))
        <|> just TK.K_INSTANCE **> (HSY.td_instance *<$> D.may_be_scontext
                                        *<*> D.qtycls *<*> D.inst *<*> ~?(D.where **> D.idecls))
          <|> HSY.td_default *<$> just TK.K_DEFAULT **> separated D.type_ D.comma
            <|> HSY.td_foreign *<$> just TK.K_FOREIGN **> D.fdecl
              <|> HSY.td_decl *<$> D.decl

(* topdecls 	→ 	topdecl1 ; … ; topdecln     	(n ≥ 0) *)
(***
    gendecl の (empty declaration) 対策
    against (empty declaration) of gendecl
    topdecl の後にあるのは semi か r_brace
    topdecl is followed by semi or r_brace
***)
let raw_topdecls = separated D.topdecl D.semi

(* body 	→ 	{ impdecls ; topdecls }      *)
(* 	| 	{ impdecls }      *)
(* 	| 	{ topdecls }      *)
(***
    gendecl の (empty declaration) 対策
    against (empty declaration) of gendecl
    topdecls の後にあるのは semi か r_brace
    topdecls is followed by semi or r_brace
***)
let raw_body =
  braced (HSY.body *<$> D.impdecls *<*> D.semi **> D.topdecls
          <|> HSY.body_no_top *<$> D.impdecls
            <|> HSY.body_no_imp *<$> D.topdecls)

(* module 	 → 	module modid [exports] where body       *)
(* 	| 	body      *)
let raw_module_ =
  (HSY.module_ HPST.begin_parse_module
  *<$> D.tk_module **> D.modid *<*> ~?D.exports *<*> D.where **> D.body
    <|> HSY.module_main HPST.begin_parse_module *<$> D.body)
  **< forget (just TK.EOF)


let rec make_derive tfo =
  (* Data.undefined () *)
  let call p d = Lazy.force (run p (d ())) in
  let fcall fp d = call (fp ()) d in

  let rec d () =
    D.cons_deriv
      { D.token = derive_seq make_derive tfo;
        D.tk_module = lazy (call raw_tk_module d);
        D.tk_import = lazy (call raw_tk_import d);
        D.tk_export = lazy (call raw_tk_export d);
        D.let_ = lazy (call raw_let_ d);
        D.where = lazy (call raw_where d);
        D.comma = lazy (call raw_comma d);
        D.semi = lazy (call raw_semi d);
        D.eq = lazy (call raw_eq d);
        D.minus = lazy (call raw_minus d);
        D.exclam = lazy (call raw_exclam d);
        D.two_colon = lazy (call raw_two_colon d);
        D.dotdot = lazy (call raw_dotdot d);
        D.bar = lazy (call raw_bar d);
        D.l_arrow = lazy (call raw_l_arrow d);
        D.r_arrow = lazy (call raw_r_arrow d);
        D.r_w_arrow = lazy (call raw_r_w_arrow d);
        D.l_paren = lazy (call raw_l_paren d);
        D.r_paren = lazy (call raw_r_paren d);
        D.l_brace = lazy (call raw_l_brace d);
        D.r_brace = lazy (call raw_r_brace d);
        D.r_brace'' = lazy (call r_brace'' d);
        D.opt_semi = lazy (call raw_opt_semi d);
        D.opt_exclam = lazy (call raw_opt_exclam d);

        D.conid = lazy (call raw_conid d);
        D.dotted_conid = lazy (call raw_dotted_conid d);
        D.literal = lazy (call raw_literal d);
        D.varsym = lazy (call raw_varsym d);
        D.consym = lazy (call raw_consym d);
        D.varid = lazy (call raw_varid d);
        D.tyvar = lazy (call raw_tyvar d);
        D.tycon = lazy (call raw_tycon d);
        D.tycls = lazy (call raw_tycls d);
        D.modid = lazy (call raw_modid d);
        D.qvarid = lazy (call raw_qvarid d);
        D.qconid = lazy (call raw_qconid d);
        D.qtycon = lazy (call raw_qtycon d);
        D.qtycls = lazy (call raw_qtycls d);
        D.qvarsym = lazy (call raw_qvarsym d);
        D.qconsym = lazy (call raw_qconsym d);
        D.string = lazy (call raw_string d);
        D.integer = lazy (call raw_integer d);
        D.float = lazy (call raw_float d);
        D.fixity_int = lazy (call raw_fixity_int d);
        D.var = lazy (call raw_var d);
        D.qvar = lazy (call raw_qvar d);
        D.con = lazy (call raw_con d);
        D.gconsym = lazy (call raw_gconsym d);
        D.qcon = lazy (call raw_qcon d);
        D.varop = lazy (call raw_varop d);
        D.qvarop = lazy (call raw_qvarop d);
        D.conop = lazy (call raw_conop d);
        D.qconop = lazy (call raw_qconop d);
        D.op = lazy (call raw_op d);
        D.qop = lazy (call raw_qop d);
        D.commas1 = lazy (call raw_commas1 d);
        D.gcon = lazy (call raw_gcon d);
        D.ops = lazy (call raw_ops d);
        D.vars = lazy (call raw_vars d);
        D.fixity = lazy (call raw_fixity d);
        D.gtycon = lazy (call raw_gtycon d);
        D.type_ = lazy (call raw_type_ d);
        D.btype = lazy (call raw_btype d);
        D.atype = lazy (call raw_atype d);
        D.class_ = lazy (call raw_class_ d);
        D.context = lazy (call raw_context d);
        D.may_be_context = lazy (call raw_may_be_context d);
        D.simpleclass = lazy (call raw_simpleclass d);
        D.scontext = lazy (call raw_scontext d);
        D.may_be_scontext = lazy (call raw_may_be_scontext d);
        D.simpletype = lazy (call raw_simpletype d);
        D.constr_arg = lazy (call raw_constr_arg d);
        D.fielddecl = lazy (call raw_fielddecl d);
        D.constr = lazy (call raw_constr d);
        D.constrs = lazy (call raw_constrs d);
        D.newconstr = lazy (call raw_newconstr d);
        D.dclass = lazy (call raw_dclass d);
        D.deriving = lazy (call raw_deriving d);
        D.inst = lazy (call raw_inst d);
        D.fatype = lazy (call raw_fatype d);
        D.frtype = lazy (call raw_frtype d);
        D.ftype = lazy (call raw_ftype d);
        D.callconv = lazy (call raw_callconv d);
        D.impent = lazy (call raw_impent d);
        D.expent = lazy (call raw_expent d);
        D.safety = lazy (call raw_safety d);
        D.fdecl = lazy (call raw_fdecl d);
        D.pat = lazy (call raw_pat d);
        D.lpat = lazy (call raw_lpat d);
        D.comma_patl_1 = lazy (call raw_comma_patl_1 d);
        D.comma_patl_2 = lazy (call raw_comma_patl_2 d);
        D.apat = lazy (call raw_apat d);
        D.fpat = lazy (call raw_fpat d);
        D.funlhs = lazy (call raw_funlhs d);
        D.opt_where_decls = lazy (call raw_opt_where_decls d);
        D.let_decls = lazy (call raw_let_decls d);
        D.rhs = lazy (call raw_rhs d);
        D.gdrhs = lazy (call raw_gdrhs d);
        D.guards = lazy (call raw_guards d);
        D.guard = lazy (call raw_guard d);
        D.exp = lazy (call raw_exp d);
        D.infixexp = lazy (call raw_infixexp d);
        D.lexp = lazy (fcall raw_lexp d);
        D.fexp = lazy (call raw_fexp d);
        D.comma_expl_1 = lazy (call raw_comma_expl_1 d);
        D.comma_expl_2 = lazy (call raw_comma_expl_2 d);
        D.aexp_without_lu = lazy (call raw_aexp_without_lu d);
        D.braced_fbind_list_1 = lazy (call raw_braced_fbind_list_1 d);
        D.aexp = lazy (call raw_aexp d);
        D.qual = lazy (call raw_qual d);
        D.alts = lazy (call raw_alts d);
        D.alt = lazy (call raw_alt d);
        D.gdpat = lazy (call raw_gdpat d);
        D.stmts = lazy (call raw_stmts d);
        D.stmt = lazy (call raw_stmt d);
        D.fbind = lazy (call raw_fbind d);
        D.gendecl = lazy (call raw_gendecl d);
        D.decls = lazy (fcall raw_decls d);
        D.decl = lazy (call raw_decl d);
        D.cdecl = lazy (call raw_cdecl d);
        D.cdecls = lazy (call raw_cdecls d);
        D.idecl = lazy (call raw_idecl d);
        D.idecls = lazy (call raw_idecls d);
        D.cname = lazy (call raw_cname d);
        D.export = lazy (call raw_export d);
        D.exports = lazy (call raw_exports d);
        D.import = lazy (call raw_import d);
        D.impspec = lazy (call raw_impspec d);
        D.impdecl = lazy (call raw_impdecl d);
        D.impdecls = lazy (call raw_impdecls d);
        D.topdecl = lazy (call raw_topdecl d);
        D.topdecls = lazy (call raw_topdecls d);
        D.body = lazy (call raw_body d);
        D.module_ = lazy (call raw_module_ d);
      }
      tfo

  in d ()

and r_brace'' l = match_or_shift make_derive D.deriv_source D.r_brace l

(* lexp 	→ 	\ apat1 … apatn -> exp     	(lambda abstraction, n ≥ 1) *)
(* 	| 	let decls in exp     	(let expression) *)
(* 	| 	if exp [;] then exp [;] else exp     	(conditional) *)
(* 	| 	case exp of { alts }     	(case expression) *)
(* 	| 	do { stmts }     	(do expression) *)
(* 	| 	fexp      *)
and     raw_lexp () =
  HSY.lambda
  *<$> just TK.KS_B_SLASH **> l1_some D.apat *<*> D.r_arrow **> D.exp
    <|> HSY.let_ *<$> D.let_decls *<*> just TK.K_IN **> D.exp
      <|> just TK.K_IF **> (HSY.if_ *<$> D.exp **< D.opt_semi
                                *<*> just TK.K_THEN **> D.exp **< D.opt_semi
                                *<*> just TK.K_ELSE **> D.exp)
        <|> just TK.K_CASE **> (HSY.case *<$>
                                      D.exp *<*>
                                      just TK.K_OF **> (D.l_brace *> D.alts <* r_brace''))
          <|> HSY.do_ *<$> just TK.K_DO **> (D.l_brace *> D.stmts <* r_brace'')
            <|> HSY.fexp *<$> D.fexp

(* Decls *)

(* decls 	→ 	{ decl1 ; … ; decln }     	(n ≥ 0) *)
(***
    gendecl の (empty declaration) 対策
    against (empty declaration) of gendecl
    decl の後にあるのは semi か r_brace
    decl is followed by semi or r_brace
***)
and     raw_decls () =  D.l_brace *> separated D.decl D.semi <* r_brace''



let test_eof    = forget (just TK.EOF)

let drop_any    = pred (fun _ -> true)

let test_any : token dparser = any

let test_any2 : token dparser = any **> any
let test_any3 : token dparser = any **> any **> any
let test_any4 : token dparser = any **> any **> any **> any
let test_any5 : token dparser = any **> any **> any **> any **> any

let test_anys : token list dparser = some (any)

let test_id = drop_any *>
  some (D.qvar <|> D.gconsym <|> D.qconop <|> D.qvarop)

(*  *)
let test_decl : (HSY.infexp HSY.decl) dparser =
  D.decl

let test_funlhs : (HSY.funlhs) dparser =
  D.funlhs

let test_rhs : (HSY.infexp HSY.rhs) dparser =
  D.rhs

let test_exp : (HSY.infexp HSY.exp) dparser =
  D.exp

let test_lexp : (HSY.infexp HSY.lexp) dparser =
  D.lexp

let test_type : (HSY.type_) dparser =
  D.type_

let test_apat : (HSY.pat HSY.apat) dparser =
  D.apat

let test_opt_where_decls = D.opt_where_decls

let test_decls : (HSY.infexp HSY.decls) dparser =
  D.decls

(* *)
