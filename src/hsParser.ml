
open Simple.Combinator

module L = List

module TK = Token
module HSY = HsSyntax
module HPST = HsParserState

let call = call_parser

let (|.|) f g x = f (g x)

let pred_tk : (TK.typ -> bool) -> (TK.t, TK.t) parser =
  fun f -> pred (f |.| fst)
let just_tk eq = pred_tk ((=) eq)
let untag_tk : (TK.typ -> 'a option) -> (TK.t, 'a * TK.region) parser =
  fun f ->
    untag (fun (tk, reg) ->
      match f tk with
        | Some v -> Some (v, reg)
        | None   -> None)

let qual_id_tk f =
  TK.with_region HSY.qual_id <$> untag_tk f

let pos_dummy = TK.pos (-1) (-1)
let region_dummy = TK.region pos_dummy pos_dummy

let pos_fix_later = pos_dummy
let region_fix_later = TK.region pos_fix_later pos_fix_later
let p_fix_later : (TK.t, unit * TK.region) parser = pure ((), region_fix_later)

(* Qualified constructor or module id is ambiguous *)
let conid = untag_tk (function | TK.T_CONID s -> Some s | _ -> None)
let doted_conid = untag_tk (function | TK.T_DOT_CONID s  -> Some s | _ -> None) <|> conid

(* 10.2  Lexical Syntax *)

(* literal 	 → 	integer | float | char | string      *)
let literal = untag_tk (function
  | TK.L_CHAR cp   -> Some (HSY.Char cp)
  | TK.L_STRING ca -> Some (HSY.Str ca)
  | TK.L_INTEGER i -> Some (HSY.Int i)
  | TK.L_FLOAT f   -> Some (HSY.Flo f)
  | _              -> None)

(* varsym 	 → 	( symbol⟨:⟩ {symbol} ){reservedop | dashes}      *)
let varsym = untag_tk (function
  | TK.T_VARSYM s -> Some s
  | TK.KS_PLUS    -> Some HSY.sym_plus
  | TK.KS_MINUS   -> Some HSY.sym_minus
  | TK.KS_EXCLAM  -> Some HSY.sym_exclam
  | _             -> None)
(* consym 	→ 	( : {symbol}){reservedop}      *)
let consym = untag_tk (function | TK.T_CONSYM s -> Some s | _ -> None)


(* varid 	 	 (variables)      *)
let varid = untag_tk (function
  | TK.T_VARID s   -> Some s
  | TK.K_AS        -> Some HSY.sym_as
  | TK.K_QUALIFIED -> Some HSY.sym_qualified
  | TK.K_HIDING    -> Some HSY.sym_hiding
  | _              -> None)
(* conid 		(constructors)      *)
(* conid は doted_conid から使われている ↑ *)
(* conid is used by doted_conid above *)
(* tyvar 	→ 	varid     	(type variables) *)
let tyvar = varid
(* tycon 	→ 	conid     	(type constructors) *)
let tycon = conid
(* tycls 	→ 	conid     	(type classes) *)
let tycls = conid
(* modid 	→ 	{conid .} conid     	(modules) *)
let modid = doted_conid

(* qvarid 	→ 	[ modid . ] varid      *)
let qvarid : (TK.t, HSY.id * TK.region) parser  = qual_id_tk (function
  | TK.T_MOD_VARID p  -> Some p
  | _ -> None) <|> (HPST.q_not_qual <$> varid)
(* qconid 	→ 	[ modid . ] conid      *)
let qconid  = HSY.sym_to_qconid <$> doted_conid
(* qtycon 	→ 	[ modid . ] tycon      *)
let qtycon  = qual_id_tk (function
  | TK.T_DOT_CONID s  -> Some (TK.syms_of_qstring (Symbol.name s))
  | _ -> None)
  <|> (HPST.q_not_qual <$> tycon)
(* qtycls 	→ 	[ modid . ] tycls      *)
let qtycls  = qual_id_tk (function
  | TK.T_DOT_CONID s  -> Some (TK.syms_of_qstring (Symbol.name s))
  | _ -> None)
  <|> (HPST.q_not_qual <$> tycls)
(* qvarsym 	→ 	[ modid . ] varsym      *)
let qvarsym = qual_id_tk (function
  | TK.T_MOD_VARSYM p -> Some p
  | _ -> None) <|> (HPST.q_not_qual <$> varsym)
(* qconsym 	→ 	[ modid . ] consym      *)
let qconsym = qual_id_tk (function
  | TK.T_MOD_CONSYM p -> Some p
  | _ -> None)
  <|> (HPST.q_not_qual <$> consym)

let integer = pred_tk (function | TK.L_INTEGER _ -> true | _ -> false)
(* let  *)

(* 10.5  Context-Free Syntax
   -- 変数とシンボルの定義は他から参照されるので先に定義 -- 
   -- variable and symbol section -- 
*)

let form_prepend a p = TK.form_prepend <$> a <*> p
let form_append  p a = TK.form_append  <$> p <*> a

let form_between a b p = TK.form_between <$> a <*> p <*> b
let between a b = form_between a b |.| (lift_a fst)

let cons = (fun a d -> a :: d)

let list_form pl =
  (function
    | []         -> ([], region_dummy)
    (* failwith "Null parsers. Syntax definition bug!" *)
    | [(e, reg)] -> ([e], reg)
    | (e :: _) as es -> TK.form_between e (L.map fst es) (L.hd (Data.init es)))
  <$> pl

let form_parened    p = form_between (just_tk TK.SP_LEFT_PAREN)   (just_tk TK.SP_RIGHT_PAREN)   p
let form_bracketed  p = form_between (just_tk TK.SP_LEFT_BRACKET) (just_tk TK.SP_RIGHT_BRACKET) p
let form_backquoted p = form_between (just_tk TK.SP_B_QUOTE)      (just_tk TK.SP_B_QUOTE)       p

let parened    p = form_parened    ((lift_a fst) p)
let bracketed  p = form_bracketed  ((lift_a fst) p)
let backquoted p = form_backquoted ((lift_a fst) p)

(* var 	→ 	varid | ( varsym )     	(variable) *)
let var = varid <|> parened varsym

(* qvar 	→ 	qvarid | ( qvarsym )     	(qualified variable) *)
let qvar = qvarid <|> parened qvarsym

(* con 	→ 	conid | ( consym )     	(constructor) *)
let con = conid <|> parened consym

let gconsym = (HSY.tk_id_colon <$> just_tk TK.KS_COLON) <|> qconsym
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
(* gconsym 	→ 	: | qconsym      *)
(* gconsym は qcon から使われている ↑ *)
(* gconsym is used by qcon above *)
 
(* gcon 	→ 	()      *)
(* 	| 	[]      *)
(* 	| 	(,{,})      *)
(* 	| 	qcon      *)
let rec commas () = (just_tk TK.SP_COMMA *> (succ <$> call commas)) <|> pure 0
let gcon =
  form_parened (pure HSY.id_unit) <|> form_bracketed (pure HSY.id_null_list)
    <|> form_parened (HSY.id_tuple <$> (call commas))
      <|> qcon

(* 10.5  Context-Free Syntax
   -- expression から参照される要素を先に定義 -- 
   -- define elements which referred by expression element. -- 
*)

(* decls 	→ 	{ decl1 ; … ; decln }     	(n ≥ 0) *)
(* decl 	→ 	gendecl      *)
(* 	| 	(funlhs | pat) rhs      *)

 
(* qual 	→ 	pat <- exp     	(generator) *)
(* 	| 	let decls     	(local declaration) *)
(* 	| 	exp     	(guard) *)
let qual = p_fix_later
 
(* alts 	→ 	alt1 ; … ; altn     	(n ≥ 1) *)
(* alt 	→ 	pat -> exp [where decls]      *)
(* 	| 	pat gdpat [where decls]      *)
(* 	| 	    	(empty alternative) *)
let alt = p_fix_later
 
(* gdpat 	→ 	guards -> exp [ gdpat ]      *)
 
(* stmts 	→ 	stmt1 … stmtn exp [;]     	(n ≥ 0) *)
(* stmt 	→ 	exp ;      *)
(* 	| 	pat <- exp ;      *)
(* 	| 	let decls ;      *)
(* 	| 	;     	(empty statement) *)
let stmt = p_fix_later
 
(* fbind 	→ 	qvar = exp      *)
let fbind = p_fix_later
 
(* pat 	→ 	lpat qconop pat     	(infix constructor) *)
(* 	| 	- (integer | float)     	(negative literal) *)
(* 	| 	lpat      *)
 
(* lpat 	→ 	apat      *)
(* 	| 	- (integer | float)     	(negative literal) *)
(* 	| 	gcon apat1 … apatk     	(arity gcon  =  k, k ≥ 1) *)
 
(* apat 	→ 	var [ @ apat]     	(as pattern) *)
(* 	| 	gcon     	(arity gcon  =  0) *)
(* 	| 	qcon { fpat1 , … , fpatk }     	(labeled pattern, k ≥ 0) *)
(* 	| 	literal      *)
(* 	| 	_     	(wildcard) *)
(* 	| 	( pat )     	(parenthesized pattern) *)
(* 	| 	( pat1 , … , patk )     	(tuple pattern, k ≥ 2) *)
(* 	| 	[ pat1 , … , patk ]     	(list pattern, k ≥ 1) *)
(* 	| 	~ apat     	(irrefutable pattern) *)
let apat = p_fix_later
 
(* fpat 	→ 	qvar = pat      *)



(* 10.5  Context-Free Syntax *)

(* module 	 → 	module modid [exports] where body       *)
(* 	| 	body      *)
(* body 	→ 	{ impdecls ; topdecls }      *)
(* 	| 	{ impdecls }      *)
(* 	| 	{ topdecls }      *)
 
(* impdecls 	→ 	impdecl1 ; … ; impdecln     	(n ≥ 1) *)
 
(* exports 	→ 	( export1 , … , exportn [ , ] )     	(n ≥ 0) *)
 
(* export 	→ 	qvar      *)
(* 	| 	qtycon [(..) | ( cname1 , … , cnamen )]     	(n ≥ 0) *)
(* 	| 	qtycls [(..) | ( qvar1 , … , qvarn )]     	(n ≥ 0) *)
(* 	| 	module modid      *)
 
(* impdecl 	→ 	import [qualified] modid [as modid] [impspec]      *)
(* 	| 	    	(empty declaration) *)
 
(* impspec 	→ 	( import1 , … , importn [ , ] )     	(n ≥ 0) *)
(* 	| 	hiding ( import1 , … , importn [ , ] )     	(n ≥ 0) *)
 
(* import 	→ 	var      *)
(* 	| 	tycon [ (..) | ( cname1 , … , cnamen )]     	(n ≥ 0) *)
(* 	| 	tycls [(..) | ( var1 , … , varn )]     	(n ≥ 0) *)
(* cname 	→ 	var | con      *)
 
(* topdecls 	→ 	topdecl1 ; … ; topdecln     	(n ≥ 0) *)
(* topdecl 	→ 	type simpletype = type      *)
(* 	| 	data [context =>] simpletype [= constrs] [deriving]      *)
(* 	| 	newtype [context =>] simpletype = newconstr [deriving]     *)
(* 	| 	class [scontext =>] tycls tyvar [where cdecls]      *)
(* 	| 	instance [scontext =>] qtycls inst [where idecls]      *)
(* 	| 	default (type1 , … , typen)     	(n ≥ 0) *)
(* 	| 	foreign fdecl      *)
(* 	| 	decl      *)
 
(* cdecls 	→ 	{ cdecl1 ; … ; cdecln }     	(n ≥ 0) *)
(* cdecl 	→ 	gendecl      *)
(* 	| 	(funlhs | var) rhs      *)
 
(* idecls 	→ 	{ idecl1 ; … ; idecln }     	(n ≥ 0) *)
(* idecl 	→ 	(funlhs | var) rhs      *)
(* 	| 	    	(empty) *)
 
(* gendecl 	→ 	vars :: [context =>] type     	(type signature) *)
(* 	| 	fixity [integer] ops     	(fixity declaration) *)
(* 	| 	    	(empty declaration) *)
 
(* ops 	→ 	op1 , … , opn     	(n ≥ 1) *)
(* vars 	→ 	var1 , …, varn     	(n ≥ 1) *)
(* fixity 	→ 	infixl | infixr | infix      *)
 
(* type 	→ 	btype [-> type]     	(function type) *)
let typ = p_fix_later
 
(* btype 	→ 	[btype] atype     	(type application) *)
 
(* atype 	→ 	gtycon      *)
(* 	| 	tyvar      *)
(* 	| 	( type1 , … , typek )     	(tuple type, k ≥ 2) *)
(* 	| 	[ type ]     	(list type) *)
(* 	| 	( type )     	(parenthesized constructor) *)
 
(* gtycon 	→ 	qtycon      *)
(* 	| 	()     	(unit type) *)
(* 	| 	[]     	(list constructor) *)
(* 	| 	(->)     	(function constructor) *)
(* 	| 	(,{,})     	(tupling constructors) *)
 
(* class 	→ 	qtycls tyvar      *)
let clazz = p_fix_later

(* context 	→ 	class      *)
(* 	| 	( class1 , … , classn )     	(n ≥ 0) *)
let rec class_list () =
  cons <$> clazz <*> (just_tk TK.SP_COMMA *> call class_list)
    <|> return []
let context =  list_form (cons <$> clazz <*> return [])
  <|> parened (list_form (call class_list))

(* 	| 	qtycls ( tyvar atype1 … atypen )     	(n ≥ 1) *)
(* scontext 	→ 	simpleclass      *)
(* 	| 	( simpleclass1 , … , simpleclassn )     	(n ≥ 0) *)
(* simpleclass	→ 	qtycls tyvar      *)
 
(* simpletype 	→ 	tycon tyvar1 … tyvark     	(k ≥ 0) *)
(* constrs 	→ 	constr1 | … | constrn     	(n ≥ 1) *)
(* constr 	→ 	con [!] atype1 … [!] atypek     	(arity con  =  k, k ≥ 0) *)
(* 	| 	(btype | ! atype) conop (btype | ! atype)     	(infix conop) *)
(* 	| 	con { fielddecl1 , … , fielddecln }     	(n ≥ 0) *)
(* newconstr 	→ 	con atype      *)
(* 	| 	con { var :: type }      *)
(* fielddecl 	→ 	vars :: (type | ! atype)      *)
(* deriving 	→ 	deriving (dclass | (dclass1, … , dclassn))     	(n ≥ 0) *)
(* dclass 	→ 	qtycls      *)
 
(* inst 	→ 	gtycon      *)
(* 	| 	( gtycon tyvar1 … tyvark )     	(k ≥ 0, tyvars distinct) *)
(* 	| 	( tyvar1 , … , tyvark )     	(k ≥ 2, tyvars distinct) *)
(* 	| 	[ tyvar ]      *)
(* 	| 	( tyvar1 -> tyvar2 )     	tyvar1 and tyvar2 distinct *)
 
(* fdecl 	→ 	import callconv [safety] impent var :: ftype     	(define varibale) *)
(* 	| 	export callconv expent var :: ftype     	(expose variable) *)
(* callconv 	→ 	ccall | stdcall | cplusplus     	(calling convention) *)
(* 	| 	jvm | dotnet      *)
(* 	| 	 system-specific calling conventions      *)
(* impent 	→ 	[string]     	(see Section 8.5.1) *)
(* expent 	→ 	[string]     	(see Section 8.5.1) *)
(* safety 	→ 	unsafe | safe      *)
 
(* ftype 	→ 	frtype      *)
(* 	| 	fatype  →  ftype      *)
(* frtype 	→ 	fatype      *)
(* 	| 	()      *)
(* fatype 	→ 	qtycon atype1 … atypek     	(k  ≥  0) *)
 
(* funlhs 	→ 	var apat { apat }      *)
(* 	| 	pat varop pat      *)
(* 	| 	( funlhs ) apat { apat }      *)
 
(* rhs 	→ 	= exp [where decls]      *)
(* 	| 	gdrhs [where decls]      *)
 
(* gdrhs 	→ 	guards = exp [gdrhs]      *)
 
(* guards 	→ 	| guard1, …, guardn     	(n ≥ 1) *)
(* guard 	→ 	pat <- infixexp     	(pattern guard) *)
(* 	| 	let decls     	(local declaration) *)
(* 	| 	infixexp     	(boolean guard) *)

let rec dummy_exp_top () = p_fix_later

(* exp 	→ 	infixexp :: [context =>] type     	(expression type signature) *)
(* 	| 	infixexp      *)
(* and     exp () = p_fix_later *)
and     exp () = 
  HSY.exp
  <$> call infixexp
  <*> optional (HSY.exp_typ
                <$> optional (form_append context (just_tk TK.KS_R_W_ARROW))
                <*> typ)
 
(* infixexp 	→ 	lexp qop infixexp     	(infix operator application) *)
(* 	| 	- infixexp     	(prefix negation) *)
(* 	| 	lexp      *)
and     infixexp () = (HSY.op_app <$> (call lexp) <*> qop <*> (call infixexp))
  <|> (HSY.neg <$> (call infixexp)) <|> (HSY.lexp <$> (call lexp))

(* lexp 	→ 	\ apat1 … apatn -> exp     	(lambda abstraction, n ≥ 1) *)
(* 	| 	let decls in exp     	(let expression) *)
(* 	| 	if exp [;] then exp [;] else exp     	(conditional) *)
(* 	| 	case exp of { alts }     	(case expression) *)
(* 	| 	do { stmts }     	(do expression) *)
(* 	| 	fexp      *)
and     lexp () = HSY.fexp <$> call fexp

(* fexp 	→ 	[fexp] aexp     	(function application) *)
(* and     fexp () = HSY.fexp_of_aexp_list <$> some (fst <$> call aexp) *)
and     fexp () = HSY.fexp_of_aexp_list <$> list_form (some (call aexp))
 
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
and     aexp () = (HSY.var <$> qvar) <|> (HSY.con <$> gcon) <|> (HSY.lit <$> literal)
  (* <|> (HSY.paren <$> parened (call exp)) *)

and     stmts () = HSY.do_stmts <$> many stmt <*> call exp
 
let any    = pred_tk (fun _ -> true)

let test_s0 = any *>
  some (qvar <|> gconsym <|> qconop <|> qvarop)

(*  *)
let test_s1 : (TK.t, HSY.infexp HSY.fexp * TK.region) parser =
  (fst <$> any) *> call fexp
