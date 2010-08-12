
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

let comma = just_tk TK.SP_COMMA
let semi  = just_tk TK.SP_SEMI

let opt_semi = ~? semi

(* 汎用の構成子 *)
(*   構文構造の並び、挟まれる構造、0以上のリスト、1以上のリスト *)

let form_prepend a p = TK.form_prepend <$> a <*> p
let form_append  p a = TK.form_append  <$> p <*> a

let ( **> ) = form_prepend
let ( <** ) = form_append

let form_between a b p = TK.form_between <$> a <*> p <*> b
let between a b = form_between a b |.| (lift_a fst)

let form_parened    p = form_between (just_tk TK.SP_LEFT_PAREN)   (just_tk TK.SP_RIGHT_PAREN)   p
let form_bracketed  p = form_between (just_tk TK.SP_LEFT_BRACKET) (just_tk TK.SP_RIGHT_BRACKET) p
let form_braced     p = form_between (just_tk TK.SP_LEFT_BRACE)   (just_tk TK.SP_RIGHT_BRACE)   p
let form_backquoted p = form_between (just_tk TK.SP_B_QUOTE)      (just_tk TK.SP_B_QUOTE)       p

let parened    p = form_parened    ((lift_a fst) p)
let bracketed  p = form_bracketed  ((lift_a fst) p)
let braced     p = form_braced     ((lift_a fst) p)
let backquoted p = form_backquoted ((lift_a fst) p)

(* リストとなる構文要素 - 長さ0でも可 - 位置情報付き *)
let list_form pl =
  (function
    | []         -> ([], region_dummy)
    | [(e, reg)] -> ([e], reg)
    | (er :: _) as ers -> TK.form_between er (L.map fst ers) (L.hd (Data.init ers)))
  <$> pl

(* 長さ1以上のリスト 意図的に0以上のリストと型が異なるようにしている *)
let l1_some p = Data.tuple2 <$> p <*> many p

let l1_form pl1 =
  (function
    | ((e, reg), []) -> ((e, []), reg)
    | (er, ers)  -> TK.form_between er (fst er, (L.map fst ers)) (L.hd (Data.init ers)))
   <$> pl1

let l1_list_form pl1 = TK.with_region Data.l1_list <$> l1_form pl1

let rec separated a d =
  (Data.cons <$> a <*> (d *> ~$ (fun () -> separated a d)))
  <|> pure []

let rec l1_separated a d =
  (Data.l1_cons <$> a <*> (d *> ~$ (fun () -> l1_separated a d)))
  <|> (Data.l1_cons_nil <$> a)

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
let rec commas1 () = comma *> ((succ <$> ~$ commas1) <|> pure 1)
let gcon =
  form_parened (pure HSY.id_unit) <|> form_bracketed (pure HSY.id_null_list)
    <|> form_parened (HSY.id_tuple <$> (~$ commas1))
      <|> qcon

(* 10.5  Context-Free Syntax
   -- expression から参照される要素を先に定義 -- 
   -- define elements which referred by expression element. -- 
*)

(* decl 	→ 	gendecl      *)
(* 	| 	(funlhs | pat) rhs      *)
let decl = p_fix_later
 
(* decls 	→ 	{ decl1 ; … ; decln }     	(n ≥ 0) *)
let decls = braced (list_form (separated decl semi))

(* qual 	→ 	pat <- exp     	(generator) *)
(* 	| 	let decls     	(local declaration) *)
(* 	| 	exp     	(guard) *)
let qual = p_fix_later
 
(* alt 	→ 	pat -> exp [where decls]      *)
(* 	| 	pat gdpat [where decls]      *)
(* 	| 	    	(empty alternative) *)
let alt = p_fix_later
 
(* alts 	→ 	alt1 ; … ; altn     	(n ≥ 1) *)
let alts = l1_list_form (l1_separated alt semi)

(* gdpat 	→ 	guards -> exp [ gdpat ]      *)
 
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
  HSY.g_qtycon <$> qtycon
  <|> form_parened (pure HSY.TC_Unit)
    <|> form_bracketed (pure HSY.TC_List)
      <|> form_parened (just_tk TK.KS_R_ARROW *> pure HSY.TC_Arrow)
        <|> (HSY.g_tuple <$> form_parened (~$ commas1))

let rec dummy_type_top () = p_fix_later

(* type 	→ 	btype [-> type]     	(function type) *)
and     typ ()   =
  HSY.typ_of_btype_list
  <$> l1_form
    (l1_separated (~$ btype) (just_tk TK.KS_R_ARROW))
 
(* btype 	→ 	[btype] atype     	(type application) *)
and     btype () =
  HSY.btype_of_atype_list
  <$> l1_form (l1_some (~$ atype))
 
(* atype 	→ 	gtycon      *)
(* 	| 	tyvar      *)
(* 	| 	( type1 , … , typek )     	(tuple type, k ≥ 2) *)
(* 	| 	[ type ]     	(list type) *)
(* 	| 	( type )     	(parenthesized constructor) *)

and     atype () =
  (HSY.a_gtc <$> gtycon)
  <|> (HSY.a_tyvar <$> tyvar)
    <|> parened (HSY.a_tuple
                 <$> l1_form (Data.l1_cons
                              <$> ~$ typ
                              <*> (comma *> l1_separated (~$ typ) comma)))
      <|> bracketed (HSY.a_list <$> ~$ typ)
        <|> parened (HSY.a_paren <$> ~$ typ)
 
(* class 	→ 	qtycls tyvar      *)
let clazz = HSY.cls <$> qtycls <*> tyvar

(* context 	→ 	class      *)
(* 	| 	( class1 , … , classn )     	(n ≥ 0) *)
let context =  list_form (Data.cons_nil <$> clazz)
  <|> parened (list_form (separated clazz comma (* class list *) ))

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
and     exp () = 
  HSY.exp
  <$> ~$ infixexp
  <*> ~? (just_tk TK.KS_2_COLON **>
            (HSY.exp_typ
             <$> ~? (form_append context (just_tk TK.KS_R_W_ARROW))
             <*> ~$ typ))
 
(* infixexp 	→ 	lexp qop infixexp     	(infix operator application) *)
(* 	| 	- infixexp     	(prefix negation) *)
(* 	| 	lexp      *)
and     infixexp () = (HSY.op_app <$> (~$ lexp) <*> qop <*> (~$ infixexp))
  <|> (just_tk TK.KS_MINUS **> (HSY.neg <$> ~$ infixexp)) <|> (HSY.lexp <$> (~$ lexp))

(* lexp 	→ 	\ apat1 … apatn -> exp     	(lambda abstraction, n ≥ 1) *)
(* 	| 	let decls in exp     	(let expression) *)
(* 	| 	if exp [;] then exp [;] else exp     	(conditional) *)
(* 	| 	case exp of { alts }     	(case expression) *)
(* 	| 	do { stmts }     	(do expression) *)
(* 	| 	fexp      *)
and     lexp () =
  (HSY.lambda
   <$> (form_prepend (just_tk TK.KS_B_SLASH) (l1_list_form (l1_some apat)))
   <*> ~$ exp)
  <|> (just_tk TK.K_LET **> (HSY.let_ <$> decls <*> (just_tk TK.K_IN **> ~$ exp)))
    <|> (just_tk TK.K_IF **> (HSY.if_ <$> (~$ exp <* opt_semi)
                              <*> (just_tk TK.K_THEN **> ~$ exp <* opt_semi)
                              <*> (just_tk TK.K_ELSE **> ~$ exp)))
      <|> (just_tk TK.K_CASE **> (HSY.case <$> ~$ exp <*> (just_tk TK.K_OF **> alts)))
        <|> (just_tk TK.K_DO **> braced (~$ stmts))
          <|> (HSY.fexp <$> ~$ fexp)

(* stmts 	→ 	stmt1 … stmtn exp [;]     	(n ≥ 0) *)
and     stmts () = HSY.do_ <$> list_form (many stmt) <*> ~$ exp <* opt_semi
(* stmt 	→ 	exp ;      *)
(* 	| 	pat <- exp ;      *)
(* 	| 	let decls ;      *)
(* 	| 	;     	(empty statement) *)
and     stmt = p_fix_later
 

(* fexp 	→ 	[fexp] aexp     	(function application) *)
and     fexp () = HSY.fexp_of_aexp_list <$> l1_form (l1_some (~$ aexp))
 
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
and     comma_expl_1 () =
  l1_separated (~$ exp) comma
and     comma_expl_2 () =
  Data.l1_cons <$> ~$ exp <*> (comma *> ~$ comma_expl_1)
and     aexp () = (HSY.var <$> qvar) <|> (HSY.con <$> gcon) <|> (HSY.lit <$> literal)
  <|> (HSY.paren <$> parened (~$ exp))
    <|> (HSY.tuple <$> parened (l1_list_form (~$ comma_expl_2)))
      <|> (HSY.list <$> bracketed (l1_list_form (~$ comma_expl_1)))
        <|> bracketed (HSY.comp
                       <$> ~$ exp
                       <*> (just_tk TK.KS_BAR **> l1_list_form (l1_separated qual comma)))
          <|> parened (HSY.left_sec <$> ~$ infixexp <*> qop)
            <|> parened (HSY.right_sec <$> (~! (just_tk TK.KS_MINUS) *> qop) <*> ~$ infixexp)
              <|> (HSY.lbl_cons <$> qcon <*> braced (list_form (separated fbind comma)))
                <|> (HSY.lbl_upd <$> qcon <*> braced (l1_list_form (l1_separated fbind comma)))

let drop_any    = pred_tk (fun _ -> true)

let test_s0 = drop_any *>
  some (qvar <|> gconsym <|> qconop <|> qvarop)

(*  *)
let test_s1 : (TK.t, HSY.infexp * TK.region) parser =
  (fst <$> drop_any) *> ~$ infixexp

(*let test_s1 : (TK.t, HSY.infexp HSY.lexp * TK.region) parser =
  (fst <$> drop_any) *> ~$ lexp*)

let test_s2 : (TK.t, HSY.typ * TK.region) parser =
  (fst <$> drop_any) *> ~$ typ
