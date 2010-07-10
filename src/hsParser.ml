
open Simple.Combinator

module TK = Token

let call = call_parser

let (|.|) f g x = f (g x)

let sat_tk : (TK.typ -> bool) -> (TK.t, TK.t) parser =
  fun f -> satisfy (f |.| fst)
let just_tk eq = sat_tk ((=) eq)

(* 10.2  Lexical Syntax *)

(* varsym 	 → 	( symbol{:} {symbol} ){reservedop | dashes}      *)
let varsym = sat_tk (function
  | (TK.T_VARSYM _ | TK.KS_PLUS | TK.KS_MINUS | TK.KS_EXCLAM) -> true
  | _ -> false)
(* consym 	→ 	( : {symbol}){reservedop}      *)
let consym = sat_tk (function | TK.T_CONSYM _ -> true | _ -> false)


(* varid 	 	 (variables)      *)
let varid = sat_tk (function
  | (TK.T_VARID _ | TK.K_AS | TK.K_QUALIFIED | TK.K_HIDING) -> true
  | _ -> false)
(* conid 		(constructors)      *)
let conid = sat_tk (function | TK.T_CONID _ -> true | _ -> false)
(* tyvar 	→ 	varid     	(type variables) *)
let tyvar = varid
(* tycon 	→ 	conid     	(type constructors) *)
let tycon = conid
(* tycls 	→ 	conid     	(type classes) *)
let tycls = conid
(* modid 	→ 	{conid .} conid     	(modules) *)
let modid = sat_tk (function | TK.T_MODID _ -> true | _ -> false)

(* qvarid 	→ 	[ modid . ] varid      *)
let qvarid  = sat_tk (function | TK.T_MOD_VARID _  -> true | _ -> false) <|> varid
(* qconid 	→ 	[ modid . ] conid      *)
let qconid  = sat_tk (function | TK.T_MOD_CONID _  -> true | _ -> false) <|> conid
(* qtycon 	→ 	[ modid . ] tycon      *)
let qtycon  = sat_tk (function | TK.T_MOD_CONID _  -> true | _ -> false) <|> tycon
(* qtycls 	→ 	[ modid . ] tycls      *)
let qtycls  = sat_tk (function | TK.T_MOD_CONID _  -> true | _ -> false) <|> tycls
(* qvarsym 	→ 	[ modid . ] varsym      *)
let qvarsym = sat_tk (function | TK.T_MOD_VARSYM _ -> true | _ -> false) <|> varsym
(* qconsym 	→ 	[ modid . ] consym      *)
let qconsym = sat_tk (function | TK.T_MOD_CONSYM _ -> true | _ -> false) <|> consym


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
 
(* decls 	→ 	{ decl1 ; … ; decln }     	(n ≥ 0) *)
(* decl 	→ 	gendecl      *)
(* 	| 	(funlhs | pat) rhs      *)
 
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
 
(* context 	→ 	class      *)
(* 	| 	( class1 , … , classn )     	(n ≥ 0) *)
(* class 	→ 	qtycls tyvar      *)
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
 
(* exp 	→ 	infixexp :: [context =>] type     	(expression type signature) *)
(* 	| 	infixexp      *)
 
(* infixexp 	→ 	lexp qop infixexp     	(infix operator application) *)
(* 	| 	- infixexp     	(prefix negation) *)
(* 	| 	lexp      *)
 
(* lexp 	→ 	\ apat1 … apatn -> exp     	(lambda abstraction, n ≥ 1) *)
(* 	| 	let decls in exp     	(let expression) *)
(* 	| 	if exp [;] then exp [;] else exp     	(conditional) *)
(* 	| 	case exp of { alts }     	(case expression) *)
(* 	| 	do { stmts }     	(do expression) *)
(* 	| 	fexp      *)
(* fexp 	→ 	[fexp] aexp     	(function application) *)
 
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
 
(* qual 	→ 	pat <- exp     	(generator) *)
(* 	| 	let decls     	(local declaration) *)
(* 	| 	exp     	(guard) *)
 
(* alts 	→ 	alt1 ; … ; altn     	(n ≥ 1) *)
(* alt 	→ 	pat -> exp [where decls]      *)
(* 	| 	pat gdpat [where decls]      *)
(* 	| 	    	(empty alternative) *)
 
(* gdpat 	→ 	guards -> exp [ gdpat ]      *)
 
(* stmts 	→ 	stmt1 … stmtn exp [;]     	(n ≥ 0) *)
(* stmt 	→ 	exp ;      *)
(* 	| 	pat <- exp ;      *)
(* 	| 	let decls ;      *)
(* 	| 	;     	(empty statement) *)
 
(* fbind 	→ 	qvar = exp      *)
 
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
 
(* fpat 	→ 	qvar = pat      *)
 
(* gcon 	→ 	()      *)
(* 	| 	[]      *)
(* 	| 	(,{,})      *)
(* 	| 	qcon      *)
 
(* var 	→ 	varid | ( varsym )     	(variable) *)
(* qvar 	→ 	qvarid | ( qvarsym )     	(qualified variable) *)
(* con 	→ 	conid | ( consym )     	(constructor) *)
(* qcon 	→ 	qconid | ( gconsym )     	(qualified constructor) *)
(* varop 	→ 	varsym | `  varid `     	(variable operator) *)
(* qvarop 	→ 	qvarsym | `  qvarid `     	(qualified variable operator) *)
(* conop 	→ 	consym | `  conid `     	(constructor operator) *)
(* qconop 	→ 	gconsym | `  qconid `     	(qualified constructor operator) *)
(* op 	→ 	varop | conop     	(operator) *)
(* qop 	→ 	qvarop | qconop     	(qualified operator) *)
(* gconsym 	→ 	: | qconsym      *)

let any    = sat_tk (fun _ -> true)

let test_s0 = any *>
  many (qvarid <|> qconid <|> qtycon
    <|> qtycls <|> qvarsym <|> qconsym <|> modid)
