

module ZL = LazyList

module Combinator = ParserDriver.Combinator(Token)
open Combinator

(* module T = ParserDriver.T *)

module SYM = Symbol
module HSY = HsSyntax

(* type ('e, 'reg) exp = ('e, 'reg) T.exp *)
(* type ('deriv, 'exp) result = ('deriv, 'exp) T.result *)
(* type ('deriv, 'exp) parser = ('deriv, 'exp) T.parser *)

type deriv = {
  token : (deriv, token exp) result;
  
(* val tk_module : token Combinator.exp Combinator.parser *)
tk_module : (deriv, token exp) result;
(* val tk_import : token Combinator.exp Combinator.parser *)
tk_import : (deriv, token exp) result;
(* val tk_export : token Combinator.exp Combinator.parser *)
tk_export : (deriv, token exp) result;
(* val let_ : token Combinator.exp Combinator.parser *)
let_ : (deriv, token exp) result;
(* val where : token Combinator.exp Combinator.parser *)
where : (deriv, token exp) result;
(* val comma : token Combinator.exp Combinator.parser *)
comma : (deriv, token exp) result;
(* val semi : token Combinator.exp Combinator.parser *)
semi : (deriv, token exp) result;
(* val eq : token Combinator.exp Combinator.parser *)
eq : (deriv, token exp) result;
(* val minus : token Combinator.exp Combinator.parser *)
minus : (deriv, token exp) result;
(* val exclam : token Combinator.exp Combinator.parser *)
exclam : (deriv, token exp) result;
(* val two_colon : token Combinator.exp Combinator.parser *)
two_colon : (deriv, token exp) result;
(* val dotdot : token Combinator.exp Combinator.parser *)
dotdot : (deriv, token exp) result;
(* val bar : token Combinator.exp Combinator.parser *)
bar : (deriv, token exp) result;
(* val l_arrow : token Combinator.exp Combinator.parser *)
l_arrow : (deriv, token exp) result;
(* val r_arrow : token Combinator.exp Combinator.parser *)
r_arrow : (deriv, token exp) result;
(* val r_w_arrow : token Combinator.exp Combinator.parser *)
r_w_arrow : (deriv, token exp) result;
(* val l_paren : token Combinator.exp Combinator.parser *)
l_paren : (deriv, token exp) result;
(* val r_paren : token Combinator.exp Combinator.parser *)
r_paren : (deriv, token exp) result;
(* val l_brace : token Combinator.exp Combinator.parser *)
l_brace : (deriv, token exp) result;
(* val r_brace : token Combinator.exp Combinator.parser *)
r_brace : (deriv, token exp) result;
(* val match_or_shift_rb : token Combinator.exp Combinator.parser *)
match_or_shift_rb : (deriv, token exp) result;
(* val opt_semi : token option Combinator.exp Combinator.parser *)
opt_semi : (deriv, token option exp) result;
(* val opt_exclam : token option Combinator.exp Combinator.parser *)
opt_exclam : (deriv, token option exp) result;

(* val conid : SYM.t Combinator.exp Combinator.parser *)
conid : (deriv, SYM.t exp) result;
(* val dotted_conid : SYM.t Combinator.exp Combinator.parser *)
dotted_conid : (deriv, SYM.t exp) result;
(* val literal : HSY.lit Combinator.exp Combinator.parser *)
literal : (deriv, HSY.lit exp) result;
(* val varsym : SYM.t Combinator.exp Combinator.parser *)
varsym : (deriv, SYM.t exp) result;
(* val consym : SYM.t Combinator.exp Combinator.parser *)
consym : (deriv, SYM.t exp) result;
(* val varid : SYM.t Combinator.exp Combinator.parser *)
varid : (deriv, SYM.t exp) result;
(* val tyvar : SYM.t Combinator.exp Combinator.parser *)
tyvar : (deriv, SYM.t exp) result;
(* val tycon : SYM.t Combinator.exp Combinator.parser *)
tycon : (deriv, SYM.t exp) result;
(* val tycls : SYM.t Combinator.exp Combinator.parser *)
tycls : (deriv, SYM.t exp) result;
(* val modid : SYM.t Combinator.exp Combinator.parser *)
modid : (deriv, SYM.t exp) result;
(* val qvarid : HSY.id Combinator.exp Combinator.parser *)
qvarid : (deriv, HSY.id exp) result;
(* val qconid : HSY.id Combinator.exp Combinator.parser *)
qconid : (deriv, HSY.id exp) result;
(* val qtycon : HSY.id Combinator.exp Combinator.parser *)
qtycon : (deriv, HSY.id exp) result;
(* val qtycls : HSY.id Combinator.exp Combinator.parser *)
qtycls : (deriv, HSY.id exp) result;
(* val qvarsym : HSY.id Combinator.exp Combinator.parser *)
qvarsym : (deriv, HSY.id exp) result;
(* val qconsym : HSY.id Combinator.exp Combinator.parser *)
qconsym : (deriv, HSY.id exp) result;
(* val string : HSY.hs_string Combinator.exp Combinator.parser *)
string : (deriv, HSY.hs_string exp) result;
(* val integer : int64 Combinator.exp Combinator.parser *)
integer : (deriv, int64 exp) result;
(* val float : float Combinator.exp Combinator.parser *)
float : (deriv, float exp) result;
(* val fixity_int : int Combinator.exp Combinator.parser *)
fixity_int : (deriv, int exp) result;
(* val var : SYM.t Combinator.exp Combinator.parser *)
var : (deriv, SYM.t exp) result;
(* val qvar : HSY.id Combinator.exp Combinator.parser *)
qvar : (deriv, HSY.id exp) result;
(* val con : SYM.t Combinator.exp Combinator.parser *)
con : (deriv, SYM.t exp) result;
(* val gconsym : HSY.id Combinator.exp Combinator.parser *)
gconsym : (deriv, HSY.id exp) result;
(* val qcon : HSY.id Combinator.exp Combinator.parser *)
qcon : (deriv, HSY.id exp) result;
(* val varop : SYM.t Combinator.exp Combinator.parser *)
varop : (deriv, SYM.t exp) result;
(* val qvarop : HSY.id Combinator.exp Combinator.parser *)
qvarop : (deriv, HSY.id exp) result;
(* val conop : SYM.t Combinator.exp Combinator.parser *)
conop : (deriv, SYM.t exp) result;
(* val qconop : HSY.id Combinator.exp Combinator.parser *)
qconop : (deriv, HSY.id exp) result;
(* val op : SYM.t Combinator.exp Combinator.parser *)
op : (deriv, SYM.t exp) result;
(* val qop : HSY.id Combinator.exp Combinator.parser *)
qop : (deriv, HSY.id exp) result;
(* val commas1 : unit -> int Combinator.exp Combinator.parser *)
commas1 : (deriv, int exp) result;
(* val gcon : HSY.id Combinator.exp Combinator.parser *)
gcon : (deriv, HSY.id exp) result;
(* val ops : SYM.t Data.l1_list Combinator.exp Combinator.parser *)
ops : (deriv, SYM.t Data.l1_list exp) result;
(* val vars : SYM.t Data.l1_list Combinator.exp Combinator.parser *)
vars : (deriv, SYM.t Data.l1_list exp) result;
(* val fixity : HSY.fixity Combinator.exp Combinator.parser *)
fixity : (deriv, HSY.fixity exp) result;
(* val gtycon : HSY.gtycon Combinator.exp Combinator.parser *)
gtycon : (deriv, HSY.gtycon exp) result;
(* val type_ : unit -> HSY.type_ Combinator.exp Combinator.parser *)
type_ : (deriv, HSY.type_ exp) result;
(* val btype : unit -> HSY.btype Combinator.exp Combinator.parser *)
btype : (deriv, HSY.btype exp) result;
(* val atype : unit -> HSY.atype Combinator.exp Combinator.parser *)
atype : (deriv, HSY.atype exp) result;
(* val class_ : HSY.class_ Combinator.exp Combinator.parser *)
class_ : (deriv, HSY.class_ exp) result;
(* val context : HSY.context Combinator.exp Combinator.parser *)
context : (deriv, HSY.context exp) result;
(* val may_be_context : HSY.may_be_context Combinator.exp Combinator.parser *)
may_be_context : (deriv, HSY.may_be_context exp) result;
(* val simpleclass : HSY.simpleclass Combinator.exp Combinator.parser *)
simpleclass : (deriv, HSY.simpleclass exp) result;
(* val scontext : HSY.scontext Combinator.exp Combinator.parser *)
scontext : (deriv, HSY.scontext exp) result;
(* val may_be_scontext : HSY.may_be_scontext Combinator.exp Combinator.parser *)
may_be_scontext : (deriv, HSY.may_be_scontext exp) result;
(* val simpletype : HSY.simpletype Combinator.exp Combinator.parser *)
simpletype : (deriv, HSY.simpletype exp) result;
(* val constr_arg : HSY.constr_arg Combinator.exp Combinator.parser *)
constr_arg : (deriv, HSY.constr_arg exp) result;
(* val fielddecl : HSY.fielddecl Combinator.exp Combinator.parser *)
fielddecl : (deriv, HSY.fielddecl exp) result;
(* val constr : HSY.constr Combinator.exp Combinator.parser *)
constr : (deriv, HSY.constr exp) result;
(* val constrs : HSY.constr Data.l1_list Combinator.exp Combinator.parser *)
constrs : (deriv, HSY.constr Data.l1_list exp) result;
(* val newconstr : HSY.newconstr Combinator.exp Combinator.parser *)
newconstr : (deriv, HSY.newconstr exp) result;
(* val dclass : HSY.id Combinator.exp Combinator.parser *)
dclass : (deriv, HSY.id exp) result;
(* val deriving : HSY.deriving Combinator.exp Combinator.parser *)
deriving : (deriv, HSY.deriving exp) result;
(* val inst : HSY.inst Combinator.exp Combinator.parser *)
inst : (deriv, HSY.inst exp) result;
(* val fatype : HSY.fatype Combinator.exp Combinator.parser *)
fatype : (deriv, HSY.fatype exp) result;
(* val frtype : HSY.frtype Combinator.exp Combinator.parser *)
frtype : (deriv, HSY.frtype exp) result;
(* val ftype : unit -> HSY.ftype Combinator.exp Combinator.parser *)
ftype : (deriv, HSY.ftype exp) result;
(* val conv_words : Symbol.t list *)
(* val safety_words : Symbol.t list *)
(* val unsafe : string -> Symbol.t *)
(* val safe : Symbol.t *)
(* val callconv : HSY.callconv Combinator.exp Combinator.parser *)
callconv : (deriv, HSY.callconv exp) result;
(* val impent : HSY.impent Combinator.exp Combinator.parser *)
impent : (deriv, HSY.impent exp) result;
(* val expent : HSY.expent Combinator.exp Combinator.parser *)
expent : (deriv, HSY.expent exp) result;
(* val safety : HSY.safety Combinator.exp Combinator.parser *)
safety : (deriv, HSY.safety exp) result;
(* val fdecl : HSY.fdecl Combinator.exp Combinator.parser *)
fdecl : (deriv, HSY.fdecl exp) result;
(* val pat : unit -> HSY.pat Combinator.exp Combinator.parser *)
pat : (deriv, HSY.pat exp) result;
(* val lpat : unit -> HSY.pat HSY.lpat Combinator.exp Combinator.parser *)
lpat : (deriv, HSY.pat HSY.lpat exp) result;
(* val comma_patl_1 : unit -> HSY.pat Data.l1_list Combinator.exp Combinator.parser *)
comma_patl_1 : (deriv, HSY.pat Data.l1_list exp) result;
(* val comma_patl_2 : unit -> HSY.pat Data.l1_list Combinator.exp Combinator.parser *)
comma_patl_2 : (deriv, HSY.pat Data.l1_list exp) result;
(* val apat : unit -> HSY.pat HSY.apat Combinator.exp Combinator.parser *)
apat : (deriv, HSY.pat HSY.apat exp) result;
(* val fpat : unit -> HSY.pat HSY.fpat Combinator.exp Combinator.parser *)
fpat : (deriv, HSY.pat HSY.fpat exp) result;
(* val funlhs : unit -> HSY.funlhs Combinator.exp Combinator.parser *)
funlhs : (deriv, HSY.funlhs exp) result;
(* val opt_where_decls : unit -> HSY.infexp HSY.decls option Combinator.exp Combinator.parser *)
opt_where_decls : (deriv, HSY.infexp HSY.decls option exp) result;
(* val let_decls : unit -> HSY.infexp HSY.decls Combinator.exp Combinator.parser *)
let_decls : (deriv, HSY.infexp HSY.decls exp) result;
(* val rhs : unit -> HSY.infexp HSY.rhs Combinator.exp Combinator.parser *)
rhs : (deriv, HSY.infexp HSY.rhs exp) result;
(* val gdrhs : unit -> HSY.infexp HSY.gdrhs Combinator.exp Combinator.parser *)
gdrhs : (deriv, HSY.infexp HSY.gdrhs exp) result;
(* val guards : unit -> HSY.infexp HSY.guard Data.l1_list Combinator.exp Combinator.parser *)
guards : (deriv, HSY.infexp HSY.guard Data.l1_list exp) result;
(* val guard : unit -> HSY.infexp HSY.guard Combinator.exp Combinator.parser *)
guard : (deriv, HSY.infexp HSY.guard exp) result;
(* val exp : unit -> HSY.infexp HSY.exp Combinator.exp Combinator.parser *)
exp : (deriv, HSY.infexp HSY.exp exp) result;
(* val infixexp : unit -> HSY.infexp Combinator.exp Combinator.parser *)
infixexp : (deriv, HSY.infexp exp) result;
(* val lexp : unit -> HSY.infexp HSY.lexp Combinator.exp Combinator.parser *)
lexp : (deriv, HSY.infexp HSY.lexp exp) result;
(* val fexp : unit -> HSY.infexp HSY.fexp Combinator.exp Combinator.parser *)
fexp : (deriv, HSY.infexp HSY.fexp exp) result;
(* val comma_expl_1 : unit -> HSY.infexp HSY.exp Data.l1_list Combinator.exp Combinator.parser *)
comma_expl_1 : (deriv, HSY.infexp HSY.exp Data.l1_list exp) result;
(* val comma_expl_2 : unit -> HSY.infexp HSY.exp Data.l1_list Combinator.exp Combinator.parser *)
comma_expl_2 : (deriv, HSY.infexp HSY.exp Data.l1_list exp) result;
(* val aexp_without_lu : unit -> HSY.infexp HSY.aexp Combinator.exp Combinator.parser *)
aexp_without_lu : (deriv, HSY.infexp HSY.aexp exp) result;
(* val braced_fbind_list_1 : unit -> HSY.infexp HSY.fbind Data.l1_list Combinator.exp Combinator.parser *)
braced_fbind_list_1 : (deriv, HSY.infexp HSY.fbind Data.l1_list exp) result;
(* val aexp : unit -> HSY.infexp HSY.aexp Combinator.exp Combinator.parser *)
aexp : (deriv, HSY.infexp HSY.aexp exp) result;
(* val qual : unit -> HSY.infexp HSY.qual Combinator.exp Combinator.parser *)
qual : (deriv, HSY.infexp HSY.qual exp) result;
(* val alts : unit -> HSY.infexp HSY.alt Data.l1_list Combinator.exp Combinator.parser *)
alts : (deriv, HSY.infexp HSY.alt Data.l1_list exp) result;
(* val alt : unit -> HSY.infexp HSY.alt Combinator.exp Combinator.parser *)
alt : (deriv, HSY.infexp HSY.alt exp) result;
(* val gdpat : unit -> (HSY.infexp HSY.guards * HSY.infexp HSY.exp) Data.l1_list Combinator.exp Combinator.parser *)
gdpat : (deriv, (HSY.infexp HSY.guards * HSY.infexp HSY.exp) Data.l1_list exp) result;
(* val stmts : unit -> (HSY.infexp HSY.stmt list * HSY.infexp HSY.exp) Combinator.exp Combinator.parser *)
stmts : (deriv, (HSY.infexp HSY.stmt list * HSY.infexp HSY.exp) exp) result;
(* val stmt : unit -> HSY.infexp HSY.stmt Combinator.exp Combinator.parser *)
stmt : (deriv, HSY.infexp HSY.stmt exp) result;
(* val fbind : unit -> HSY.infexp HSY.fbind Combinator.exp Combinator.parser *)
fbind : (deriv, HSY.infexp HSY.fbind exp) result;
(* val gendecl : unit -> HSY.gendecl Combinator.exp Combinator.parser *)
gendecl : (deriv, HSY.gendecl exp) result;
(* val decls : unit -> HSY.infexp HSY.decls Combinator.exp Combinator.parser *)
decls : (deriv, HSY.infexp HSY.decls exp) result;
(* val decl : unit -> HSY.infexp HSY.decl Combinator.exp Combinator.parser *)
decl : (deriv, HSY.infexp HSY.decl exp) result;
(* val cdecl : unit -> HSY.infexp HSY.cdecl Combinator.exp Combinator.parser *)
cdecl : (deriv, HSY.infexp HSY.cdecl exp) result;
(* val cdecls : HSY.infexp HSY.cdecls Combinator.exp Combinator.parser *)
cdecls : (deriv, HSY.infexp HSY.cdecls exp) result;
(* val idecl : unit -> HSY.infexp HSY.idecl Combinator.exp Combinator.parser *)
idecl : (deriv, HSY.infexp HSY.idecl exp) result;
(* val idecls : HSY.infexp HSY.idecls Combinator.exp Combinator.parser *)
idecls : (deriv, HSY.infexp HSY.idecls exp) result;
(* val cname : HSY.cname Combinator.exp Combinator.parser *)
cname : (deriv, HSY.cname exp) result;
(* val export : HSY.export Combinator.exp Combinator.parser *)
export : (deriv, HSY.export exp) result;
(* val exports : HSY.exports Combinator.exp Combinator.parser *)
exports : (deriv, HSY.exports exp) result;
(* val import : HSY.import Combinator.exp Combinator.parser *)
import : (deriv, HSY.import exp) result;
(* val impspec : HSY.impspec Combinator.exp Combinator.parser *)
impspec : (deriv, HSY.impspec exp) result;
(* val impdecl : HSY.impdecl Combinator.exp Combinator.parser *)
impdecl : (deriv, HSY.impdecl exp) result;
(* val impdecls : HSY.impdecl Data.l1_list Combinator.exp Combinator.parser *)
impdecls : (deriv, HSY.impdecl Data.l1_list exp) result;
(* val topdecl : HSY.topdecl Combinator.exp Combinator.parser *)
topdecl : (deriv, HSY.topdecl exp) result;
(* val topdecls : HSY.topdecls Combinator.exp Combinator.parser *)
topdecls : (deriv, HSY.topdecls exp) result;
(* val body : HSY.body Combinator.exp Combinator.parser *)
body : (deriv, HSY.body exp) result;
(* val module_ : HSY.module_ Combinator.exp Combinator.parser *)
module_ : (deriv, HSY.module_ exp) result;

}



let tk_module : deriv -> (deriv, token exp) result =
  fun d -> d.tk_module
let tk_import : deriv -> (deriv, token exp) result =
  fun d -> d.tk_import
let tk_export : deriv -> (deriv, token exp) result =
  fun d -> d.tk_export
let let_ : deriv -> (deriv, token exp) result =
  fun d -> d.let_
let where : deriv -> (deriv, token exp) result =
  fun d -> d.where
let comma : deriv -> (deriv, token exp) result =
  fun d -> d.comma
let semi : deriv -> (deriv, token exp) result =
  fun d -> d.semi
let eq : deriv -> (deriv, token exp) result =
  fun d -> d.eq
let minus : deriv -> (deriv, token exp) result =
  fun d -> d.minus
let exclam : deriv -> (deriv, token exp) result =
  fun d -> d.exclam
let two_colon : deriv -> (deriv, token exp) result =
  fun d -> d.two_colon
let dotdot : deriv -> (deriv, token exp) result =
  fun d -> d.dotdot
let bar : deriv -> (deriv, token exp) result =
  fun d -> d.bar
let l_arrow : deriv -> (deriv, token exp) result =
  fun d -> d.l_arrow
let r_arrow : deriv -> (deriv, token exp) result =
  fun d -> d.r_arrow
let r_w_arrow : deriv -> (deriv, token exp) result =
  fun d -> d.r_w_arrow
let l_paren : deriv -> (deriv, token exp) result =
  fun d -> d.l_paren
let r_paren : deriv -> (deriv, token exp) result =
  fun d -> d.r_paren
let l_brace : deriv -> (deriv, token exp) result =
  fun d -> d.l_brace
let r_brace : deriv -> (deriv, token exp) result =
  fun d -> d.r_brace
let match_or_shift_rb : deriv -> (deriv, token exp) result =
  fun d -> d.match_or_shift_rb
let opt_semi : deriv -> (deriv, token option exp) result =
  fun d -> d.opt_semi
let opt_exclam : deriv -> (deriv, token option exp) result =
  fun d -> d.opt_exclam
let conid : deriv -> (deriv, SYM.t exp) result =
  fun d -> d.conid
let dotted_conid : deriv -> (deriv, SYM.t exp) result =
  fun d -> d.dotted_conid
let literal : deriv -> (deriv, HSY.lit exp) result =
  fun d -> d.literal
let varsym : deriv -> (deriv, SYM.t exp) result =
  fun d -> d.varsym
let consym : deriv -> (deriv, SYM.t exp) result =
  fun d -> d.consym
let varid : deriv -> (deriv, SYM.t exp) result =
  fun d -> d.varid
let tyvar : deriv -> (deriv, SYM.t exp) result =
  fun d -> d.tyvar
let tycon : deriv -> (deriv, SYM.t exp) result =
  fun d -> d.tycon
let tycls : deriv -> (deriv, SYM.t exp) result =
  fun d -> d.tycls
let modid : deriv -> (deriv, SYM.t exp) result =
  fun d -> d.modid
let qvarid : deriv -> (deriv, HSY.id exp) result =
  fun d -> d.qvarid
let qconid : deriv -> (deriv, HSY.id exp) result =
  fun d -> d.qconid
let qtycon : deriv -> (deriv, HSY.id exp) result =
  fun d -> d.qtycon
let qtycls : deriv -> (deriv, HSY.id exp) result =
  fun d -> d.qtycls
let qvarsym : deriv -> (deriv, HSY.id exp) result =
  fun d -> d.qvarsym
let qconsym : deriv -> (deriv, HSY.id exp) result =
  fun d -> d.qconsym
let string : deriv -> (deriv, HSY.hs_string exp) result =
  fun d -> d.string
let integer : deriv -> (deriv, int64 exp) result =
  fun d -> d.integer
let float : deriv -> (deriv, float exp) result =
  fun d -> d.float
let fixity_int : deriv -> (deriv, int exp) result =
  fun d -> d.fixity_int
let var : deriv -> (deriv, SYM.t exp) result =
  fun d -> d.var
let qvar : deriv -> (deriv, HSY.id exp) result =
  fun d -> d.qvar
let con : deriv -> (deriv, SYM.t exp) result =
  fun d -> d.con
let gconsym : deriv -> (deriv, HSY.id exp) result =
  fun d -> d.gconsym
let qcon : deriv -> (deriv, HSY.id exp) result =
  fun d -> d.qcon
let varop : deriv -> (deriv, SYM.t exp) result =
  fun d -> d.varop
let qvarop : deriv -> (deriv, HSY.id exp) result =
  fun d -> d.qvarop
let conop : deriv -> (deriv, SYM.t exp) result =
  fun d -> d.conop
let qconop : deriv -> (deriv, HSY.id exp) result =
  fun d -> d.qconop
let op : deriv -> (deriv, SYM.t exp) result =
  fun d -> d.op
let qop : deriv -> (deriv, HSY.id exp) result =
  fun d -> d.qop
let commas1 : deriv -> (deriv, int exp) result =
  fun d -> d.commas1
let gcon : deriv -> (deriv, HSY.id exp) result =
  fun d -> d.gcon
let ops : deriv -> (deriv, SYM.t Data.l1_list exp) result =
  fun d -> d.ops
let vars : deriv -> (deriv, SYM.t Data.l1_list exp) result =
  fun d -> d.vars
let fixity : deriv -> (deriv, HSY.fixity exp) result =
  fun d -> d.fixity
let gtycon : deriv -> (deriv, HSY.gtycon exp) result =
  fun d -> d.gtycon
let type_ : deriv -> (deriv, HSY.type_ exp) result =
  fun d -> d.type_
let btype : deriv -> (deriv, HSY.btype exp) result =
  fun d -> d.btype
let atype : deriv -> (deriv, HSY.atype exp) result =
  fun d -> d.atype
let class_ : deriv -> (deriv, HSY.class_ exp) result =
  fun d -> d.class_
let context : deriv -> (deriv, HSY.context exp) result =
  fun d -> d.context
let may_be_context : deriv -> (deriv, HSY.may_be_context exp) result =
  fun d -> d.may_be_context
let simpleclass : deriv -> (deriv, HSY.simpleclass exp) result =
  fun d -> d.simpleclass
let scontext : deriv -> (deriv, HSY.scontext exp) result =
  fun d -> d.scontext
let may_be_scontext : deriv -> (deriv, HSY.may_be_scontext exp) result =
  fun d -> d.may_be_scontext
let simpletype : deriv -> (deriv, HSY.simpletype exp) result =
  fun d -> d.simpletype
let constr_arg : deriv -> (deriv, HSY.constr_arg exp) result =
  fun d -> d.constr_arg
let fielddecl : deriv -> (deriv, HSY.fielddecl exp) result =
  fun d -> d.fielddecl
let constr : deriv -> (deriv, HSY.constr exp) result =
  fun d -> d.constr
let constrs : deriv -> (deriv, HSY.constr Data.l1_list exp) result =
  fun d -> d.constrs
let newconstr : deriv -> (deriv, HSY.newconstr exp) result =
  fun d -> d.newconstr
let dclass : deriv -> (deriv, HSY.id exp) result =
  fun d -> d.dclass
let deriving : deriv -> (deriv, HSY.deriving exp) result =
  fun d -> d.deriving
let inst : deriv -> (deriv, HSY.inst exp) result =
  fun d -> d.inst
let fatype : deriv -> (deriv, HSY.fatype exp) result =
  fun d -> d.fatype
let frtype : deriv -> (deriv, HSY.frtype exp) result =
  fun d -> d.frtype
let ftype : deriv -> (deriv, HSY.ftype exp) result =
  fun d -> d.ftype
let callconv : deriv -> (deriv, HSY.callconv exp) result =
  fun d -> d.callconv
let impent : deriv -> (deriv, HSY.impent exp) result =
  fun d -> d.impent
let expent : deriv -> (deriv, HSY.expent exp) result =
  fun d -> d.expent
let safety : deriv -> (deriv, HSY.safety exp) result =
  fun d -> d.safety
let fdecl : deriv -> (deriv, HSY.fdecl exp) result =
  fun d -> d.fdecl
let pat : deriv -> (deriv, HSY.pat exp) result =
  fun d -> d.pat
let lpat : deriv -> (deriv, HSY.pat HSY.lpat exp) result =
  fun d -> d.lpat
let comma_patl_1 : deriv -> (deriv, HSY.pat Data.l1_list exp) result =
  fun d -> d.comma_patl_1
let comma_patl_2 : deriv -> (deriv, HSY.pat Data.l1_list exp) result =
  fun d -> d.comma_patl_2
let apat : deriv -> (deriv, HSY.pat HSY.apat exp) result =
  fun d -> d.apat
let fpat : deriv -> (deriv, HSY.pat HSY.fpat exp) result =
  fun d -> d.fpat
let funlhs : deriv -> (deriv, HSY.funlhs exp) result =
  fun d -> d.funlhs
let opt_where_decls : deriv -> (deriv, HSY.infexp HSY.decls option exp) result =
  fun d -> d.opt_where_decls
let let_decls : deriv -> (deriv, HSY.infexp HSY.decls exp) result =
  fun d -> d.let_decls
let rhs : deriv -> (deriv, HSY.infexp HSY.rhs exp) result =
  fun d -> d.rhs
let gdrhs : deriv -> (deriv, HSY.infexp HSY.gdrhs exp) result =
  fun d -> d.gdrhs
let guards : deriv -> (deriv, HSY.infexp HSY.guard Data.l1_list exp) result =
  fun d -> d.guards
let guard : deriv -> (deriv, HSY.infexp HSY.guard exp) result =
  fun d -> d.guard
let exp : deriv -> (deriv, HSY.infexp HSY.exp exp) result =
  fun d -> d.exp
let infixexp : deriv -> (deriv, HSY.infexp exp) result =
  fun d -> d.infixexp
let lexp : deriv -> (deriv, HSY.infexp HSY.lexp exp) result =
  fun d -> d.lexp
let fexp : deriv -> (deriv, HSY.infexp HSY.fexp exp) result =
  fun d -> d.fexp
let comma_expl_1 : deriv -> (deriv, HSY.infexp HSY.exp Data.l1_list exp) result =
  fun d -> d.comma_expl_1
let comma_expl_2 : deriv -> (deriv, HSY.infexp HSY.exp Data.l1_list exp) result =
  fun d -> d.comma_expl_2
let aexp_without_lu : deriv -> (deriv, HSY.infexp HSY.aexp exp) result =
  fun d -> d.aexp_without_lu
let braced_fbind_list_1 : deriv -> (deriv, HSY.infexp HSY.fbind Data.l1_list exp) result =
  fun d -> d.braced_fbind_list_1
let aexp : deriv -> (deriv, HSY.infexp HSY.aexp exp) result =
  fun d -> d.aexp
let qual : deriv -> (deriv, HSY.infexp HSY.qual exp) result =
  fun d -> d.qual
let alts : deriv -> (deriv, HSY.infexp HSY.alt Data.l1_list exp) result =
  fun d -> d.alts
let alt : deriv -> (deriv, HSY.infexp HSY.alt exp) result =
  fun d -> d.alt
let gdpat : deriv -> (deriv, (HSY.infexp HSY.guards * HSY.infexp HSY.exp) Data.l1_list exp) result =
  fun d -> d.gdpat
let stmts : deriv -> (deriv, (HSY.infexp HSY.stmt list * HSY.infexp HSY.exp) exp) result =
  fun d -> d.stmts
let stmt : deriv -> (deriv, HSY.infexp HSY.stmt exp) result =
  fun d -> d.stmt
let fbind : deriv -> (deriv, HSY.infexp HSY.fbind exp) result =
  fun d -> d.fbind
let gendecl : deriv -> (deriv, HSY.gendecl exp) result =
  fun d -> d.gendecl
let decls : deriv -> (deriv, HSY.infexp HSY.decls exp) result =
  fun d -> d.decls
let decl : deriv -> (deriv, HSY.infexp HSY.decl exp) result =
  fun d -> d.decl
let cdecl : deriv -> (deriv, HSY.infexp HSY.cdecl exp) result =
  fun d -> d.cdecl
let cdecls : deriv -> (deriv, HSY.infexp HSY.cdecls exp) result =
  fun d -> d.cdecls
let idecl : deriv -> (deriv, HSY.infexp HSY.idecl exp) result =
  fun d -> d.idecl
let idecls : deriv -> (deriv, HSY.infexp HSY.idecls exp) result =
  fun d -> d.idecls
let cname : deriv -> (deriv, HSY.cname exp) result =
  fun d -> d.cname
let export : deriv -> (deriv, HSY.export exp) result =
  fun d -> d.export
let exports : deriv -> (deriv, HSY.exports exp) result =
  fun d -> d.exports
let import : deriv -> (deriv, HSY.import exp) result =
  fun d -> d.import
let impspec : deriv -> (deriv, HSY.impspec exp) result =
  fun d -> d.impspec
let impdecl : deriv -> (deriv, HSY.impdecl exp) result =
  fun d -> d.impdecl
let impdecls : deriv -> (deriv, HSY.impdecl Data.l1_list exp) result =
  fun d -> d.impdecls
let topdecl : deriv -> (deriv, HSY.topdecl exp) result =
  fun d -> d.topdecl
let topdecls : deriv -> (deriv, HSY.topdecls exp) result =
  fun d -> d.topdecls
let body : deriv -> (deriv, HSY.body exp) result =
  fun d -> d.body
let module_ : deriv -> (deriv, HSY.module_ exp) result =
  fun d -> d.module_
  
(* *)
