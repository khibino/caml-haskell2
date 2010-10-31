
module A = Array
module L = List

module SYM = Symbol
module TK = Token

type hs_integer = int64
type hs_float   = float
type hs_string  = int array

type sp_con =
  | Colon
  | Unit
  | NullList
  | Tuple of int

let sp_con_str = function
  | Colon    -> ":"
  | Unit     -> "()"
  | NullList -> "[]"
  | Tuple i  -> ("(" ^ (A.fold_left (^) "" (A.make (i-1) ",")) ^ ")")

type qualifier =
  | Unq of SYM.t (* unqualified id has scope module name *)
  | Q   of SYM.t

type short =
  | N  of SYM.t
  | Sp of sp_con

type id = {
  short : short;
  qual  : qualifier;
}

let short id = id.short
let qual  id = id.qual

let short_sym id =
  match short id with
    | N n -> n
    | Sp sp -> SYM.intern (sp_con_str sp)

let qual_sym id =
  match qual id with
    | Q m | Unq m -> m

let pair_sym id =
  (qual_sym id, short_sym id)

let long_name id =
  (SYM.name (qual_sym id)) ^ "."
  ^ (SYM.name (short_sym id))

let long_sym id =
  SYM.intern (long_name id)

let to_sym id =
  match qual id with
    | Q (_) -> long_sym id
    | _ -> short_sym id


let qual_id (q, n) = {
  short = N n;
  qual  = Q q;
}

let unqual_id (m, n) = {
  short = N   n;
  qual  = Unq m;
}

let modid_prelude = SYM.intern "Prelude"
let modid_main    = SYM.intern "Main"

let varid_main    = SYM.intern "main"

let qvarid_main   = unqual_id (modid_main, varid_main)

let sym_nul    = SYM.intern ""
let sym_plus   = SYM.intern "+"
let sym_minus  = SYM.intern "-"
let sym_exclam = SYM.intern "!"

let sym_as        = SYM.intern "as"
let sym_qualified = SYM.intern "qualified"
let sym_hiding    = SYM.intern "hiding"
let sym_export    = SYM.intern "export"

let special_id spc = {
  short = Sp spc;
  qual  = Unq modid_prelude;
}

let id_colon = special_id Colon
let id_unit  = special_id Unit
let id_null_list = special_id NullList
let id_tuple i   = special_id (Tuple i)

type conid = SYM.t
type varsym = SYM.t
type comsym = SYM.t
type varid = SYM.t
type tyvar = SYM.t
type tycon = SYM.t
type tycls = SYM.t
type modid = SYM.t
type qvarid = id
type qconid = id
type qtycon = id
type qtycls = id
type qvarsym = id
type qconsym = id

type var = SYM.t
type qvar = id

type con = SYM.t
type gconsym = id
type qcon = id

type varop = SYM.t
type qvarop = id

type conop = SYM.t
type qconop = id

type op = SYM.t
type qop = id

type gcon = id

let tk_id_colon = function
  | TK.KS_COLON -> id_colon
  | _           -> failwith "Not KS_COLON token!"

type lit =
  | Char of int
  | Str  of int array
  | Int  of int64
  | Flo  of float

type ops  = op list
type vars = var list

type fixity =
  | I_left
  | I_right
  | I_infix

let default_fixity = I_left
let default_level  = 9

type gtycon =
  | GT_QTycon of qtycon
  | GT_Unit
  | GT_List
  | GT_Arrow
  | GT_Tuple of int

type type_ =
  | T_fun of (btype * type_)
  | T_btype of btype

and  btype =
  | BT_app of (btype * atype)
  | BT_atype of atype

and  atype =
  | AT_gtc of gtycon
  | AT_tyvar of tyvar
  | AT_tuple of type_ list
  | AT_list  of type_
  | AT_paren of type_

let type_of_btype_list : btype * btype list -> type_ =
  fun tl1 -> let (hd, tl) = Data.l1_rev tl1 in
             L.fold_left (fun type_ btype -> T_fun (btype, type_)) (T_btype hd) tl

let btype_of_atype_list : (atype * atype list) -> btype =
  fun (hd, tl) -> L.fold_left (fun b a -> BT_app (b, a)) (BT_atype hd) tl

let gt_qtycon = fun qtycon -> GT_QTycon qtycon
let gt_tuple  = fun i -> GT_Tuple i
(* GT_Unit *)
(* GT_List *)
(* GT_Arrow *)
(* let gt_arrow : TK.type_ -> gtycon *)
(*   = TK.with_region_just GT_Arrow *)
  
let at_gtc   = fun gtc -> AT_gtc gtc
let at_tyvar = fun tyvar -> AT_tyvar tyvar
let at_tuple = fun types -> AT_tuple (Data.l1_list types)
let at_list  = fun type_ -> AT_list  type_
let at_paren = fun type_ -> AT_paren type_

(*
type class_tyvar =
  | CT_val of tyvar
  | CT_app of tyvar * atype list
*)

type class_ = qtycls * (tyvar * atype list)
let class_tval = fun qtycls tyvar -> (qtycls, (tyvar , []))
let class_tapp =
  fun qtycls tyvar atype_list -> (qtycls, (tyvar, Data.l1_list atype_list))

type context = class_ list
type may_be_context = context option

type simpleclass = qtycls * tyvar
let simpleclass = Data.tuple2

type scontext = simpleclass list
type may_be_scontext = scontext option

type simpletype = tycon * tyvar list
let simpletype = Data.tuple2

type may_banana_atype = atype * bool

let may_banana_atype =
  fun exclam atype -> match exclam with
    | Some _ -> (atype, true)
    | None   -> (atype, false)


type constr_fld =
  | CF_type of type_
  | CF_satype of atype

let cf_type = fun type_ -> CF_type type_ 
let cf_satype = fun sat -> CF_satype sat

type fielddecl = vars * constr_fld

let fielddecl = fun vars constr_fld -> (Data.l1_list vars, constr_fld)

type constr_arg =
  | CA_btype  of btype
  | CA_satype of atype

let ca_btype = fun btype -> CA_btype btype
let ca_satype = fun sat -> CA_satype sat

type constr = 
  | CO_con of con * may_banana_atype list
  | CO_bin of constr_arg * conop * constr_arg
  | CO_rec of con * fielddecl list

let co_con = fun con atypel -> CO_con (con, atypel)
let co_bin = fun a1 conop a2 -> CO_bin (a1, conop, a2)
let co_rec = fun con flddecll -> CO_rec (con, flddecll)

type constrs = constr list

type newconstr =
  | NC_con of con * atype
  | NC_rec of con * var * type_

let nc_con = fun con atype -> NC_con (con, atype)
let nc_rec = fun con var type_ -> NC_rec (con, var, type_)

type dclass = qtycls
type deriving = dclass list

type inst =
  | IN_tyapp of gtycon * tyvar list
  | IN_tuple of tyvar list
  | IN_list  of tyvar
  | IN_fun   of tyvar * tyvar

let in_tyapp_zero = fun gtycon -> IN_tyapp (gtycon, []) 
let in_tyapp = fun gtycon tyvarl -> IN_tyapp (gtycon, tyvarl)
let in_tuple = fun tyvarl -> IN_tuple (Data.l1_list tyvarl)
let in_list  = fun tyvar -> IN_list tyvar
let in_fun   = fun tyvar1 tyvar2 -> IN_fun (tyvar1, tyvar2)

type fatype = qtycon * atype list

let fatype = Data.tuple2

type frtype = FRT_fa of fatype | FRT_unit

let frt_fa = fun fatype -> FRT_fa fatype

type ftype =
  | FT_fr of frtype
  | FT_fun of fatype * ftype

let ft_fr = fun frtype -> FT_fr frtype
let ft_fun = fun fatype ftype -> FT_fun (fatype, ftype)

type callconv = varid
type impent = hs_string option
type expent = hs_string option
type safety = varid

type fdecl =
  | FO_import of callconv * safety option * impent * var * ftype
  | FO_export of callconv * expent * var * ftype

let fo_import =
  fun callconv safety impent var ftype ->
    FO_import (callconv, safety, impent, var, ftype)

let fo_export =
  fun callconv expent var ftype ->
    FO_export (callconv, expent, var, ftype)

type 'pat fpat = (qvar * 'pat)

type 'pat apat =
  | AP_var  of (var * 'pat apat option)
  | AP_gcon of gcon
  | AP_qcon of qcon * 'pat fpat list
  | AP_lit  of lit
  | AP_all
  | AP_paren of 'pat
  | AP_tuple of 'pat list
  | AP_list  of 'pat list
  | AP_irr   of 'pat apat

type 'pat lpat =
  | LP_apat of 'pat apat
  | LP_neg_int of hs_integer
  | LP_neg_float of hs_float
  | LP_gcon of (gcon * 'pat apat list)

type pat  =
  | P_infix of pat lpat * qconop * pat
  | P_neg_int of hs_integer
  | P_neg_float of hs_float
  | P_lpat of pat lpat

let fpat = Data.tuple2

let ap_var = fun var as_p -> AP_var (var, as_p)

let ap_gcon = fun gcon -> AP_gcon gcon
let ap_qcon = fun qcon flist -> AP_qcon (qcon, flist)
let ap_lit  = fun lit -> AP_lit lit
let ap_all : TK.type_ -> pat apat = fun _ -> AP_all
let ap_paren = fun pat -> AP_paren pat
let ap_tuple = fun pl -> AP_tuple (Data.l1_list pl)
let ap_list  = fun pl -> AP_list  (Data.l1_list pl)
let ap_irr   = fun apat -> AP_irr apat

let lp_apat = fun apat -> LP_apat apat
let lp_neg_int = fun i -> LP_neg_int (Int64.neg i)
let lp_neg_float = fun f -> LP_neg_float (~-. f)
let lp_gcon = fun gcon pl -> LP_gcon (gcon, Data.l1_list pl)

let p_infix = fun lpat qconop pat -> P_infix (lpat, qconop, pat)
let p_neg_int = fun i -> P_neg_int (Int64.neg i)
let p_neg_float = fun f -> P_neg_float (~-. f)
let p_lpat = fun lp -> P_lpat lp

type funlhs =
  | FL_var  of var * pat apat list
  | FL_op   of pat * varop * pat
  | FL_nest of funlhs * pat apat list

let fl_var = fun var apat_list -> FL_var (var, Data.l1_list apat_list)
let fl_op  = fun p_left varop p_right -> FL_op (p_left, varop, p_right)
let fl_nest = fun funlhs apat_list -> FL_nest (funlhs, Data.l1_list apat_list)

type 'infexp exp = 'infexp * (may_be_context * type_) option

let exp_type : may_be_context -> type_ -> (may_be_context * type_) = Data.tuple2
let exp : 'infexp -> (may_be_context * type_) option -> 'infexp exp = Data.tuple2

type 'infexp fbind = qvar * 'infexp exp

let fbind qvar exp = Data.tuple2 qvar exp

type gendecl =
  | GD_vars of vars * may_be_context * type_
  | GD_fixity of fixity * int * ops
  | GD_empty

let gd_vars = fun vars context type_ ->
  GD_vars (Data.l1_list vars, context, type_)

let gd_fixity = fun fixity level ops ->
      GD_fixity (fixity,
                 (match level with
                   | Some level -> level
                   | None -> default_level),
                 Data.l1_list ops)

type lhs =
  | LHS_fun of funlhs
  | LHS_pat of pat

let lhs_fun = fun funlhs -> LHS_fun funlhs
let lhs_pat = fun pat -> LHS_pat pat

type 'infexp decl  =
  | D_gen of gendecl
  | D_val of lhs * 'infexp rhs

and  'infexp decls = 'infexp decl list

and  'infexp guard =
  | GU_pat of pat * 'infexp
  | GU_let of 'infexp decls
  | GU_exp of 'infexp

and  'infexp guards = 'infexp guard list

and  'infexp gdrhs = ('infexp guards * 'infexp exp) list

and  'infexp rhs =
  | RHS_exp of 'infexp exp   * 'infexp decls option
  | RHS_gd  of 'infexp gdrhs * 'infexp decls option

let d_gen = fun gendecl -> D_gen gendecl
let d_val = fun lhs rhs -> D_val (lhs, rhs)

let gu_pat = fun pat infexp -> GU_pat (pat, infexp)
let gu_let = fun decll -> GU_let decll
let gu_exp = fun exp -> GU_exp exp

let gdrhs_pair :
    'infexp guard Data.l1_list -> 'infexp exp -> ('infexp guards * 'infexp exp) =
  fun gd exp -> (Data.l1_list gd, exp)
let gdrhs : ('infexp guards * 'infexp exp) Data.l1_list -> 'infexp gdrhs = Data.l1_list

let rhs_exp = fun exp decls -> RHS_exp (exp, decls)
let rhs_gd  = fun gdrhs decls -> RHS_gd (gdrhs, decls)

type 'infexp stmt  =
  | ST_exp of 'infexp exp
  | ST_act of pat * 'infexp exp
  | ST_let of 'infexp decls
  | ST_empty

(*
(* 'infexp stmt list よりもこちらの表現の方が parse 時には自然?
   後で変えるかも *)
type 'infexp stmts =
  | SS_cons of 'infexp stmt * 'infexp stmts
  | SS_exp of 'infexp exp
*)

let st_exp = fun exp -> ST_exp exp
let st_act = fun pat exp -> ST_act (pat, exp)
let st_let_ = fun decls -> ST_let decls
let st_empty = fun _ -> ST_empty
let stmts_cons_nil = fun exp -> ([], exp)
let stmts_cons = fun stmt (stmt_list, exp) -> (stmt :: stmt_list, exp)

type 'infexp qual  =
  | Q_gen of pat * 'infexp exp
  | Q_let of 'infexp decls
  | Q_exp of 'infexp exp

let q_gen = fun pat exp -> Q_gen (pat, exp)
let q_let = fun decls -> Q_let decls
let q_exp = fun exp -> Q_exp exp

type 'infexp gdpat = ('infexp guards * 'infexp exp) list

let gp_gdpat = fun g e -> (Data.l1_list g, e)

type 'infexp alt =
  | AL_pat   of pat * 'infexp exp * 'infexp decls option
  | AL_gdpat of pat * 'infexp gdpat * 'infexp decls option
  | AL_empty

let al_pat = fun pat exp decls -> AL_pat (pat, exp, decls)

let al_gdpat = fun pat gdp decls -> AL_gdpat (pat, Data.l1_list gdp, decls)

type 'infexp aexp =
  | Var    of qvar
  | Con    of gcon
  | Lit    of lit
  | Paren  of 'infexp exp
  | Tuple  of 'infexp exp list
  | List   of 'infexp exp list
  | ASeq   of 'infexp exp * 'infexp exp option * 'infexp exp option (* arithmetic sequence *)
  | Comp   of 'infexp exp * 'infexp qual list (* list comprehension *)
  | LeftS  of 'infexp * qop (* left section*)
  | RightS of qop * 'infexp (* right section*)
  | ConsL  of qcon * 'infexp fbind list (* labeled construction *)
  | UpdL   of 'infexp aexp * 'infexp fbind list (* labeled update *)

(* 以下の実装のように引数を一つづつ部分適用するのは効率が良くないはず。
 * 一度に複数の引数を適用するように変更するかも。
 *)
type 'infexp fexp =
  | FApp of ('infexp fexp * 'infexp aexp)
  | AExp of 'infexp aexp

type 'infexp lexp =
  | Lambda of pat apat list * 'infexp exp
  | Let    of 'infexp decls * 'infexp exp
  | If     of 'infexp exp * 'infexp exp * 'infexp exp
  | Case   of 'infexp exp * 'infexp alt list
  | Do     of 'infexp stmt list * 'infexp exp
  | FExp   of 'infexp fexp

type infexp =
  | OpApp of infexp lexp * qop * infexp
  | Neg   of infexp
  | LExp  of infexp lexp

(* infexp construction *)
let op_app = fun lexp qop infexp -> OpApp (lexp, qop, infexp)
let neg = fun infexp -> Neg infexp
let lexp = fun lexp -> LExp lexp

(* aexp construction *)
let var = fun qvar -> Var qvar
let con = fun gcon -> Con gcon
let lit = fun lit -> Lit lit
let paren = fun exp -> (Paren exp : infexp aexp)
let tuple = fun el  -> (Tuple (Data.l1_list el) : infexp aexp)
let list  = fun el  -> (List (Data.l1_list el) : infexp aexp)

let aseq = fun _1st _2nd last -> ASeq (_1st, _2nd, last)

let comp = fun exp ql -> Comp (exp, Data.l1_list ql)
let left_sec   = fun infexp qop -> LeftS (infexp, qop)
let right_sec  = fun qop infexp -> RightS (qop, infexp)
let lbl_cons = fun qcon bl -> ConsL (qcon, bl)
let lbl_upd  = fun aexp bl -> UpdL  (aexp, Data.l1_list bl)

let lbl_upd_of_fbinds_list = fun aexp bl ->L.fold_left lbl_upd aexp bl

(* fexp construction *)
let fexp_of_aexp_list : infexp aexp * infexp aexp list -> infexp fexp =
  fun (e, es) -> L.fold_left (fun fexp e -> FApp (fexp, e)) (AExp e) es

(* lexp construction *)
let lambda = fun patl exp -> Lambda (Data.l1_list patl, exp)
let let_ = fun decll exp -> Let (decll, exp)
let if_  = fun p t e -> If (p, t, e)
let case = fun  exp altl -> Case (exp, Data.l1_list altl)
let do_ = fun (stmts, e) -> Do (stmts, e)
let fexp : infexp fexp -> infexp lexp = fun fexp -> FExp fexp

type 'infexp cdecl  =
  | CD_gen of gendecl
  | CD_val of lhs * 'infexp rhs

type 'infexp cdecls = 'infexp cdecl list

let cd_gen = fun gendecl -> CD_gen gendecl
let cd_val = fun lhs rhs -> CD_val (lhs, rhs)

type 'infexp idecl  =
  | ID_val of lhs * 'infexp rhs
  | ID_empty

type 'infexp idecls = 'infexp idecl list

let id_val = fun lhs rhs -> ID_val (lhs, rhs)

type cname =
  | CN_var of var
  | CN_con of con

let cn_var = fun var -> CN_var var
let cn_con = fun con -> CN_con con

type 'sym ex_flags =
  | EXF_all
  | EXF_list of 'sym list

let exf_all = fun _ -> EXF_all
let exf_list = fun syms -> EXF_list syms

type export =
  | EX_var of qvar
  | EX_con of qtycon * cname ex_flags option
  | EX_cls of qtycls * qvar ex_flags option
  | EX_mod of modid

type exports = export list

let ex_var = fun var -> EX_var var
let ex_con = fun qtycon ex_flags -> EX_con (qtycon, ex_flags)
let ex_cls = fun qtycls ex_flags -> EX_cls (qtycls, ex_flags)
let ex_mod = fun modid -> EX_mod modid

type import =
  | IM_var of var
  | IM_con of tycon * cname ex_flags option
  | IM_cls of tycls * var ex_flags option

let im_var = fun var -> IM_var var
let im_con = fun tycon ex_flags -> IM_con (tycon, ex_flags)
let im_cls = fun tycls ex_flags -> IM_cls (tycls, ex_flags)

type impspec =
  | IS_imp of import list
  | IS_hide of import list

let is_imp = fun il -> IS_imp il
let is_hide = fun il -> IS_hide il

type impdecl =
  | IMD_imp of bool * modid * modid option * impspec option
  | IMD_empty

type impdecls = impdecl list

let imd_imp = fun qual modid as_modid spec ->
  IMD_imp ((match qual with | Some _ -> true | None -> false),
           modid, as_modid, spec)

type topdecl =
  | TD_type of simpletype * type_
  | TD_data of may_be_context * simpletype * constrs option * deriving option
  | TD_newtype of may_be_context * simpletype * newconstr * deriving option
  | TD_class of may_be_scontext * tycls * tyvar * infexp cdecls option
  | TD_instance of may_be_scontext * qtycls * inst * infexp idecls option
  | TD_default of type_ list
  | TD_foreign of fdecl
  | TD_decl of infexp decl

type topdecls = topdecl list

let td_type = fun simpletype type_ -> TD_type (simpletype, type_)

let td_data = fun context simpletype constrs deriving ->
  TD_data (context, simpletype,
           Data.with_option Data.l1_list constrs,
           deriving)

let td_newtype = fun context simpletype newconstr deriving ->
  TD_newtype (context, simpletype, newconstr, deriving)

let td_class = fun context tycls tyvar cdecls ->
  TD_class (context, tycls, tyvar, cdecls)

let td_instance = fun context tycls inst idecls ->
  TD_instance (context, tycls, inst, idecls)

let td_default = fun tl -> TD_default tl

let td_foreign = fun fdecl -> TD_foreign fdecl

let td_decl = fun decl -> TD_decl decl

type body = impdecls * topdecls

let body = fun impdecls topdecls -> (Data.l1_list impdecls, topdecls)
let body_no_top = fun impdecls -> (Data.l1_list impdecls, [])
let body_no_imp = fun topdecls -> ([], topdecls)

type may_be_exports =
  | EXS_all
  | EXS_list of exports

type module_ = modid * (may_be_exports * body)


let module_ : (modid -> 'a) -> modid -> exports option -> body -> module_ =
  fun register_module modid ->
    let _ = register_module modid in
    fun exports body ->
      (modid,
       ((match exports with
         | Some ex -> EXS_list ex
         | None    -> EXS_all),
        body))

let module_main register_module =
  let modid = modid_main in
  let _ = register_module modid in
  fun body -> (modid, (EXS_list [EX_var qvarid_main], body))

(*  *)
