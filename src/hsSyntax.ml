
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

let sym_prelude = SYM.intern "Prelude"

let sym_nul    = SYM.intern ""
let sym_plus   = SYM.intern "+"
let sym_minus  = SYM.intern "-"
let sym_exclam = SYM.intern "!"

let sym_as        = SYM.intern "as"
let sym_qualified = SYM.intern "qualified"
let sym_hiding    = SYM.intern "hiding"

let special_id spc = {
  short = Sp spc;
  qual  = Unq sym_prelude;
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

let tk_id_colon = TK.with_region (function
  | TK.KS_COLON -> id_colon
  | _           -> failwith "Not KS_COLON token!")

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

let comp2_region a b cons = TK.form_between a (cons (fst a) (fst b)) b
let comp3_region a b c cons = TK.form_between a (cons (fst a) (fst b) (fst c)) c

let tuple2_region a b = comp2_region a b Data.tuple2

let comp2_left_opt  l r cons = match l with
  | Some l -> comp2_region l r (fun a b -> cons (Some a) b)
  | None   -> TK.with_region (fun b -> cons None b) r

let comp2_right_opt l r cons = match r with
  | Some r -> comp2_region l r (fun a b -> cons a (Some b))
  | None   -> TK.with_region (fun a -> cons a None) l

type fix_later = unit

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

let type_of_btype_list
    : (btype * btype list) * TK.region -> type_ * TK.region =
  TK.with_region (fun
    tl1 -> let (hd, tl) = Data.l1_rev tl1 in
           L.fold_left (fun type_ btype -> T_fun (btype, type_)) (T_btype hd) tl)

let btype_of_atype_list
    : (atype * atype list) * TK.region -> btype * TK.region =
  TK.with_region (fun
    (hd, tl) -> L.fold_left (fun b a -> BT_app (b, a)) (BT_atype hd) tl)

let gt_qtycon = TK.with_region (fun qtycon -> GT_QTycon qtycon)
let gt_tuple  = TK.with_region (fun i -> GT_Tuple i)
(* GT_Unit *)
(* GT_List *)
let gt_arrow : (TK.typ * TK.region) -> (gtycon * TK.region)
  = TK.with_region_just GT_Arrow
  
let at_gtc   = TK.with_region (fun gtc -> AT_gtc gtc)
let at_tyvar = TK.with_region (fun tyvar -> AT_tyvar tyvar)
let at_tuple = TK.with_region (fun types -> AT_tuple (Data.l1_list types))
let at_list  = TK.with_region (fun type_ -> AT_list  type_)
let at_paren = TK.with_region (fun type_ -> AT_paren type_)

(*
type class_tyvar =
  | CT_val of tyvar
  | CT_app of tyvar * atype list
*)

type class_ = qtycls * (tyvar * atype list)
let class_tval qtycls tyvar =
  comp2_region qtycls tyvar (fun a b -> (a, (b, [])))
let class_tapp qtycls tyvar atype_list =
  comp3_region qtycls tyvar atype_list (fun a b c -> (a, (b, Data.l1_list c)))

type context = class_ list
type may_be_context = context option

type simpleclass = qtycls * tyvar
let simpleclass = tuple2_region

type simpletype = tycon * tyvar list
let simpletype = tuple2_region

type may_banana_atype = atype * bool
let may_banana_atype exclam atype =
  comp2_right_opt atype exclam (fun a -> function
    | Some _ -> (a, true)
    | None   -> (a, false))

type constr_fld =
  | CF_type of type_
  | CF_satype of atype

let cf_type type_ = TK.with_region (fun a -> CF_type a) type_
let cf_satype sat = TK.with_region (fun a -> CF_satype a) sat

type fielddecl = vars * constr_fld

let fielddecl vars constr_fld =
  comp2_region vars constr_fld (fun a b -> (Data.l1_list a, b))

type constr_arg =
  | CA_btype  of btype
  | CA_satype of atype

let ca_btype btype = TK.with_region (fun a -> CA_btype a) btype
let ca_satype sat = TK.with_region (fun a -> CA_satype a) sat

type constr = 
  | CO_con of con * may_banana_atype list
  | CO_bin of constr_arg * conop * constr_arg
  | CO_rec of con * fielddecl list

let co_con con atypel = comp2_region con atypel (fun a b -> CO_con (a, b))
let co_bin a1 conop a2 = comp3_region a1 conop a2 (fun a b c -> CO_bin (a, b, c))
let co_rec con flddecll = comp2_region con flddecll (fun a b -> CO_rec (a, b))

type newconstr =
  | NC_con of con * atype
  | NC_rec of con * var * type_

let nc_con con atype = comp2_region con atype (fun a b -> NC_con (a, b))
let nc_rec con var type_ = comp3_region con var type_ (fun a b c -> NC_rec (a, b, c))

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

let fpat = tuple2_region

let ap_var var as_p = comp2_right_opt var as_p (fun a b -> AP_var (a, b))

let ap_gcon = TK.with_region (fun gcon -> AP_gcon gcon)
let ap_qcon qcon flist = comp2_region qcon flist (fun a b -> AP_qcon (a, b))
let ap_lit  = TK.with_region (fun lit -> AP_lit lit)
let ap_all : (TK.typ * TK.region) -> (pat apat * TK.region)
  = TK.with_region_just AP_all
let ap_paren a = TK.with_region (fun pat -> AP_paren pat) a
let ap_tuple a = TK.with_region (fun pl -> AP_tuple (Data.l1_list pl)) a
let ap_list  a = TK.with_region (fun pl -> AP_list  (Data.l1_list pl)) a
let ap_irr   a = TK.with_region (fun apat -> AP_irr apat) a

let lp_apat apat = TK.with_region (fun apat -> LP_apat apat) apat
let lp_neg_int = TK.with_region (fun i -> LP_neg_int (Int64.neg i))
let lp_neg_float = TK.with_region (fun f -> LP_neg_float (~-. f))
let lp_gcon gcon pl = comp2_region gcon pl (fun a b -> LP_gcon (a, Data.l1_list b))

let p_infix lpat qconop pat =
  comp3_region lpat qconop pat (fun a b c -> P_infix (a, b, c))
let p_neg_int = TK.with_region (fun i -> P_neg_int (Int64.neg i))
let p_neg_float = TK.with_region (fun f -> P_neg_float (~-. f))
let p_lpat = TK.with_region (fun lp -> P_lpat lp)

type funlhs =
  | FL_var  of var * pat apat list
  | FL_op   of pat * varop * pat
  | FL_nest of funlhs * pat apat list

let fl_var var apat_list =
  comp2_region var apat_list (fun a b -> FL_var (a, Data.l1_list b))
let fl_op  p_left varop p_right =
  comp3_region p_left varop p_right (fun a b c -> FL_op (a, b, c))
let fl_nest funlhs apat_list =
  comp2_region funlhs apat_list (fun a b -> FL_nest (a, Data.l1_list b))

type 'infexp exp = 'infexp * (may_be_context * type_) option

let exp_type context type_ = comp2_left_opt context type_ Data.tuple2
let exp infexp type_ = comp2_right_opt infexp type_ Data.tuple2

type 'infexp fbind = qvar * 'infexp exp

let fbind qvar exp = tuple2_region qvar exp

type gendecl =
  | GD_vars of vars * may_be_context * type_
  | GD_fixity of fixity * int * ops
  | GD_empty

let gd_vars vars context type_ = match context with
  | Some cont -> comp3_region vars cont type_ (fun a b c -> GD_vars (Data.l1_list a, Some b, c))
  | None      -> comp2_region vars type_      (fun a c -> GD_vars (Data.l1_list a, None, c))

let gd_fixity fixity level ops = match level with
  | Some level -> comp3_region fixity level ops (fun a b c -> GD_fixity (a, b, Data.l1_list c))
  | None       -> comp2_region fixity ops       (fun a c -> GD_fixity (a, default_level, Data.l1_list c))

type lhs =
  | LHS_fun of funlhs
  | LHS_pat of pat

let lhs_fun funlhs = TK.with_region (fun a -> LHS_fun a) funlhs
let lhs_pat pat    = TK.with_region (fun a -> LHS_pat a) pat

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

let d_gen gendecl = TK.with_region (fun a -> D_gen a) gendecl
let d_val lhs rhs = comp2_region lhs rhs (fun a b -> D_val (a, b))

let gu_pat pat infexp = comp2_region pat infexp (fun a b -> GU_pat (a, b))
let gu_let decll  = TK.with_region (fun a -> GU_let a) decll
let gu_exp exp    = TK.with_region (fun a -> GU_exp a) exp

let gdrhs_pair gd exp = comp2_region gd exp (fun a b -> (Data.l1_list a, b))
let gdrhs pair_list = TK.with_region Data.l1_list pair_list

let rhs_exp exp   decls = comp2_right_opt exp   decls (fun a b -> RHS_exp (a, b))
let rhs_gd  gdrhs decls = comp2_right_opt gdrhs decls (fun a b -> RHS_gd (a, b))

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

let st_exp exp = TK.with_region (fun a -> ST_exp a) exp
let st_act pat exp = comp2_region pat exp (fun a b -> ST_act (a, b))
let st_let_ decls = TK.with_region (fun a -> ST_let a) decls
let st_empty semi = TK.with_region (fun _ -> ST_empty) semi
let stmts_cons_nil exp =
  TK.with_region (fun a -> ([], a)) exp
let stmts_cons stmt stmt_list =
  comp2_region stmt stmt_list (fun a (b, c) -> (a :: b, c))

type 'infexp qual  =
  | Q_gen of pat * 'infexp exp
  | Q_let of 'infexp decls
  | Q_exp of 'infexp exp

let q_gen pat exp = comp2_region pat exp (fun a b -> Q_gen (a, b))
let q_let decls = TK.with_region (fun a -> Q_let a) decls
let q_exp exp   = TK.with_region (fun a -> Q_exp a) exp

type 'infexp gdpat = ('infexp guards * 'infexp exp) list

let gp_gdpat g e =
  comp2_region g e
    (fun a b -> (Data.l1_list a, b))

type 'infexp alt =
  | AL_pat   of pat * 'infexp exp * 'infexp decls option
  | AL_gdpat of pat * 'infexp gdpat * 'infexp decls option
  | AL_empty

let al_pat pat exp = function
  | Some decls -> comp3_region pat exp decls (fun a b c -> AL_pat (a, b, Some c))
  | None       -> comp2_region pat exp (fun a b -> AL_pat (a, b, None))

let al_gdpat pat gdp =
  function
    | Some decls -> comp3_region pat gdp decls (fun a b c -> AL_gdpat (a, Data.l1_list b, Some c))
    | None       -> comp2_region pat gdp (fun a b -> AL_gdpat (a, Data.l1_list b, None))

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
let op_app lexp qop infexp =
  comp3_region lexp qop infexp (fun a b c -> OpApp (a, b, c))
let neg = TK.with_region (fun infexp -> Neg infexp)
let lexp = TK.with_region (fun lexp -> LExp lexp)

(* aexp construction *)
let var = TK.with_region (fun qvar -> Var qvar)
let con = TK.with_region (fun gcon -> Con gcon)
let lit = TK.with_region (fun lit -> Lit lit)
let paren = TK.with_region (fun exp -> (Paren exp : infexp aexp))
let tuple = TK.with_region (fun el  -> (Tuple (Data.l1_list el) : infexp aexp))
let list  = TK.with_region (fun el  -> (List (Data.l1_list el) : infexp aexp))

(* arithmetic sequence はブラケット [ ] の region となるので手抜き  *)
let aseq _1st _2nd last =
  TK.form_between
    _1st
    (ASeq (fst _1st,
           Data.with_option fst _2nd,
           Data.with_option fst last))
    _1st

let comp exp ql = comp2_region exp ql (fun a b -> Comp (a, Data.l1_list b))
let left_sec  infexp qop = comp2_region infexp qop (fun a b -> LeftS (a, b))
let right_sec qop infexp = comp2_region qop infexp (fun a b -> RightS (a, b))
let lbl_cons qcon bl = comp2_region qcon bl (fun a b -> ConsL (a, b))
let lbl_upd  aexp bl = comp2_region aexp bl (fun a b -> UpdL  (a, Data.l1_list b))

let lbl_upd_of_fbinds_list aexp bl =
  L.fold_left lbl_upd aexp bl

(* fexp construction *)
let fexp_of_aexp_list
    : (infexp aexp * infexp aexp list) * TK.region -> infexp fexp * TK.region =
  TK.with_region (fun
    (e, es)  -> L.fold_left (fun fexp e -> FApp (fexp, e)) (AExp e) es)

(* lexp construction *)
let lambda patl exp =
  comp2_region patl exp (fun a b -> Lambda (Data.l1_list a, b))
let let_ decll exp =
  comp2_region decll exp (fun a b -> Let (a, b))
let if_ p t e =
  comp3_region p t e (fun a b c -> If (a, b, c))
let case exp altl =
  comp2_region exp altl (fun a b -> Case (a, Data.l1_list b))
let do_ stmts = TK.with_region (fun (a, b) -> Do (a, b)) stmts
let fexp : infexp fexp * TK.region -> infexp lexp * TK.region =
  TK.with_region (fun fexp -> FExp fexp)
