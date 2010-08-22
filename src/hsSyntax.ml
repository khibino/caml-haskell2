
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

let sym_to_qconid = TK.with_region (fun qs -> qual_id (TK.syms_of_qstring (SYM.name qs)))

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

type gcon = id

let tk_id_colon = TK.with_region (function
  | TK.KS_COLON -> id_colon
  | _           -> failwith "Not KS_COLON token!")


type lit =
  | Char of int
  | Str  of int array
  | Int  of int64
  | Flo  of float


type fixity =
  | I_left
  | I_right
  | I_infix

let comp2_region a b cons =
  TK.form_between a (cons (fst a) (fst b)) b

let tuple2_region a b =
  comp2_region a b Data.tuple2

(*let comp3_region a b c cons =
  TK.form_between a (cons (fst a) (fst b) (fst c)) c*)

type fix_later = unit

type gtycon =
  | GT_QTycon of id
  | GT_Unit
  | GT_List
  | GT_Arrow
  | GT_Tuple of int

type typ =
  | T_fun of (btype * typ)
  | T_btype of btype

and  btype =
  | BT_app of (btype * atype)
  | BT_atype of atype

and  atype =
  | AT_gtc of gtycon
  | AT_tyvar of SYM.t
  | AT_tuple of typ list
  | AT_list  of typ
  | AT_paren of typ

let typ_of_btype_list
    : (btype * btype list) * TK.region -> typ * TK.region =
  TK.with_region (fun
    tl1 -> let (hd, tl) = Data.l1_rev tl1 in
           L.fold_left (fun typ btype -> T_fun (btype, typ)) (T_btype hd) tl)

let btype_of_atype_list
    : (atype * atype list) * TK.region -> btype * TK.region =
  TK.with_region (fun
    (hd, tl) -> L.fold_left (fun b a -> BT_app (b, a)) (BT_atype hd) tl)

let gt_qtycon = TK.with_region (fun id -> GT_QTycon id)
let gt_tuple  = TK.with_region (fun i -> GT_Tuple i)
(* GT_Unit *)
(* GT_List *)
let gt_arrow : (TK.typ * TK.region) -> (gtycon * TK.region)
  = TK.with_region_just GT_Arrow
  
let at_gtc   = TK.with_region (fun gtc -> AT_gtc gtc)
let at_tyvar = TK.with_region (fun tyvar -> AT_tyvar tyvar)
let at_tuple = TK.with_region (fun types -> AT_tuple (Data.l1_list types))
let at_list  = TK.with_region (fun typ -> AT_list  typ)
let at_paren = TK.with_region (fun typ -> AT_paren typ)

type cls = id * SYM.t
let cls = tuple2_region

type context = cls list

type 'pat fpat = (id * 'pat)

type 'pat apat =
  | AP_var  of (SYM.t * 'pat apat option)
  | AP_gcon of gcon
  | AP_qcon of id * 'pat fpat list
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
  | P_infix of pat lpat * id * pat
  | P_neg_int of hs_integer
  | P_neg_float of hs_float
  | P_lpat of pat lpat

let fpat = tuple2_region

let ap_var var = function
  | Some as_p -> comp2_region var as_p (fun a b -> AP_var (a, Some b))
  | None      -> (AP_var (fst var, None), snd var)

let ap_gcon = TK.with_region (fun id -> AP_gcon id)
let ap_qcon id flist = comp2_region id flist (fun a b -> AP_qcon (a, b))
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
  TK.form_between lpat (P_infix (fst lpat, fst qconop, fst pat)) pat
let p_neg_int = TK.with_region (fun i -> P_neg_int (Int64.neg i))
let p_neg_float = TK.with_region (fun f -> P_neg_float (~-. f))
let p_lpat = TK.with_region (fun lp -> P_lpat lp)

type 'infexp exp = 'infexp * (context option * typ) option

let exp_typ context typ = match context with
  | Some c -> comp2_region c typ (fun a b -> ((Some a), b))
  | None   -> ((None, fst typ), snd typ)

let exp infexp = function
  | Some typ -> comp2_region infexp typ (fun a b -> (a, Some b))
  | None     -> ((fst infexp, None), snd infexp)

type 'infexp qual  = fix_later
type 'infexp fbind = id * 'infexp exp
type 'infexp alt   = fix_later
type 'infexp stmt  = fix_later
type 'infexp decl  = fix_later

let fbind qvar exp = comp2_region qvar exp Data.tuple2

type 'infexp aexp =
  | Var    of id
  | Con    of id
  | Lit    of lit
  | Paren  of 'infexp exp
  | Tuple  of 'infexp exp list
  | List   of 'infexp exp list
  | ASeq   of 'infexp exp * 'infexp exp option * 'infexp exp option (* arithmetic sequence *)
  | Comp   of 'infexp exp * 'infexp qual list (* list comprehension *)
  | LeftS  of 'infexp * id (* left section*)
  | RightS of id * 'infexp (* right section*)
  | ConsL  of id * 'infexp fbind list (* labeled construction *)
  | UpdL   of 'infexp aexp * 'infexp fbind list (* labeled update *)

(* 以下の実装のように引数を一つづつ部分適用するのは効率が良くないはず。
 * 一度に複数の引数を適用するように変更するかも。
 *)
type 'infexp fexp =
  | FApp of ('infexp fexp * 'infexp aexp)
  | AExp of 'infexp aexp

type 'infexp lexp =
  | Lambda of pat apat list * 'infexp exp
  | Let    of 'infexp decl list * 'infexp exp
  | If     of 'infexp exp * 'infexp exp * 'infexp exp
  | Case   of 'infexp exp * 'infexp alt list
  | Do     of 'infexp stmt list * 'infexp exp
  | FExp   of 'infexp fexp

type infexp =
  | OpApp of infexp lexp * id * infexp
  | Neg   of infexp
  | LExp  of infexp lexp

(* infexp construction *)
let op_app lexp id infexp = TK.form_between lexp (OpApp (fst lexp, fst id, fst infexp)) infexp
let neg = TK.with_region (fun infexp -> Neg infexp)
let lexp = TK.with_region (fun lexp -> LExp lexp)

(* aexp construction *)
let var = TK.with_region (fun id -> Var id)
let con = TK.with_region (fun id -> Con id)
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
let left_sec  infexp id = comp2_region infexp id (fun a b -> LeftS (a, b))
let right_sec id infexp = comp2_region id infexp (fun a b -> RightS (a, b))
let lbl_cons id bl = comp2_region id bl (fun a b -> ConsL (a, b))
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
  TK.form_between p (If (fst p, fst t, fst e)) e
let case exp altl =
  comp2_region exp altl (fun a b -> Case (a, Data.l1_list b))
let do_ stmts exp = 
  comp2_region stmts exp (fun a b -> Do (a, b))
let fexp : infexp fexp * TK.region -> infexp lexp * TK.region =
  TK.with_region (fun fexp -> FExp fexp)
