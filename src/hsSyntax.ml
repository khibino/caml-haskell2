
module A = Array
module L = List

module SYM = Symbol
module TK = Token

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

let unqual_id (n, m) = {
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

let tk_id_colon = TK.with_region (function
  | TK.KS_COLON -> id_colon
  | _           -> failwith "Not KS_COLON token!")


type lit =
  | Char of int
  | Str  of int array
  | Int  of int64
  | Flo  of float


type fix_later = unit

type typ = fix_later
type cls = fix_later

type context = cls list

type apat = fix_later

type 'infexp exp = 'infexp * (context option * typ) option

let exp_typ context typ = match context with
  | Some c -> TK.form_between c (Some (fst c), fst typ) typ
  | None   -> ((None, fst typ), snd typ)

let exp infexp = function
  | Some typ -> TK.form_between infexp (fst infexp, Some (fst typ)) typ
  | None     -> ((fst infexp, None), snd infexp)

type 'infexp qual  = fix_later
type 'infexp fbind = fix_later
type 'infexp alt   = fix_later
type 'infexp stmt  = fix_later
type 'infexp decl  = fix_later

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
  | UpdL   of id * 'infexp fbind list (* labeled update *)

(* 以下の実装のように引数を一つづつ部分適用するのは効率が良くないはず。
 * 一度に複数の引数を適用するように変更するかも。
 *)
type 'infexp fexp =
  | FApp of ('infexp fexp * 'infexp aexp)
  | AExp of 'infexp aexp

type 'infexp lexp =
  | Lambda of apat list * 'infexp exp
  | Let    of 'infexp decl list * 'infexp exp
  | If     of 'infexp exp * 'infexp exp * 'infexp exp
  | Case   of 'infexp exp * 'infexp alt list
  | Do     of 'infexp stmt list * 'infexp exp
  | FExp   of 'infexp fexp

type infexp =
  | OpApp of infexp lexp * id * infexp
  | Neg   of infexp
  | LExp  of infexp lexp

let comp2_region a b cons =
  TK.form_between a (cons (fst a) (fst b)) b

(* infexp construction *)
let op_app lexp id infexp = TK.form_between lexp (OpApp (fst lexp, fst id, fst infexp)) infexp
let neg = TK.with_region (fun infexp -> Neg infexp)
let lexp = TK.with_region (fun lexp -> LExp lexp)

(* aexp construction *)
let var = TK.with_region (fun id -> Var id)
let con = TK.with_region (fun id -> Con id)
let lit = TK.with_region (fun lit -> Lit lit)
let paren = TK.with_region (fun exp -> (Paren exp : infexp aexp))
let tuple = TK.with_region (fun el  -> (Tuple el : infexp aexp))
let list  = TK.with_region (fun el  -> (List el : infexp aexp))

(* let comp exp ql = TK.form_between exp (Comp (fst exp, fst ql)) ql *)
let comp exp ql = comp2_region exp ql (fun a b -> Comp (a, b))
let left_sec  infexp id = comp2_region infexp id (fun a b -> LeftS (a, b))
let right_sec id infexp = comp2_region id infexp (fun a b -> RightS (a, b))
let lbl_cons id bl = comp2_region id bl (fun a b -> ConsL (a, b))
let lbl_upd  id bl = comp2_region id bl (fun a b -> UpdL  (a, b))

(* fexp construction *)
let fexp_of_aexp_list
    : infexp aexp list * TK.region -> infexp fexp * TK.region =
  TK.with_region (function
    | []  -> failwith "Something wrong. fexp parser is broken?"
    | e :: es  -> L.fold_left (fun fexp e -> FApp (fexp, e)) (AExp e) es)

(* lexp construction *)
let lambda patl exp (* : infexp lexp * TK.region *) =
  comp2_region patl exp (fun a b -> Lambda (a, b))
let let_ decll exp (* : infexp lexp * TK.region *) =
  comp2_region decll exp (fun a b -> Let (a, b))
let if_ p t e (* : infexp lexp * TK.region *) =
  TK.form_between p (If (fst p, fst t, fst e)) e
let case exp altl (* : infexp lexp * TK.region *) =
  comp2_region exp altl (fun a b -> Case (a, b))
let do_ stmts exp = 
  comp2_region stmts exp (fun a b -> Do (a, b))
let fexp : infexp fexp * TK.region -> infexp lexp * TK.region =
  TK.with_region (fun fexp -> FExp fexp)
