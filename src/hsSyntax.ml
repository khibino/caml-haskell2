
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


type 'exp aexp =
  | Var of id
  | Con of id
  | Lit of lit
  | Paren of 'exp
  | Tuple of 'exp list
  | List of 'exp list
  (* | Comp (exp, qual list) (* list comprehension *) *)
  (* |  (* left section*) *)
  (* |  (* right section*) *)
  (* | (* labeled construction *) *)
  (* | (* labeled update *) *)

let var = TK.with_region (fun id -> Var id)
let con = TK.with_region (fun id -> Con id)
let lit = TK.with_region (fun lit -> Lit lit)

(* 以下の実装のように引数を一つづつ部分適用するのは効率が良くないはず。
 * 一度に複数の引数を適用するように変更するかも。
 *)
type 'exp fexp =
  | FApp of ('exp fexp * 'exp aexp)
  | AExp of 'exp aexp

let fexp_of_aexp_list = function
  | []  -> failwith "Something wrong. fexp parser is broken?"
  | e :: es  -> L.fold_left (fun fexp e -> FApp (fexp, e)) (AExp e) es

(* type 'exp lexp *)
