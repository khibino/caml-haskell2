
module ZL = LazyList

type  op =
  | Pl
  | Mi
  | Ti

type token =
  | BinOp of op
  | LParen
  | RParen
  | Num of int
  | Not
  | Eos

type uni_expr =
  | Prim   of int
  | Paren  of expr
  | UMinus of uni_expr

and  func_expr =
  | RSec of (op * uni_expr)
  | LSec of (uni_expr * op)

and  app_expr =
  | FApp of (func_expr * uni_expr)
  | Uni of uni_expr

and  mul_expr =
  | Times of (app_expr * app_expr)
  | App of app_expr

and  expr =
  | Plus  of (mul_expr * mul_expr)
  | Minus of (mul_expr * mul_expr)
  | Mul   of mul_expr

let prim i = Prim i
let paren e = Paren e
let uminus p = UMinus p

let r_sec op u = RSec (op, u)
let l_sec u op = LSec (u, op)

let fapp f u =  FApp (f, u)
let uni u = Uni u

let times a0 a1 = Times (a0, a1)
let app a = App a

let plus  m0 m1 = Plus (m0, m1)
let minus m0 m1 = Minus (m0, m1)
let mul m = Mul m

module CI =
struct
  type type_ = token
  type region = unit

  let cover_region = fun () () -> ()

  let type_to_string =
    function
      | BinOp _ -> "bin_op"
      | LParen -> "("
      | RParen -> ")"
      | Num i  -> Printf.sprintf "%d : num" i
      | Not -> "not"
      | Eos -> "<eos>"
end

module C = ParserDriver.Combinator(CI)
open C

type derive_t = {
  token     : (deriv, token exp) result;
  uni_expr  : (deriv, uni_expr exp) result;
  func_expr : (deriv, func_expr exp) result;
  app_expr  : (deriv, app_expr exp) result;
  mul_expr  : (deriv, mul_expr exp) result;
  expr      : (deriv, expr exp) result;
  top       : (deriv, expr exp) result;
}

and  deriv = derive_t * token exp seq

let deriv t s = (t, s)
let d_deriv  (t, _) = t
let d_source (_, s) = s

module D = struct
  let token : deriv -> (deriv, token exp) result =
    fun d -> (d_deriv d).token
  let uni_expr : deriv -> (deriv, uni_expr exp) result =
    fun d -> (d_deriv d).uni_expr
  let func_expr : deriv -> (deriv, func_expr exp) result =
    fun d -> (d_deriv d).func_expr
  let app_expr : deriv -> (deriv, app_expr exp) result =
    fun d -> (d_deriv d).app_expr
  let mul_expr : deriv -> (deriv, mul_expr exp) result =
    fun d -> (d_deriv d).mul_expr
  let expr : deriv -> (deriv, expr exp) result =
    fun d -> (d_deriv d).expr
  let top : deriv -> (deriv, expr exp) result =
    fun d -> (d_deriv d).top
end

module Z = Lazy

let any = D.token

let just : token -> ('deriv, token exp) parser =
  fun tk d -> to_just any tk d

let untag : (token -> 'e option)-> ('deriv, 'e exp) parser =
  fun tk d -> to_untag any tk d

let l_paren = just LParen
let r_paren = just RParen
let opt_not = ~?(just Not)

let num = untag (function
  | Num i -> Some i
  | _     -> None)

let plus_op = just (BinOp Pl)
let minus_op = just (BinOp Mi)
let times_op = just (BinOp Ti)

let bin_op = untag (function
  | BinOp op -> Some op
  | _        -> None)

(* let parened p = l_paren *> p <* r_paren *)

let     app_expr () =
  (fapp <$> D.func_expr <*> D.uni_expr)
  <|> (uni <$> D.uni_expr)

let     mul_expr () =
  (times <$> D.app_expr <*> (times_op *> D.app_expr))
  <|> (app <$> D.app_expr)

let     expr () = 
  (plus <$> D.mul_expr <*> plus_op *> D.mul_expr)
  <|> (minus <$> D.mul_expr <*>  minus_op *> D.mul_expr)
    <|> (mul <$> D.mul_expr)

let     top () =
  D.expr <* just Eos

let rec r_paren' l = may_shift r_paren l

and     uni_expr ()  =
  (prim *<$> num
   <|> paren *<$> l_paren **> D.expr **< r_paren'
     <|> (minus_op *> (uminus <$> D.uni_expr))) **< opt_not

and     func_expr () =
  l_paren **> (
    (~! minus_op *> (r_sec <$> bin_op <*> D.uni_expr))
    <|> (l_sec <$> D.uni_expr <*> bin_op)
  ) **< r_paren'

and     make_derive tfo =
  let call p d = Z.force (run (p ()) (d ())) in

  let rec d () =
    deriv
      { token     = derive_seq make_derive tfo;
        uni_expr  = _uni ();
        func_expr = _func ();
        app_expr  = _app ();
        mul_expr  = _mul ();
        expr      = _expr ();
        top       = _top ();
      }
      tfo

  and _uni  () = lazy (call uni_expr d)
  and _func () = lazy (call func_expr d)
  and _app  () = lazy (call app_expr d)
  and _mul  () = lazy (call mul_expr d)
  and _expr () = lazy (call expr d)
  and _top  () = lazy (call top d)

  in d ()

and     may_shift d = match_or_shift make_derive d_source d

let num_pair =
  Data.tuple2 *<$> num *<*> num

let num_pair2 =
  Data.tuple2 *<$> num **< ~&num *<*> num

let call_parser parser_ tokens =
  run parser_
    (make_derive (ZL.return (ZL.tree_of_lzlist
                               (ZL.of_list
                                  (List.map (fun x -> (x, None))
                                     tokens)))))

let parse tokens = Z.force (call_parser (top ()) tokens)

let call_parser' parser_ tree =
  run parser_
    (make_derive (ZL.return (ZL.tmap (fun x -> (x, None)) (ZL.tree_of_eager tree))))

let parse' tree = Z.force (call_parser' (top ()) tree)

let s0 = [LParen; BinOp Pl; Num 3 ; RParen; Num 4; Eos]
let e0 = [LParen; BinOp Pl; Num 3 ; RParen; Eos]

let s1 = [LParen; BinOp Mi; Num 3 ; RParen; Eos]
let e1 = [LParen; BinOp Mi; Num 3 ; RParen; Num 4; Eos]

let s2 = [LParen; BinOp Mi; Num 3 ; RParen; Not ; BinOp Pl ; Num 1; Eos]

open ZL

let ts0 = ENode (LParen, [ENode (BinOp Pl, [ENode (Num 3,
                                                   [ENode (RParen, [ENode (Num 4, [ENode (Eos, [])])])])])])

let ts1 = ENode (LParen, [ENode (BinOp Pl, [ENode (Num 3,
                                                   [ENode (Eos, []);
                                                    ENode (RParen, [ENode (Num 4, [ENode (Eos, [])])])])])])
