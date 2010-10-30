
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

  let type_to_string _ = ""
end

module C = ParserDriver.Combinator(CI)
open C

let l_paren = just "l_paren" (LParen : C.token)
let r_paren = just "r_paren" RParen
let opt_not = ~?(just "not" Not)

let parened p = l_paren *> p <* r_paren

let num = untag "num" (function
  | Num i -> Some i
  | _     -> None)

let plus_op = just "plus" (BinOp Pl)
let minus_op = just "minus" (BinOp Mi)
let times_op = just "times" (BinOp Ti)

let bin_op = untag "bin_op" (function
  | BinOp op -> Some op
  | _        -> None)

let rec uni_expr ()  =
  ((prim <$> num)
   <|> parened (paren <$> ~$ expr)
     <|> (minus_op *> (uminus <$> ~$ uni_expr))) **< opt_not

and     func_expr () =
  parened (
    (~! minus_op *> (r_sec <$> bin_op <*> ~$ uni_expr))
    <|> (l_sec <$> ~$ uni_expr <*> bin_op)
  )

and     app_expr () =
  (fapp <$> ~$ func_expr <*> ~$ uni_expr)
  <|> (uni <$> ~$ uni_expr)

and     mul_expr () =
  (times <$> ~$ app_expr <*> (times_op *> ~$ app_expr))
  <|> (app <$> ~$ app_expr)

and     expr () = 
  (plus <$> ~$ mul_expr <*> plus_op *> ~$ mul_expr)
  <|> (minus <$> ~$ mul_expr <*>  minus_op *> ~$ mul_expr)
    <|> (mul <$> ~$ mul_expr)

and     top () =
  ~$ expr <* just "eos" Eos

let num_pair =
  Data.tuple2 *<$> num *<*> num

let num_pair2 =
  Data.tuple2 *<$> num **< ~&num *<*> num

let call_parser parser_ tokens =
  run parser_
    (ZL.return (ZL.tree_of_lzlist
                  (ZL.of_list tokens)))

let parse tokens = call_parser (top ()) tokens

let s0 = [LParen; BinOp Pl; Num 3 ; RParen; Num 4; Eos]
let e0 = [LParen; BinOp Pl; Num 3 ; RParen; Eos]

let s1 = [LParen; BinOp Mi; Num 3 ; RParen; Eos]
let e1 = [LParen; BinOp Mi; Num 3 ; RParen; Num 4; Eos]

let s2 = [LParen; BinOp Mi; Num 3 ; RParen; Not ; BinOp Pl ; Num 1; Eos]
