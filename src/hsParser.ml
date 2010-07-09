
open Simple.Combinator

module TK = Token

let call = call_parser

let ( |.| ) f g x = f (g x)

let sat_tk : (TK.typ -> bool) -> (TK.t, TK.t) parser =
  fun f -> satisfy (f |.| fst)
let just_tk eq = sat_tk ((=) eq)

let qvarid = sat_tk (function | TK.T_VARID _ -> true | TK.T_MOD_VARID _ -> true | _ -> false)

let any    = sat_tk (fun _ -> true)

let test_s0 = any *> qvarid
