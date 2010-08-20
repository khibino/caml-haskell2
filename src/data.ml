
(* pair functions *)
let tuple2 = fun a b -> (a, b)

let with_snd : ('a -> 'b) -> ('a * 'c) -> ('b * 'c) = fun f (a, b) -> (f a, b)
let with_fst : ('a -> 'b) -> ('c * 'a) -> ('c * 'b) = fun f (a, b) -> (a, f b)

(* option functions *)
let with_option : ('a -> 'b) -> 'a option -> 'b option = fun f -> function
  | Some x -> Some (f x)
  | None   -> None

module L = List

let rec last = function
  | []        -> None
  | [_] as v  -> Some v
  | _ :: rest -> last rest

let last' l = match last l with
  | None   -> failwith "init"
  | Some v -> v


(* リスト - 長さ0でも可 *)
let cons = (fun a d -> a :: d)
let cons_nil x = [x]

(* 長さ1以上のリスト 意図的に0以上のリストと型が異なるようにしている *)
let l1_cons hd (ohd, tl) = (hd, ohd :: tl)
let l1_cons_nil hd = (hd, [])
let l1_list (hd, tl) = hd :: tl

let l1_rev l1 = match L.rev (l1_list l1) with
  | hd :: tl -> (hd, tl)
  | []       -> failwith "Something wrong? l1_rev failed!"
