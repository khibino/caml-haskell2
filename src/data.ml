
let tuple2 = fun a b -> (a, b)

let with_snd : ('a -> 'b) -> ('a * 'c) -> ('b * 'c) = fun f (a, b) -> (f a, b)
let with_fst : ('a -> 'b) -> ('c * 'a) -> ('c * 'b) = fun f (a, b) -> (a, f b)

module L = List

let rec last = function
  | []        -> None
  | [_] as v  -> Some v
  | _ :: rest -> last rest

let init l = match last l with
  | None   -> failwith "init"
  | Some v -> v
