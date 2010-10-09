
(* let id = fun x -> x *)

let undefined () = failwith "Undefined."
let zundefined = lazy (undefined ())

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
type 'a l1_list = ('a * 'a list)

let l1_list_cons : 'a -> 'a list -> 'a l1_list =
  tuple2

let l1_cons     : 'a -> 'a l1_list -> 'a l1_list =
  fun hd (ohd, tl) -> (hd, ohd :: tl)
let l1_cons_nil : 'a -> 'a l1_list =
  fun hd -> (hd, [])
let l1_list : 'a l1_list -> 'a list =
  fun (hd, tl) -> hd :: tl

let l1_rev : 'a l1_list -> 'a l1_list = fun l1 -> match L.rev (l1_list l1) with
  | hd :: tl -> (hd, tl)
  | []       -> failwith "Something wrong? l1_rev failed!"

let apply a b = a b

let rec take n xs =
  match (n, xs) with
    | (n, _) when n <= 0 -> []
    | (n, [])            -> []
    | (n, x :: xs')      -> x :: take (n - 1) xs'

let rec drop n xs =
  match (n, xs) with
    | (n, _)  when n <= 0 -> xs
    | (n, [])             -> []
    | (n, _ :: xs')       -> drop (n - 1) xs'

let list_compare xs ys =
  let rec list_compare xs ys n = match (xs, ys) with
    | ([], []) -> (true, "")
    | (x :: xs', []) -> (false, "second is shorter.")
    | ([], y :: ys') -> (false, "first is shorter.")
    | (x :: xs', y :: ys') ->
      if x <> y then (false, Format.sprintf "differ at %d" n)
      else list_compare xs' ys' (n + 1)
  in list_compare xs ys 0

