(*  *)

module Z = Lazy

(* lazy list like ocaml batteries *)
type 'a cons_t =
  | Nil 
  | Cons of ('a * 'a t)
and 'a t = ('a cons_t) Lazy.t

let nil    = Z.lazy_from_val Nil

let mzero = nil

let null_p x = (x = nil)

let run : 'a t -> 'a cons_t = Z.force

let cons car cdr = Z.lazy_from_val (Cons(car, cdr))

let return car = cons car nil

let (^:) = cons

let next l = match run l with
  | Nil       -> None
  | Cons cons -> Some cons

let peek l = match run l with
  | Nil           -> None
  | Cons (car, _) -> Some car

let unfold (seed:'b) (gen: 'b -> ('a * 'b) option) =
  let rec step stat = match gen stat with
    | Some(car, n) -> Cons(car, lazy (step n))
    | None         -> Nil
  in lazy (step seed)

let rec (++) : 'a t -> 'a t -> 'a t = fun xs ys -> match run xs with
  | Nil          -> ys
  | Cons (x, xs) -> lazy (Cons (x, (xs ++ ys)))

let foldr
    : ('a -> 'b -> 'b)
    -> 'b -> 'a t -> 'b =
  fun k z xs ->
    let lzk = fun za zb -> lazy (k (Z.force za) (Z.force zb)) in
    let rec go = fun xs -> match run xs with
        | Nil          -> Z.lazy_from_val z
        | Cons (x, xs) -> lzk (Z.lazy_from_val x) (go xs)
    in Z.force (go xs)

let foldl'
    : ('a -> 'b -> 'a)
    -> 'a -> 'b t -> 'a =
  fun f z0 xs0 ->
    let rec lgo =
      fun z xs -> match run xs with
        | Nil -> z
        | Cons (x, xs) -> lgo (f z x) xs
    in lgo z0 xs0

let rec map : ('a -> 'b) -> 'a t -> 'b t = fun f l -> match run l with
    | Nil          -> nil
    | Cons (x, xs) -> lazy (Cons (f x, map f xs))

let rec iter : ('a -> unit) -> ('a t) -> unit =
  fun f l -> match run l with
    | Nil          -> ()
    | Cons (x, xs) -> let () = f x in iter f xs

let rec to_list lz =
  match next lz with
    | Some (car, cdr) -> car :: to_list cdr
    | None            -> []

let of_list l =
  unfold l (function
    | car :: cdr -> Some (car, cdr)
    | []         -> None)


type 'a node_t =
  | Node of ('a * 'a forest_t)
and  'a tree_t = ('a node_t) Lazy.t

and 'a forest_t = ('a tree_t) t

let empty = Z.lazy_from_val Nil

let empty_p x = (x = empty)

let t_run =  Z.force

let t_childs t = let Node n = t_run t in n

let t_peek t = let Node (e, _) = t_run t in e

let t_cons  e chld = Z.lazy_from_val (Node (e, chld))

(*
let rec f_nth l i =
  match l with
    | []                   -> None
    | _ :: cdr when i >  0 -> f_nth cdr (i-1)
    | car :: _ when i == 0 -> Some car
    | _                    -> None

let t_next t alti =
  let (e, cs) = t_childs t in
  (e, f_nth cs alti)

let t_next_0 t = t_next t 0
let t_next_1 t = t_next t 1
*)

let rec f_unfold : 'b -> ('b -> ('a * 'b) t) -> 'a forest_t =
  fun seed gen ->
    let rec step stat =
      map
        (fun (pare, stat) -> lazy (Node (pare, step stat)))
        (gen stat)
    in step seed
      

let t_unfold : 'b -> ('b -> ('a * 'b t)) -> 'a tree_t =
  fun seed gen ->
    let rec step stat =
      lazy (let (e, ss) = gen stat in
            Node (e, map (fun seed -> step seed) ss))
    in step seed

let tree_of_lzlist l =
  t_unfold l
    (fun l -> match next l with
      | None                 -> failwith "null list can't convert tree!"
      | Some (car, lazy Nil) -> (car, nil)
      | Some (car, cdr)      -> (car, cons cdr nil)
    )

let rec tmap f (lazy (Node (n, chld))) =
  lazy (Node (f n, (map (tmap f) chld)))

let fomap f fr = map (tmap f) fr

let rec tree_to_lzlist tr =
  unfold (return tr)
    (fun forest ->
      match peek forest with
        | None                 -> None
        | Some (lazy (Node n)) -> Some n)

let (show_token_tree, show_token_forest) =
  let make_show to_string =
    let rec show_tree il t =
      let indent =
        Format.sprintf "%2d>" il ^ String.make il ' '
      in
      let ((tk, _), chldl) = t_childs t in
      let () = print_endline (indent ^ (to_string tk)) in
      show_forest il chldl
    and show_forest il f =
      iter (show_tree (il + 2)) f
    in (show_tree 0, show_forest 0)
  in ((fun tos -> fst (make_show tos)),
      (fun tos -> snd (make_show tos)))

type 'a e_tree_t =
  | ENode of ('a * 'a e_tree_t list)

let rec tree_to_eager t =
  match t_childs t with
    | (e, chldl) -> ENode (e, to_list (map tree_to_eager chldl))

let rec tree_of_eager et =
  match et with
    | ENode (e, chldl) -> t_cons e (of_list (List.map tree_of_eager chldl))

(*
 * end of lazyList.ml
 *)
