(*  *)


(* lazy list like ocaml batteries *)
type 'a cons_t =
  | Nil 
  | Cons of ('a * 'a t)
and 'a t = ('a cons_t) Lazy.t

let nil    = Lazy.lazy_from_val Nil

let null_p x = (x = nil)

let run = Lazy.force

let cons car cdr = Lazy.lazy_from_val (Cons(car, cdr))

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

let rec to_list l =
  match next l with
    | Some (car, cdr) -> car :: to_list cdr
    | None            -> []


type 'a node_t =
  | Node of ('a * 'a tree_t list)
and  'a tree_t = ('a node_t) Lazy.t

type 'a forest_t = 'a tree_t list

let empty = []

let empty_p x = (x = empty)

let t_run =  Lazy.force

let t_childs t = match t_run t with
  | Node n -> n

let t_peek t = match t_run t with
  | Node (e, _) -> e

let rec f_nth l i =
  match l with
    | []                   -> None
    | _ :: cdr when i >  0 -> f_nth cdr (i-1)
    | car :: _ when i == 0 -> Some car
    | _                    -> None

let t_cons  e chld = Lazy.lazy_from_val (Node (e, chld))

let t_next t alti =
  let (e, cs) = t_childs t in
  (e, f_nth cs alti)

let t_next_0 t = t_next t 0
let t_next_1 t = t_next t 1


let f_unfold (seed:'b) (gen: 'b -> ('a * 'b) list) =
  let rec step stat = 
    List.map (fun (pare, seed) -> lazy (Node (pare, step seed))) (gen stat)
  in step seed

let t_unfold (seed:'b) (gen: 'b -> ('a * 'b list)) =
  let rec step stat =
    let (e, ss) = gen stat in
    lazy (Node (e, List.map (fun seed -> step seed) ss))
  in step seed

let tree_of_lzlist l =
  t_unfold l
    (fun l -> match next l with
      | None                 -> failwith "null list can't convert tree!"
      | Some (car, lazy Nil) -> (car, [])
      | Some (car, cdr)      -> (car, [cdr])
    )

(* let show_tree t *)

type 'a e_tree_t =
  | ENode of ('a * 'a e_tree_t list)

let rec tree_to_eager t =
  match t_childs t with
    | (e, chldl) -> ENode (e, List.map tree_to_eager chldl)
