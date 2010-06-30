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
