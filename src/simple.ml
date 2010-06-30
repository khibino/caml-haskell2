
module Driver =
struct

  type 'tk tklist = 'tk LazyList.t
  type ('tk, 'e) parser = 'tk tklist -> ('e * 'tk tklist) option
  type 'e result = 'e option

  let bind m f s = match m s with None -> None | Some (a, s) -> f a s

  let return e = fun tkl -> Some (e, tkl)

  let mplus a b s = match a s with
    | None -> b s
    | v    -> v

  let satisfy f s = match Lazy.force s with
    | LazyList.Cons (i, tl) when f i -> Some (i, tl)
    | _ -> None

  let tokens tkg = LazyList.unfold () (fun () -> Some (tkg (), ()))

  let run p tkl = match p tkl with None -> None | Some (e, _) -> Some e
end

module Combinator = Parser.Combinator2(Driver)
