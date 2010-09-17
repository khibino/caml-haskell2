
module Driver =
struct

  type 'tk tk_result =
    | Good of 'tk
    | Bad  of 'tk
    | Eos

  type 'tk tklist = 'tk LazyList.forest_t
  type ('tk, 'e) parser = 'tk tklist -> ('e * 'tk tklist) option
  type 'e result = 'e option

  let bind m f s = match m s with None -> None | Some (a, s) -> f a s

  let return e = fun tkl -> Some (e, tkl)

  let mzero = fun _ -> None

  let mplus a b s = match a s with
    | None -> b s
    | v    -> v

  let and_parser m s = match m s with
    | Some _ -> Some ((), s)
    | None   -> None

  let not_parser m s = match m s with
    | None   -> Some ((), s)
    | Some _ -> None

  let satisfy pred s =
    let sat_single br =
      let LazyList.Node ((i, _) as node) = Lazy.force br in
      if pred i then (Some node)
      else           (None     )
    in
    List.fold_left (fun res br -> match res with
      | None   -> sat_single br
      | Some _ -> res) None s

  let any s = satisfy (fun _ -> true) s

  let tokens tkg = [LazyList.t_unfold () (fun () -> (tkg (), [()]))]

  let run p tkl = match p tkl with None -> None | Some (e, _) -> Some e
end

module Combinator = Parser.Combinator2(Driver)
