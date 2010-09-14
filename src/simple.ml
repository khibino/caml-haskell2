
module Driver =
struct

  type 'tk tklist = 'tk LazyList.forest_t
  type ('tk, 'e) parser = 'tk tklist -> ('e * 'tk tklist) option
  type 'e result = 'e option

  let bind m f s = match m s with None -> None | Some (a, s) -> f a s

  let return e = fun tkl -> Some (e, tkl)

  let fail = fun _ -> None

  let mplus a b s = match a s with
    | None -> b s
    | v    -> v

  let and_parser m s = match m s with
    | Some _ -> Some ((), s)
    | None   -> None

  let not_parser m s = match m s with
    | None   -> Some ((), s)
    | Some _ -> None

  let satisfy f s =
    let sat_single s =
      match Lazy.force s with
        | LazyList.Node (i, tl) when f i -> Some (i, tl)
        | _                              -> None
    in
    match s with
      | []              -> None
      | fst :: []       -> sat_single fst
      | fst :: snd :: _ ->
        (match sat_single fst with
          | (Some _ as v) -> v
          | None          -> sat_single snd
        )

  let tokens tkg = [LazyList.t_unfold () (fun () -> (tkg (), [()]))]

  let run p tkl = match p tkl with None -> None | Some (e, _) -> Some e
end

module Combinator = Parser.Combinator2(Driver)
