

module DebugInfo =
struct

  module ZL = LazyList

  type 'tk tk_match =
    | A of 'tk
    | R of 'tk

  type 'tk syntax_match =
    | A_ret
    | R_mzero

    | R1_bind of 'tk syntax_match
    | R2_bind of 'tk syntax_match * 'tk syntax_match
    | A_bind of 'tk syntax_match * 'tk syntax_match

    | A1_or of 'tk syntax_match
    | A2_or of 'tk syntax_match * 'tk syntax_match
    | R_or of 'tk syntax_match * 'tk syntax_match

    | A_and of 'tk syntax_match list
    | R_and of 'tk syntax_match list

    | A_not of 'tk syntax_match list
    | R_not of 'tk syntax_match list

    | TK_sat of 'tk tk_match

  type 'tk tklist = 'tk ZL.forest_t
  type 'tk monad = 'tk tklist -> ('tk tklist) option * 'tk syntax_match
  type 'tk result = bool * 'tk syntax_match

  let bind ma f tkl =
    let (va, xa) = ma tkl in match va with
      | None          -> (None, R1_bind xa)
      | Some (a, tkl) ->
        let (vb, xb) = f a tkl in match vb with
          | None   -> (vb, R2_bind (xa, xb))
          | Some _ -> (vb, A_bind (xa, xb))

  let return = fun tkl -> (Some (tkl), A_ret)

  let mzero = fun _ -> (None, R_mzero)

  let mplus ma mb tkl =
    let (va, xa) = ma tkl in match va with
      | Some _ -> (va, A1_or xa)
      | None   ->
        let (vb, xb) = mb tkl in match vb with
          | Some _ -> (vb, A2_or (xa, xb))
          | None   -> (vb, R_or (xa, xb))

  let and_parser ma tkl =
    let (va, xa) = ma tkl in match va with
      | Some _ -> (Some ((), tkl), A_and xa)
      | None   -> (None, R_and xa)

  let not_parser ma tkl =
    let (va, xa) = ma tkl in match va with
      | None   -> (Some ((), tkl), A_not xa)
      | Some _ -> (None, R_not xa)

  let sat_single =
    (fun pred br ->
      let lazy (ZL.Node (i, _)) = br in
      if pred i then A i
      else           R i)

  let satisfy name pred tkl =
    TK_sat (sat_single pred (List.hd tkl))

end

module Driver =
struct

  module ZL = LazyList

  type 'tk tree_t = 'tk ZL.tree_t
  type 'tk forest_t = 'tk tree_t ZL.t

  type 'tk tklist = 'tk forest_t
  type ('tk, 'e) parser = 'tk tklist -> ('e * 'tk tklist) option
  type ('tk, 'e) result = ('e * 'tk tklist) option

  let (++) = ZL.(++)

(*
  let with_apply
      : (('e0 * 'tk tklist) option -> ('tk, 'e1) result -> ('tk, 'e1) result)
      -> ('tk, 'e0) parser -> 'tk tklist -> ('tk, 'e1) result =
    fun f ma tkl ->
      ZL.foldr f ZL.mzero (ma tkl)
*)

  let bind
      : ('tk, 'e0) parser -> ('e0 -> ('tk, 'e1) parser) ->
        ('tk, 'e1) parser =
    fun ma f tfo ->
      match ma tfo with
        | None -> None
        | Some (a, tfo) -> f a tfo

(*
      with_apply
        (fun a rl ->
          (match a with
            | None -> ZL.nil
            | Some (a, tkl) -> f a tkl) ++ rl)
        ma tkl
*)

  let (>>=) = bind

  let return : 'e -> ('tk, 'e) parser =
    fun e tfo -> Some (e, tfo)

  let mzero : ('tk, 'e) parser = fun _ -> None

  let mplus
      : ('tk, 'e) parser -> ('tk, 'e) parser ->
        ('tk, 'e) parser =
    fun ma mb tfo ->
      match ma tfo with
        | None -> mb tfo
        | v    -> v

  let and_parser : ('tk, 'e) parser -> ('tk, unit) parser =
    fun ma tfo ->
      match ma tfo with
        | None   -> None
        | Some _ -> Some ((), tfo)

  let not_parser : ('tk, 'e) parser -> ('tk, unit) parser =
    fun ma tfo ->
      match ma tfo with
        | None   -> Some ((), tfo)
        | Some _ -> None

  let any : ('tk, 'tk) parser =
    fun tfo ->
      match ZL.peek tfo with
        | None                         -> None
        | Some (lazy (ZL.Node (node))) -> Some node

  let satisfy : string -> ('tk -> bool) -> ('tk, 'tk) parser =
    fun name pred tfo ->
      match ZL.peek tfo with
        | Some (lazy (ZL.Node (tk, _ as node))) when pred tk
            -> Some node
        | None | Some _ -> None

  let tokens tkg =
    ZL.return (ZL.t_unfold
                 ()
                 (fun () -> (tkg (), ZL.return ())))

  let run p tkl = p tkl

end

module Combinator = Parser.Combinator2(Driver)
