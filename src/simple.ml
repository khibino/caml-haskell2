
module type TOKEN =
sig

end

module Driver (* (Tk : ) *)  =
struct

  module ZL = LazyList

  type 'tk tree_t = 'tk ZL.tree_t
  type 'tk forest_t = 'tk tree_t ZL.t

  type 'tk tklist = 'tk forest_t
  type ('tk, 'e) parsed = ('e * 'tk tklist) option
  type ('tk, 'e) parser = 'tk tklist -> ('tk, 'e) parsed
  type ('tk, 'e) result = ('tk, 'e) parsed

  let bind
      : ('tk, 'e0) parser -> ('e0 -> ('tk, 'e1) parser) ->
        ('tk, 'e1) parser =
    fun ma f tfo ->
      match ma tfo with
        | None -> None
        | Some (a, tfo) -> f a tfo

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
        | Some _ | None -> None

  (* マッチしなかったときに次の枝をマッチする parser に変換 *)
  let match_or_shift : ('tk, 'tk) parser -> ('tk, 'tk) parser =
    fun ma tfo ->
      match ma tfo with
        | None ->
          (match ZL.next tfo with
            | Some (_, tfo) -> ma tfo
            | None           -> None)
        | v    -> v

  let tokens : (unit -> 'a) -> ('a) tklist =
    fun tkg ->
      ZL.return (ZL.t_unfold
                   ()
                   (fun () -> (tkg (), ZL.return ())))

  let run : ('tk, 'e) parser -> ('tk) tklist -> ('tk, 'e) result =
    fun p tkl -> p tkl

end

module Combinator = Parser.Combinator2(Driver)

(*
module DriverWithRegion =
struct

  module ZL = LazyList

  type region = Token.region
  type 'tk tree_t = ('tk * region) ZL.tree_t
  type 'tk forest_t = 'tk tree_t ZL.t

  type 'tk tklist = 'tk forest_t
  type ('tk, 'e) parsed = (('e * region) * 'tk tklist) option
  type ('tk, 'e) parser = 'tk tklist -> ('tk, 'e) parsed
  type ('tk, 'e) result = ('tk, 'e) parsed

  let cover_region = Token.cover_region

  let bind
      : ('tk, 'e0) parser -> ('e0 -> ('tk, 'e1) parser) ->
        ('tk, 'e1) parser =
    fun ma f tfo ->
      match ma tfo with
        | None -> None
        | Some ((a, ra), tfo) ->
          let (b, rb) = f a tfo in
          (b, cover_region ra rb)

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
        | Some _ | None -> None

  (* マッチしなかったときに次の枝をマッチする parser に変換 *)
  let match_or_shift : ('tk, 'tk) parser -> ('tk, 'tk) parser =
    fun ma tfo ->
      match ma tfo with
        | None ->
          (match ZL.next tfo with
            | Some (_, tfo) -> ma tfo
            | None           -> None)
        | v    -> v

  let tokens : (unit -> 'a) -> ('a) tklist =
    fun tkg ->
      ZL.return (ZL.t_unfold
                   ()
                   (fun () -> (tkg (), ZL.return ())))

  let run : ('tk, 'e) parser -> ('tk) tklist -> ('tk, 'e) result =
    fun p tkl -> p tkl

end
*)

module DebugInfo =
struct

  module D = Driver

  module ZL = LazyList

  type 'tk tk_match =
    | A of 'tk
    | R of 'tk
    | Z

  type 'tk syntax_match =
    | A_ret
    | R_mzero

    | R1_bind of 'tk syntax_match
    | R2_bind of 'tk syntax_match * 'tk syntax_match
    | A_bind of 'tk syntax_match * 'tk syntax_match

    | A1_or of 'tk syntax_match
    | A2_or of 'tk syntax_match * 'tk syntax_match
    | R_or of 'tk syntax_match * 'tk syntax_match

    | A_and of 'tk syntax_match
    | R_and of 'tk syntax_match

    | A_not of 'tk syntax_match
    | R_not of 'tk syntax_match

    | TK_sat of 'tk tk_match
    | TK_any of 'tk tk_match

  type 'tk tklist = 'tk ZL.forest_t
  type ('tk, 'e) parser = 'tk tklist -> ('tk, 'e) D.parsed * 'tk syntax_match
  type ('tk, 'e) result = ('tk, 'e) D.result * 'tk syntax_match

  let bind 
      : ('tk, 'e0) parser -> ('e0 -> ('tk, 'e1) parser) ->
        ('tk, 'e1) parser =
    fun ma f tkl ->
      let (va, xa) = ma tkl in match va with
        | None          -> (None, R1_bind xa)
        | Some (a, tkl) ->
          let (vb, xb) = f a tkl in match vb with
            | None   -> (vb, R2_bind (xa, xb))
            | Some _ -> (vb, A_bind (xa, xb))

  let return  : 'e -> ('tk, 'e) parser = fun e tfo -> (Some (e, tfo), A_ret)

  let mzero : ('tk, 'e) parser = fun _ -> (None, R_mzero)

  let mplus
      : ('tk, 'e) parser -> ('tk, 'e) parser ->
        ('tk, 'e) parser =
    fun ma mb tkl ->
      let (va, xa) = ma tkl in match va with
        | Some _ -> (va, A1_or xa)
        | None   ->
          let (vb, xb) = mb tkl in match vb with
            | Some _ -> (vb, A2_or (xa, xb))
            | None   -> (vb, R_or (xa, xb))

  let and_parser : ('tk, 'e) parser -> ('tk, unit) parser =
    fun ma tkl ->
      let (va, xa) = ma tkl in match va with
        | Some _ -> (Some ((), tkl), A_and xa)
        | None   -> (None, R_and xa)

  let not_parser : ('tk, 'e) parser -> ('tk, unit) parser =
    fun ma tkl ->
      let (va, xa) = ma tkl in match va with
        | None   -> (Some ((), tkl), A_not xa)
        | Some _ -> (None, R_not xa)

  let sat_single =
    (fun pred br ->
      let lazy (ZL.Node (i, _)) = br in
      if pred i then A i
      else           R i)

  let satisfy : string -> ('tk -> bool) -> ('tk, 'tk) parser =
    fun name pred tfo ->
      let (p, m) = (match ZL.peek tfo with
        | Some (lazy (ZL.Node (tk, _ as node))) ->
            if pred tk then (Some node, A tk)
            else (None, R tk)
        | None -> (None, Z))
      in (p, TK_sat m)

  let match_or_shift : ('tk, 'tk) parser -> ('tk, 'tk) parser =
    fun ma tfo ->
      let (va, xa) = ma tfo in match va with
        | None ->
          (match ZL.next tfo with
            | Some (_, tfo) -> ma tfo
            | None          -> (None, xa))
        | v    -> (v, xa)

  let any : ('tk, 'tk) parser =
    fun tfo ->
      match ZL.peek tfo with
        | None                         -> (None, TK_any Z)
        | Some (lazy (ZL.Node ((tk, _) as node))) -> (Some node, TK_any (A tk))


  let tokens : (unit -> 'a) -> ('a) tklist = D.tokens
  let run : ('tk, 'e) parser -> ('tk) tklist -> ('tk, 'e) result =
    fun p tkl -> p tkl

end

module DebugCombinator = Parser.Combinator2(DebugInfo)
