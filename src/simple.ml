module type TOKEN =
sig
  type t

  val to_string : t -> string
end

module type DRIVER =
sig
  (* type 'e parsed *)
  include Parser.EAGER_BASIC_OP2
end

module ZL = LazyList

module Types =
struct
  type 'tk tklist = 'tk ZL.forest_t
  type ('tk, 'e) result = ('e * 'tk tklist) option
  type ('tk, 'e) parser = 'tk tklist -> ('tk, 'e) result
end

module Driver(Tk : TOKEN) : DRIVER
  with type token = Tk.t
  and  type 'tk tklist = 'tk Types.tklist
  and  type 'e result = (Tk.t, 'e) Types.result
  and  type 'e parser = (Tk.t, 'e) Types.parser
         =
struct

  type token = Tk.t

  type 'tk tklist = 'tk Types.tklist
  type 'e parser = (token, 'e) Types.parser
  type 'e result = (token, 'e) Types.result

  let bind
      : 'e0 parser -> ('e0 -> 'e1 parser) ->
        'e1 parser =
    fun ma f tfo ->
      match ma tfo with
        | None -> None
        | Some (a, tfo) -> f a tfo

  let (>>=) = bind

  let return : 'e -> 'e parser =
    fun e tfo -> Some (e, tfo)

  let mzero : 'e parser = fun _ -> None

  let mplus
      : 'e parser -> 'e parser ->
        'e parser =
    fun ma mb tfo ->
      match ma tfo with
        | None -> mb tfo
        | v    -> v

  let and_parser : 'e parser -> unit parser =
    fun ma tfo ->
      match ma tfo with
        | None   -> None
        | Some _ -> Some ((), tfo)

  let not_parser : 'e parser -> unit parser =
    fun ma tfo ->
      match ma tfo with
        | None   -> Some ((), tfo)
        | Some _ -> None

  let any : token parser =
    fun tfo ->
      match ZL.peek tfo with
        | None                         -> None
        | Some (lazy (ZL.Node (node))) -> Some node

  let satisfy : string -> ('tk -> bool) -> token parser =
    fun name pred tfo ->
      match ZL.peek tfo with
        | Some (lazy (ZL.Node (tk, _ as node))) when pred tk
            -> Some node
        | Some _ | None -> None


  module F = Printf
  (* マッチしなかったときに次の枝をマッチする parser に変換 *)
  let match_or_shift : token parser -> token parser =
    fun ma tfo ->
      match ma tfo with
        | None ->
          (match ZL.next tfo with
            | Some (_, tfo) ->
              if true then
                (match tfo with
                  | lazy (ZL.Cons (lazy (ZL.Node (_, next)), _)) ->
                    F.printf "shift invocation: next: %s\n"
                      (ZL.foldl'
                         (fun res tr -> res ^ " " ^ Tk.to_string (ZL.t_peek tr))
                         "" next)
                  | _ -> ());
              ma tfo
            | None           -> None)
        | v    -> v

  let tokens : (unit -> token) -> token tklist =
    fun tkg ->
      ZL.return (ZL.t_unfold
                   ()
                   (fun () -> (tkg (), ZL.return ())))

  let run : 'e parser -> 'tk tklist -> 'e result =
    fun p tkl -> p tkl

end

module Combinator(Tk : TOKEN) = Parser.Combinator2(Driver(Tk))

(*
module DebugInfo(Tk : TOKEN) : DRIVER
  with type token = Tk.t
  and  type 'tk tklist = 'tk Types.tklist
         =
struct

  module D = Driver(Tk)

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

  type token = Tk.t
  type 'tk tklist = 'tk Types.tklist
  type 'e result = (token, 'e) Types.result * token syntax_match
  type 'e parser = token tklist -> 'e result

  let bind 
      : 'e0 parser -> ('e0 -> 'e1 parser) ->
        'e1 parser =
    fun ma f tkl ->
      let (va, xa) = ma tkl in match va with
        | None          -> (None, R1_bind xa)
        | Some (a, tkl) ->
          let (vb, xb) = f a tkl in match vb with
            | None   -> (vb, R2_bind (xa, xb))
            | Some _ -> (vb, A_bind (xa, xb))

  let return  : 'e -> 'e parser = fun e tfo -> (Some (e, tfo), A_ret)

  let mzero : 'e parser = fun _ -> (None, R_mzero)

  let mplus
      : 'e parser -> 'e parser ->
        'e parser =
    fun ma mb tkl ->
      let (va, xa) = ma tkl in match va with
        | Some _ -> (va, A1_or xa)
        | None   ->
          let (vb, xb) = mb tkl in match vb with
            | Some _ -> (vb, A2_or (xa, xb))
            | None   -> (vb, R_or (xa, xb))

  let and_parser : 'e parser -> unit parser =
    fun ma tkl ->
      let (va, xa) = ma tkl in match va with
        | Some _ -> (Some ((), tkl), A_and xa)
        | None   -> (None, R_and xa)

  let not_parser : 'e parser -> unit parser =
    fun ma tkl ->
      let (va, xa) = ma tkl in match va with
        | None   -> (Some ((), tkl), A_not xa)
        | Some _ -> (None, R_not xa)

  let sat_single =
    (fun pred br ->
      let lazy (ZL.Node (i, _)) = br in
      if pred i then A i
      else           R i)

  let satisfy : string -> ('tk -> bool) -> token parser =
    fun name pred tfo ->
      let (p, m) = (match ZL.peek tfo with
        | Some (lazy (ZL.Node (tk, _ as node))) ->
            if pred tk then (Some node, A tk)
            else (None, R tk)
        | None -> (None, Z))
      in (p, TK_sat m)

  let match_or_shift : token parser -> token parser =
    fun ma tfo ->
      let (va, xa) = ma tfo in match va with
        | None ->
          (match ZL.next tfo with
            | Some (_, tfo) -> ma tfo
            | None          -> (None, xa))
        | v    -> (v, xa)

  let any : token parser =
    fun tfo ->
      match ZL.peek tfo with
        | None                         -> (None, TK_any Z)
        | Some (lazy (ZL.Node ((tk, _) as node))) -> (Some node, TK_any (A tk))


  let tokens : (unit -> token) -> token tklist = (* D.tokens *)
    fun tkg ->
      ZL.return (ZL.t_unfold
                   ()
                   (fun () -> (tkg (), ZL.return ())))

  let run : 'e parser -> token tklist -> 'e result =
    fun p tkl -> p tkl

end

module DebugCombinator(Tk : TOKEN) = Parser.Combinator2(DebugInfo(Tk))
*)
