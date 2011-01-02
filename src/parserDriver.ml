module type TOKEN =
sig
  (* type t *)
  type type_
  type region

  val cover_region : region -> region -> region

  (* val type_ : t -> type_ *)
  (* val region : t -> region *)

  (* val to_string : t -> string *)
  val type_to_string : type_ -> string
end

module type DRIVER =
sig
  (* type 'e parsed *)
  include Parser.EAGER_BASIC_OP2
end

module ZL = LazyList

module T =
struct
  type ('e, 'reg) exp = 'e * 'reg option
  type 'exp seq = 'exp ZL.forest_t
  type ('deriv, 'exp) result = ('exp * 'deriv) option
  type ('deriv, 'exp) parser = 'deriv -> ('deriv, 'exp) result
  type ('deriv, 'exp) raw_parser = 'exp seq -> ('deriv, 'exp) result
end

module DriverWithRegion(Tk : TOKEN) : DRIVER
  with type token = Tk.type_ 
  (* and  type 'tk tklist = ('tk, Tk.region) T.tklist *)
  and  type 'e exp = ('e, Tk.region) T.exp
  and  type 'exp seq = 'exp T.seq
  and  type ('deriv, 'exp) result = ('deriv, 'exp) T.result
  and  type ('deriv, 'exp) parser = ('deriv, 'exp) T.parser
  and  type ('deriv, 'exp) raw_parser = ('deriv, 'exp) T.raw_parser
                 =
struct

  type token = Tk.type_

  type 'e exp = ('e, Tk.region) T.exp
  type 'exp seq = 'exp T.seq
  type ('deriv, 'exp) result = ('deriv, 'exp) T.result
  type ('deriv, 'exp) parser = ('deriv, 'exp) T.parser
  type ('deriv, 'exp) raw_parser = ('deriv, 'exp) T.raw_parser

  let merge_region = function
    | (Some a, Some b) -> Some (Tk.cover_region a b)
    | (a, None)        -> a
    | (None, b)        -> b


  let return' : 'e -> 'e exp = fun x -> (x, None)
  let fmap'   : ('e0 -> 'e1) -> 'e0 exp -> 'e1 exp = Data.with_snd
  let ap'     : ('e0 -> 'e1) exp -> 'e0 exp -> 'e1 exp =
    fun (f, reg0) (e0, reg1) -> (f e0, merge_region (reg0, reg1))

  let bind : ('deriv, 'e0 exp) parser -> ('e0 exp -> ('deriv, 'e1 exp) parser) -> ('deriv, 'e1 exp) parser =
    fun ma f dv ->
      match ma dv with
        | None                -> None
        | Some ((_, ra as a), dv) -> 
          (match f a dv with
            | None                -> None
            | Some ((eb, rb), dv) -> Some ((eb, merge_region (ra, rb)), dv))

  let (>>=) = bind

  let return : 'e exp -> ('deriv, 'e exp) parser = fun e dv -> Some (e, dv)

  let mzero : ('deriv, 'e exp) parser = fun _ -> None

  let mplus : ('deriv, 'e exp) parser -> ('deriv, 'e exp) parser -> ('deriv, 'e exp) parser =
    fun ma mb dv ->
      match ma dv with
        | None -> mb dv
        | v    -> v

  let and_parser : ('deriv, 'e exp) parser -> ('deriv, unit exp) parser =
    fun ma dv ->
      match ma dv with
        | None   -> None
        | Some _ -> Some (((), None), dv)

  let not_parser : ('deriv, 'e exp) parser -> ('deriv, unit exp) parser =
    fun ma dv ->
      match ma dv with
        | None   -> Some (((), None), dv)
        | Some _ -> None

  let forget : ('deriv, 'e exp) parser -> ('deriv, 'e exp) parser =
    fun ma dv ->
      match ma dv with
        | Some ((ea, _), dv) -> Some ((ea, None), dv)
        | None                -> None

  let derive_seq
      : (token exp seq -> 'derive) -> ('deriv, token exp) raw_parser =
    fun do_derive dv ->
      match ZL.peek dv with
        | None                             -> None
        | Some (lazy (ZL.Node (fst, dv))) -> Some (fst, do_derive dv)

  let to_satisfy
      : ('deriv, token exp) parser -> (token -> bool)
        -> ('deriv, token exp) parser =
    fun tp pre d ->
      match tp d with
        | (Some ((tk, _), d') as r) when pre tk -> r
        | Some _ | None                         -> None

  let debug = true

  let debug_show_shift =
    if debug then
      (function 
        | (lazy (ZL.Cons (lazy (ZL.Node (_, next)), _))) as tfo ->
          Printf.fprintf stderr "shift invocation: next: %s\n"
            (ZL.foldl'
               (fun res tr -> res ^ " " ^ Tk.type_to_string (fst (ZL.t_peek tr)))
               "" next);
          flush stderr;
          tfo
        | tfo -> tfo)
    else fun tfo -> tfo

  let may_shift
      : token exp seq -> token exp seq option =
    fun tfo ->
      match ZL.next tfo with
        | Some (_, tfo) -> Some (debug_show_shift tfo)
        | None          -> None

  (* マッチしなかったときに次の枝をマッチする parser に変換 *)
  let match_or_shift
      : (token exp seq -> 'deriv) -> ('deriv -> token exp seq)
    -> ('deriv, token exp) parser
    -> ('deriv, token exp) parser =
    fun to_d from_d ma dv ->
      match ma dv with
        | None ->
          (match may_shift (from_d dv) with
            | None     -> None
            | Some tfo -> ma (to_d tfo))
        | v    -> v

  let run : ('deriv, 'e exp) parser -> 'deriv -> ('deriv, 'e exp) result =
    fun p dv -> p dv

end

module Combinator(Tk : TOKEN) = Parser.Combinator2(DriverWithRegion(Tk))
