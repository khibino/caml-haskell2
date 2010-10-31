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
  type ('tk, 'reg) tklist = ('tk, 'reg) exp ZL.forest_t
  type ('tk, 'reg, 'exp) result = ('exp * ('tk, 'reg) tklist) option
  type ('tk, 'reg, 'exp) parser = ('tk, 'reg) tklist -> ('tk, 'reg, 'exp) result
end

module DriverWithRegion(Tk : TOKEN) : DRIVER
  with type token = Tk.type_ 
  and  type 'tk tklist = ('tk, Tk.region) T.tklist
  and  type 'e exp = ('e, Tk.region) T.exp
  and  type 'exp result = (Tk.type_, Tk.region, 'exp) T.result
  and  type 'exp parser = (Tk.type_, Tk.region, 'exp) T.parser
                 =
struct

  type token = Tk.type_

  type 'tk tklist = ('tk, Tk.region) T.tklist
  type 'e exp = ('e, Tk.region) T.exp
  type 'exp result = (Tk.type_, Tk.region, 'exp) T.result
  type 'exp parser = (Tk.type_, Tk.region, 'exp) T.parser

  let merge_region = function
    | (Some a, Some b) -> Some (Tk.cover_region a b)
    | (a, None)        -> a
    | (None, b)        -> b


  let return' : 'e -> 'e exp = fun x -> (x, None)
  let fmap'   : ('e0 -> 'e1) -> 'e0 exp -> 'e1 exp = Data.with_snd
  let ap'     : ('e0 -> 'e1) exp -> 'e0 exp -> 'e1 exp =
    fun (f, reg0) (e0, reg1) -> (f e0, merge_region (reg0, reg1))

  let bind : 'e0 exp parser -> ('e0 exp -> 'e1 exp parser) -> 'e1 exp parser =
    fun ma f tfo ->
      match ma tfo with
        | None                -> None
        | Some ((_, ra as a), tfo) -> 
          (match f a tfo with
            | None                -> None
            | Some ((eb, rb), tfo) -> Some ((eb, merge_region (ra, rb)), tfo))

  let (>>=) = bind

  let return : 'e exp -> 'e exp parser = fun e tfo -> Some (e, tfo)

  let mzero : 'e exp parser = fun _ -> None

  let mplus : 'e exp parser -> 'e exp parser -> 'e exp parser =
    fun ma mb tfo ->
      match ma tfo with
        | None -> mb tfo
        | v    -> v

  let and_parser : 'e exp parser -> unit exp parser =
    fun ma tfo ->
      match ma tfo with
        | None   -> None
        | Some _ -> Some (((), None), tfo)

  let not_parser : 'e exp parser -> unit exp parser =
    fun ma tfo ->
      match ma tfo with
        | None   -> Some (((), None), tfo)
        | Some _ -> None

  let forget : 'e exp parser -> 'e exp parser =
    fun ma -> ma >>= fun (a, _) -> return (return' a)

  let any : token exp parser =
    fun tfo ->
      match ZL.peek tfo with
        | None                         -> None
        | Some (lazy (ZL.Node (node))) -> Some node

  let satisfy : string -> (token -> bool) -> token exp parser =
    fun name pred tfo ->
      match ZL.peek tfo with
        | Some (lazy (ZL.Node ((tk, _), _ as node))) when pred tk
            -> Some node
        | Some _ | None -> None


  module F = Printf
  (* マッチしなかったときに次の枝をマッチする parser に変換 *)
  let match_or_shift : token exp parser -> token exp parser =
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
                         (fun res tr -> res ^ " " ^ Tk.type_to_string (fst (ZL.t_peek tr)))
                         "" next)
                  | _ -> ());
              ma tfo
            | None           -> None)
        | v    -> v

  let run : 'e exp parser -> token tklist -> 'e exp result =
    fun p tkl -> p tkl

end

module Combinator(Tk : TOKEN) = Parser.Combinator2(DriverWithRegion(Tk))
