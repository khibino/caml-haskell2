(*  *)


module type BASIC_OP2  =
sig
  type token
  type 'tk tklist
  type 'e result

  type 'e parser = token tklist -> 'e result

  val bind    : 'e0 parser -> ('e0 -> 'e1 parser) -> 'e1 parser
  val return  : 'e -> 'e parser
  val mzero    : 'e parser
  val any      : token parser
  val mplus   : 'e parser -> 'e parser -> 'e parser
  val and_parser : 'e parser -> unit parser
  val not_parser : 'e parser -> unit parser
  val satisfy : string -> (token -> bool) -> token parser
  val match_or_shift : token parser -> token parser

  val tokens  :  (unit -> token) -> token tklist

  val run     : 'e parser -> token tklist -> 'e result
end

module type EAGER_BASIC_OP2 =
sig
  include BASIC_OP2
end

module type LAZY_BASIC_OP2 =
sig
  include BASIC_OP2
end

module Eager2Lazy2 (EOp : EAGER_BASIC_OP2) : LAZY_BASIC_OP2
  (* Exporting type implement. *)
  with type token = EOp.token
  and  type 'tk tklist = 'tk EOp.tklist
  and  type 'e result = 'e EOp.result Lazy.t =
struct
  type token = EOp.token
  type 'tk tklist = 'tk EOp.tklist
  type 'e result = 'e EOp.result Lazy.t

  type 'e parser = token tklist -> 'e result

  let force = Lazy.force

  let l2e p l = force (p l)

  let bind  a b l  = lazy (EOp.bind (l2e a) (fun x -> l2e (b x)) l)
  let return a' l  = lazy (EOp.return a' l)

  let mzero  l     = lazy (EOp.mzero l)
  let any    l     = lazy (EOp.any l)

  let mplus a b l  = lazy (EOp.mplus (l2e a) (l2e b) l)

  let and_parser a l = lazy (EOp.and_parser (l2e a) l)
  let not_parser a l = lazy (EOp.not_parser (l2e a) l)

  let satisfy name p l = lazy (EOp.satisfy name p l)
  let match_or_shift a l = lazy (EOp.match_or_shift (l2e a) l)

  let tokens = EOp.tokens

  let run    p l = p l

(*
  let bind    a b = lazy (EOp.bind (force a) (fun x -> force (b x)))
  let return  a'  = lazy (EOp.return a')

  let mzero        = lazy EOp.mzero
  let any          = lazy EOp.any
  let mplus   a b = lazy (EOp.mplus (force a) (force b))
  let and_parser a = lazy (EOp.and_parser (force a))
  let not_parser a = lazy (EOp.not_parser (force a))
  let satisfy name p = lazy (EOp.satisfy name p)
  let match_or_shift a = lazy (EOp.match_or_shift (force a))

  let tokens = EOp.tokens

  let run    p l = lazy (EOp.run (force p) l)
*)

end

module type COMBINATOR2 =
sig
  type token
  type 'tk tklist
  type 'e parser
  type 'e result

  val force : 'a Lazy.t -> 'a

  val bind    : 'e0 parser -> ('e0 -> 'e1 parser) -> 'e1 parser
  val return  : 'e -> 'e parser
  val mzero   : 'e parser
  val mplus   : 'e parser -> 'e parser -> 'e parser
  val any     : token parser
  val satisfy : string -> (token -> bool) -> token parser
  val match_or_shift : token parser -> token parser

  val (>>=)  : 'e0 parser -> ('e0 -> 'e1 parser) -> 'e1 parser
  val (>>)   : 'e0 parser -> 'e1 parser -> 'e1 parser

  val (<|>)  : 'e parser -> 'e parser -> 'e parser
  val pure   : 'e -> 'e parser

  val lift_a : ('e0 -> 'e1) -> 'e0 parser -> 'e1 parser
  val (<$>)  : ('e0 -> 'e1) -> 'e0 parser -> 'e1 parser
  val ( *<$> )  : ('e0 -> 'e1) -> 'e0 parser -> 'e1 parser

  val and_parser : 'e parser -> unit parser
  val (~&)       : 'e parser -> unit parser
  val not_parser : 'e parser -> unit parser
  val (~!)       : 'e parser -> unit parser

  val call_parser : (unit -> 'e parser) -> 'e parser
  val (~$)        : (unit -> 'e parser) -> 'e parser

  val ap    : ('e0 -> 'e1) parser -> 'e0 parser -> 'e1 parser
  val (<*>) : ('e0 -> 'e1) parser -> 'e0 parser -> 'e1 parser
  val ( *<*> ) : ('e0 -> 'e1) parser -> 'e0 parser -> 'e1 parser

  val ibind  : 'e0 parser -> 'e1 parser -> 'e1 parser
  val ( *> ) : 'e0 parser -> 'e1 parser -> 'e1 parser
  val ( **> ) : 'e0 parser -> 'e1 parser -> 'e1 parser

  val skip   : 'e0 parser -> 'e1 parser -> 'e0 parser
  val ( <* ) : 'e0 parser -> 'e1 parser -> 'e0 parser
  val ( **< ) : 'e0 parser -> 'e1 parser -> 'e0 parser

  val some   : 'e parser -> 'e list parser
  val many   : 'e parser -> 'e list parser

  val optional : 'e parser -> 'e option parser
  val (~?)     : 'e parser -> 'e option parser

  val pred : string -> (token -> bool) -> token parser
  val just : string -> token -> token parser
  val untag : string -> (token -> 'e option) -> 'e parser

  val tokens : (unit -> token) -> token tklist

  val run : 'e parser -> token tklist -> 'e result
end

module Combinator2ByLazy (LOp : LAZY_BASIC_OP2) : COMBINATOR2
  (* Exporting type implement. *)
  with type token = LOp.token
  and  type 'tk tklist = 'tk LOp.tklist
  and  type 'e parser = 'e LOp.parser
  and  type 'e result = 'e LOp.result  =
struct
  type token = LOp.token
  type ('tk) tklist = ('tk) LOp.tklist
  type 'e parser = 'e LOp.parser
  type 'e result = 'e LOp.result

  let force = Lazy.force

  let bind   = LOp.bind
  let return = LOp.return
  let mzero   = LOp.mzero
  let mplus  = LOp.mplus 
  let any    = LOp.any
  let satisfy = LOp.satisfy
  let match_or_shift = LOp.match_or_shift

  let (>>=)  = bind
  let (>>)   = fun m k -> m >>= (fun _ -> k)

  let (<|>)  = mplus
  let pure   = return

  let lift_a f lz =
    lz >>= fun a -> return (f a)
  let (<$>) = lift_a
  let ( *<$>) = lift_a

  let and_parser = LOp.and_parser
  let (~&) = and_parser

  let not_parser = LOp.not_parser
  let (~!) = not_parser

  let call_parser f =
    return () >>= fun a -> f a
  let (~$) = call_parser

  let ap mf lz =
    mf >>= fun f ->
      lz >>= fun l -> return (f l)
  let (<*>) = ap
  let ( *<*>) = ap

  let ibind az bz =
    az >>= fun _ -> bz
  let ( *> ) = ibind
  let ( **> ) = ibind

  let skip az bz =
    (az >>= fun a -> bz >> return a)
  let ( <* ) = skip
  let ( **< ) = skip

  let cons = (fun h t -> h :: t)

(*           where many_v = some_v <|> pure [] *)
(*                 some_v = (:) <$> v <*> many_v *)
  let call = call_parser
  let rec many az () = call (some az) <|> pure []
  and     some az () = cons <$> az <*> call (many az)

  let many az = many az ()
  let some az = some az ()

  let optional az = ((fun x -> Some x) <$> az) <|> pure None
  let (~?) = optional

  let pred = LOp.satisfy
  let just name tk = pred name ((=) tk)
  let untag name f =
    (fun i -> match f i with
      | Some v -> v
      | None   -> failwith "")
      <$>
      pred name (fun i -> match f i with
        | Some _ -> true
        | None   -> false)

  let tokens = LOp.tokens

  let run = LOp.run
end

module Combinator2 (EOp : EAGER_BASIC_OP2) : COMBINATOR2
  with type token = Eager2Lazy2(EOp).token
  and  type 'tk tklist = 'tk Eager2Lazy2(EOp).tklist
  and  type 'e parser = 'e Eager2Lazy2(EOp).parser
  and  type 'e result = 'e Eager2Lazy2(EOp).result  =
struct
  module LOp = Eager2Lazy2(EOp)

  include Combinator2ByLazy(LOp)
end
