(*  *)


module type BASIC_OP2 =
sig
  type ('tk) tklist
  type ('tk, 'e) parser
  type ('e) result

  val bind    : ('tk, 'e0) parser -> ('e0 -> ('tk, 'e1) parser) -> ('tk, 'e1) parser
  val return  : 'e -> ('tk, 'e) parser
  val fail    : ('tk, 'e) parser
  val mplus   : ('tk, 'e) parser -> ('tk, 'e) parser -> ('tk, 'e) parser
  val and_parser : ('tk, 'e) parser -> ('tk, unit) parser
  val not_parser : ('tk, 'e) parser -> ('tk, unit) parser
  val satisfy : ('tk -> bool) -> ('tk, 'tk) parser

  val tokens  :  (unit -> 'a) -> ('a) tklist

  val run     : ('tk, 'e) parser -> ('tk) tklist -> ('e) result
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
  with type ('tk) tklist = ('tk) EOp.tklist
  and  type ('tk, 'e) parser = ('tk, 'e) EOp.parser Lazy.t
  and  type ('e) result = ('e) EOp.result  =
struct
  type ('tk) tklist = ('tk) EOp.tklist
  type ('tk, 'e) parser = ('tk, 'e) EOp.parser Lazy.t (* may change to ('tk, 'e Lazy.t) EOp.parser Lazy.t *)
  type ('e) result = ('e) EOp.result

  let force = Lazy.force

  let bind    a b = lazy (EOp.bind (force a) (fun x -> force (b x)))
  let return  a   = lazy (EOp.return a)

  let fail        = lazy EOp.fail
  let mplus   a b = lazy (EOp.mplus (force a) (force b))
  let and_parser a = lazy (EOp.and_parser (force a))
  let not_parser a = lazy (EOp.not_parser (force a))
  let satisfy p   = lazy (EOp.satisfy p)

  let tokens = EOp.tokens

  let run     p l = EOp.run (force p) l
end

module type COMBINATOR2 =
sig
  type ('tk) tklist
  type ('tk, 'e) parser
  type ('tk, 'e) uparser
  type ('e) result

  val force : 'a Lazy.t -> 'a

  val bind    : ('tk, 'e0) parser -> ('e0 -> ('tk, 'e1) parser) -> ('tk, 'e1) parser
  val return  : 'e -> ('tk, 'e) parser
  val fail    : ('tk, 'e) parser
  val mplus   : ('tk, 'e) parser -> ('tk, 'e) parser -> ('tk, 'e) parser
  val satisfy : ('tk -> bool) -> ('tk, 'tk) parser

  val (>>=)  : ('tk, 'e0) parser -> ('e0 -> ('tk, 'e1) parser) -> ('tk, 'e1) parser
  val (>>)   : ('tk, 'e0) parser -> ('tk, 'e1) parser -> ('tk, 'e1) parser

  val (<|>)  : ('tk, 'e) parser -> ('tk, 'e) parser -> ('tk, 'e) parser
  val pure   : 'e -> ('tk, 'e) parser

  val lift_a : ('e0 -> 'e1) -> ('tk, 'e0) parser -> ('tk, 'e1) parser
  val (<$>)  : ('e0 -> 'e1) -> ('tk, 'e0) parser -> ('tk, 'e1) parser
  val ( *<$> )  : ('e0 -> 'e1) -> ('tk, 'e0) parser -> ('tk, 'e1) parser

  val and_parser : ('tk, 'e) parser -> ('tk, unit) parser
  val (~&)       : ('tk, 'e) parser -> ('tk, unit) parser
  val not_parser : ('tk, 'e) parser -> ('tk, unit) parser
  val (~!)       : ('tk, 'e) parser -> ('tk, unit) parser

  val call_parser : (unit -> ('tk, 'e) parser) -> ('tk, 'e) parser
  val (~$)        : (unit -> ('tk, 'e) parser) -> ('tk, 'e) parser

  val ap    : ('tk, 'e0 -> 'e1) parser -> ('tk, 'e0) parser -> ('tk, 'e1) parser
  val (<*>) : ('tk, 'e0 -> 'e1) parser -> ('tk, 'e0) parser -> ('tk, 'e1) parser
  val ( *<*> ) : ('tk, 'e0 -> 'e1) parser -> ('tk, 'e0) parser -> ('tk, 'e1) parser

  val ibind  : ('tk, 'e0) parser -> ('tk, 'e1) parser -> ('tk, 'e1) parser
  val ( *> ) : ('tk, 'e0) parser -> ('tk, 'e1) parser -> ('tk, 'e1) parser
  val ( **> ) : ('tk, 'e0) parser -> ('tk, 'e1) parser -> ('tk, 'e1) parser

  val skip   : ('tk, 'e0) parser -> ('tk, 'e1) parser -> ('tk, 'e0) parser
  val ( <* ) : ('tk, 'e0) parser -> ('tk, 'e1) parser -> ('tk, 'e0) parser
  val ( **< ) : ('tk, 'e0) parser -> ('tk, 'e1) parser -> ('tk, 'e0) parser

  val some   : ('tk, 'e) parser -> ('tk, 'e list) parser
  val many   : ('tk, 'e) parser -> ('tk, 'e list) parser

  val optional : ('tk, 'e) parser -> ('tk, 'e option) parser
  val (~?)     : ('tk, 'e) parser -> ('tk, 'e option) parser

  val pred : ('tk -> bool) -> ('tk, 'tk) parser
  val just : 'tk -> ('tk, 'tk) parser
  val untag : ('tk -> 'e option) -> ('tk, 'e) parser

  val tokens : (unit -> 'a) -> ('a) tklist

  val run : ('tk, 'e) parser -> ('tk) tklist -> ('e) result
end

module Combinator2ByLazy (LOp : LAZY_BASIC_OP2) : COMBINATOR2
  (* Exporting type implement. *)
  with type ('tk) tklist = ('tk) LOp.tklist
  and  type ('tk, 'e) parser = ('tk, 'e) LOp.parser
  and  type ('tk, 'e) uparser = ('tk, 'e) LOp.parser
  and  type ('e) result = ('e) LOp.result  =
struct
  type ('tk) tklist = ('tk) LOp.tklist
  type ('tk, 'e) parser = ('tk, 'e) LOp.parser
  type ('tk, 'e) uparser = ('tk, 'e) LOp.parser
  type ('e) result = ('e) LOp.result

  let force = Lazy.force

  let bind   = LOp.bind
  let return = LOp.return
  let fail   = LOp.fail
  let mplus  = LOp.mplus 
  let satisfy = LOp.satisfy

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

  let not_parser = LOp.and_parser
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
  let just tk = pred ((=) tk)
  let untag f =
    (fun i -> match f i with
      | Some v -> v
      | None   -> failwith "")
      <$>
      pred (fun i -> match f i with
        | Some _ -> true
        | None   -> false)

  let tokens = LOp.tokens

  let run = LOp.run
end

module Combinator2 (EOp : EAGER_BASIC_OP2) : COMBINATOR2
  with type ('tk) tklist = ('tk) Eager2Lazy2(EOp).tklist
  and  type ('tk, 'e) parser = ('tk, 'e) Eager2Lazy2(EOp).parser
  and  type ('tk, 'e) uparser = ('tk, 'e) Eager2Lazy2(EOp).parser
  and  type ('e) result = ('e) Eager2Lazy2(EOp).result  =
struct
  module LOp = Eager2Lazy2(EOp)

  include Combinator2ByLazy(LOp)
end
