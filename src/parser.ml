(*  *)


module type BASIC_OP2 =
sig
  type ('tk) tklist
  type ('tk, 'e) parser
  type ('e) result

  val bind    : ('tk, 'e0) parser -> ('e0 -> ('tk, 'e1) parser) -> ('tk, 'e1) parser
  val return  : 'e -> ('tk, 'e) parser
  val mplus   : ('tk, 'e) parser -> ('tk, 'e) parser -> ('tk, 'e) parser
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

  (* Same as EAGER_BASIC_OP2, but may change from 'e0 to 'e0 Lazy.t *)
  (* val bind    : ('tk, 'e0) parser -> ('e0 -> ('tk, 'e1) parser) -> ('tk, 'e1) parser *)
  (* Same as EAGER_BASIC_OP2, but may change from 'e to 'e Lazy.t *)
  (* val return  : 'e -> ('tk, 'e) parser *)
end

module Eager2Lazy2 (EOp : EAGER_BASIC_OP2) : LAZY_BASIC_OP2
  (* Exporting type implement. *)
  with type ('tk) tklist = ('tk) EOp.tklist
  and  type ('tk, 'e) parser = ('tk, 'e) EOp.parser Lazy.t
  and  type ('e) result = ('e) EOp.result  =
struct
  type ('tk) tklist = ('tk) EOp.tklist
  type ('tk, 'e) parser = ('tk, 'e) EOp.parser Lazy.t
  type ('e) result = ('e) EOp.result

  let force = Lazy.force

  let bind    a b = lazy (EOp.bind (force a) (fun x -> force (b x)))
  let return  a   = lazy (EOp.return a)
  let mplus   a b = lazy (EOp.mplus (force a) (force b))
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
  val mplus   : ('tk, 'e) parser -> ('tk, 'e) parser -> ('tk, 'e) parser
  val satisfy : ('tk -> bool) -> ('tk, 'tk) parser

  val (>>=)  : ('tk, 'e0) parser -> ('e0 -> ('tk, 'e1) parser) -> ('tk, 'e1) parser
  val (>>)   : ('tk, 'e0) parser -> ('tk, 'e1) parser -> ('tk, 'e1) parser

  val (<|>)  : ('tk, 'e) parser -> ('tk, 'e) parser -> ('tk, 'e) parser
  val pure   : 'e -> ('tk, 'e) parser

  val lift_a : ('e0 -> 'e1) -> ('tk, 'e0) parser -> ('tk, 'e1) parser
  val (<$>)  : ('e0 -> 'e1) -> ('tk, 'e0) parser -> ('tk, 'e1) parser

  val call_parser : (unit -> ('tk, 'e) parser) -> ('tk, 'e) parser

  val ap    : ('tk, 'e0 -> 'e1) parser -> ('tk, 'e0) parser -> ('tk, 'e1) parser
  val (<*>) : ('tk, 'e0 -> 'e1) parser -> ('tk, 'e0) parser -> ('tk, 'e1) parser

  val ibind  : ('tk, 'e0) parser -> ('tk, 'e1) parser -> ('tk, 'e1) parser
  val ( *> ) : ('tk, 'e0) parser -> ('tk, 'e1) parser -> ('tk, 'e1) parser

  val skip   : ('tk, 'e0) parser -> ('tk, 'e1) parser -> ('tk, 'e0) parser
  val ( <* ) : ('tk, 'e0) parser -> ('tk, 'e1) parser -> ('tk, 'e0) parser

  val pred : ('tk -> bool) -> ('tk, 'tk) parser
  val just : 'tk -> ('tk, 'tk) parser

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
  let mplus  = LOp.mplus 
  let satisfy = LOp.satisfy

  let (>>=)  = bind
  let (>>)   = fun m k -> m >>= (fun _ -> k)

  let (<|>)  = mplus
  let pure   = return

  let lift_a f lz =
    lz >>= fun a -> return (f a)
  let (<$>) = lift_a

  let call_parser f =
    return () >>= fun a -> f a

  let ap mf lz =
    mf >>= fun f ->
      lz >>= fun l -> return (f l)
  let (<*>) = ap

  let ibind az bz =
    az >>= fun _ -> bz
  let ( *> ) = ibind

  let skip az bz =
    (az >>= fun a -> bz >> return a)
  let ( <* ) = skip

  let pred = LOp.satisfy
  let just tk = pred ((=) tk)

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

module type BASIC_OP3 =
sig
  type ('tk, 'st) tklist
  type ('tk, 'e, 'st) parser
  type ('e, 'st) result

  val bind    : ('tk, 'e0, 'st) parser -> ('e0 -> ('tk, 'e1, 'st) parser) -> ('tk, 'e1, 'st) parser
  val return  : 'e -> ('tk, 'e, 'st) parser
  val mplus   : ('tk, 'e, 'st) parser -> ('tk, 'e, 'st) parser -> ('tk, 'e, 'st) parser
  val satisfy : ('tk -> bool) -> ('tk, 'tk, 'st) parser

  val tokens  :  (unit -> 'a) -> 'b -> ('a -> 'b -> 'b) -> ('a, 'b) tklist

  val run     : ('tk, 'e, 'st) parser -> ('tk, 'st) tklist -> ('e, 'st) result
end

module type EAGER_BASIC_OP3 =
sig
  include BASIC_OP3
end

module type LAZY_BASIC_OP3 =
sig
  include BASIC_OP3

  (* Same as EAGER_BASIC_OP3, but may change from 'e0 to 'e0 Lazy.t *)
  (* val bind    : ('tk, 'e0, 'st) parser -> ('e0 -> ('tk, 'e1, 'st) parser) -> ('tk, 'e1, 'st) parser *)
  (* Same as EAGER_BASIC_OP3, but may change from 'e to 'e Lazy.t *)
  (* val return  : 'e -> ('tk, 'e, 'st) parser *)
end

module Eager2Lazy3 (EOp : EAGER_BASIC_OP3) : LAZY_BASIC_OP3
  (* Exporting type implement. *)
  with type ('tk, 'st) tklist = ('tk, 'st) EOp.tklist
  and  type ('tk, 'e, 'st) parser = ('tk, 'e, 'st) EOp.parser Lazy.t
  and  type ('e, 'st) result = ('e, 'st) EOp.result  =
struct
  type ('tk, 'st) tklist = ('tk, 'st) EOp.tklist
  type ('tk, 'e, 'st) parser = ('tk, 'e, 'st) EOp.parser Lazy.t
  type ('e, 'st) result = ('e, 'st) EOp.result

  let force = Lazy.force

  let bind    a b = lazy (EOp.bind (force a) (fun x -> force (b x)))
  let return  a   = lazy (EOp.return a)
  let mplus   a b = lazy (EOp.mplus (force a) (force b))
  let satisfy p   = lazy (EOp.satisfy p)

  let tokens = EOp.tokens

  let run     p l = EOp.run (force p) l
end

module type COMBINATOR3 =
sig
  type ('tk, 'st) tklist
  type ('tk, 'e, 'st) parser
  type ('tk, 'e) uparser
  type ('e, 'st) result

  val force : 'a Lazy.t -> 'a

  val bind    : ('tk, 'e0, 'st) parser -> ('e0 -> ('tk, 'e1, 'st) parser) -> ('tk, 'e1, 'st) parser
  val return  : 'e -> ('tk, 'e, 'st) parser
  val mplus   : ('tk, 'e, 'st) parser -> ('tk, 'e, 'st) parser -> ('tk, 'e, 'st) parser
  val satisfy : ('tk -> bool) -> ('tk, 'tk, 'st) parser

  val (>>=)  : ('tk, 'e0, 'st) parser -> ('e0 -> ('tk, 'e1, 'st) parser) -> ('tk, 'e1, 'st) parser
  val (>>)   : ('tk, 'e0, 'st) parser -> ('tk, 'e1, 'st) parser -> ('tk, 'e1, 'st) parser

  val (<|>)  : ('tk, 'e, 'st) parser -> ('tk, 'e, 'st) parser -> ('tk, 'e, 'st) parser
  val pure   : 'e -> ('tk, 'e, 'st) parser

  val lift_a : ('e0 -> 'e1) -> ('tk, 'e0, 'st) parser -> ('tk, 'e1, 'st) parser
  val (<$>)  : ('e0 -> 'e1) -> ('tk, 'e0, 'st) parser -> ('tk, 'e1, 'st) parser

  val call_parser : (unit -> ('tk, 'e, 'st) parser) -> ('tk, 'e, 'st) parser

  val ap    : ('tk, 'e0 -> 'e1, 'st) parser -> ('tk, 'e0, 'st) parser -> ('tk, 'e1, 'st) parser
  val (<*>) : ('tk, 'e0 -> 'e1, 'st) parser -> ('tk, 'e0, 'st) parser -> ('tk, 'e1, 'st) parser

  val ibind  : ('tk, 'e0, 'st) parser -> ('tk, 'e1, 'st) parser -> ('tk, 'e1, 'st) parser
  val ( *> ) : ('tk, 'e0, 'st) parser -> ('tk, 'e1, 'st) parser -> ('tk, 'e1, 'st) parser

  val skip   : ('tk, 'e0, 'st) parser -> ('tk, 'e1, 'st) parser -> ('tk, 'e0, 'st) parser
  val ( <* ) : ('tk, 'e0, 'st) parser -> ('tk, 'e1, 'st) parser -> ('tk, 'e0, 'st) parser

  val pred : ('tk -> bool) -> ('tk, 'tk, 'st) parser
  val just : 'tk -> ('tk, 'tk, 'st) parser

  val tokens : (unit -> 'a) -> 'b -> ('a -> 'b -> 'b) -> ('a, 'b) tklist

  val run : ('tk, 'e, 'st) parser -> ('tk, 'st) tklist -> ('e, 'st) result
end

module Combinator3ByLazy (LOp : LAZY_BASIC_OP3) : COMBINATOR3
  (* Exporting type implement. *)
  with type ('tk, 'st) tklist = ('tk, 'st) LOp.tklist
  and  type ('tk, 'e, 'st) parser = ('tk, 'e, 'st) LOp.parser
  and  type ('tk, 'e) uparser = ('tk, 'e, unit) LOp.parser
  and  type ('e, 'st) result = ('e, 'st) LOp.result  =
struct
  type ('tk, 'st) tklist = ('tk, 'st) LOp.tklist
  type ('tk, 'e, 'st) parser = ('tk, 'e, 'st) LOp.parser
  type ('tk, 'e) uparser = ('tk, 'e, unit) LOp.parser
  type ('e, 'st) result = ('e, 'st) LOp.result

  let force = Lazy.force

  let bind   = LOp.bind
  let return = LOp.return
  let mplus  = LOp.mplus 
  let satisfy = LOp.satisfy

  let (>>=)  = bind
  let (>>)   = fun m k -> m >>= (fun _ -> k)

  let (<|>)  = mplus
  let pure   = return

  let lift_a f lz =
    lz >>= fun a -> return (f a)
  let (<$>) = lift_a

  let call_parser f =
    return () >>= fun a -> f a

  let ap mf lz =
    mf >>= fun f ->
      lz >>= fun l -> return (f l)
  let (<*>) = ap

  let ibind az bz =
    az >>= fun _ -> bz
  let ( *> ) = ibind

  let skip az bz =
    (az >>= fun a -> bz >> return a)
  let ( <* ) = skip

  let pred = LOp.satisfy
  let just tk = pred ((=) tk)

  let tokens = LOp.tokens

  let run = LOp.run
end

module Combinator3 (EOp : EAGER_BASIC_OP3) : COMBINATOR3
  with type ('tk, 'st) tklist = ('tk, 'st) Eager2Lazy3(EOp).tklist
  and  type ('tk, 'e, 'st) parser = ('tk, 'e, 'st) Eager2Lazy3(EOp).parser
  and  type ('tk, 'e) uparser = ('tk, 'e, unit) Eager2Lazy3(EOp).parser
  and  type ('e, 'st) result = ('e, 'st) Eager2Lazy3(EOp).result  =
struct
  module LOp = Eager2Lazy3(EOp)

  include Combinator3ByLazy(LOp)
end

