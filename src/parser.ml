(*  *)


module type BASIC_OP2  =
sig
  type token
  type 'tk tklist
  type 'e exp
  type 'exp result

  type 'exp parser = token tklist -> 'exp result

  val return' : 'e -> 'e exp
  val fmap'   : ('e0 -> 'e1) -> 'e0 exp -> 'e1 exp
  val ap'     : ('e0 -> 'e1) exp -> 'e0 exp -> 'e1 exp

  val bind    : 'e0 exp parser -> ('e0 exp -> 'e1 exp parser) -> 'e1 exp parser
  val return  : 'e exp -> 'e exp parser
  val mzero   : 'e exp parser
  val mplus   : 'e exp parser -> 'e exp parser -> 'e exp parser
  val and_parser : 'e exp parser -> unit exp parser
  val not_parser : 'e exp parser -> unit exp parser
  val forget  : 'e exp parser -> 'e exp parser
  val any     : token exp parser
  val satisfy : string -> (token -> bool) -> token exp parser
  val match_or_shift : token exp parser -> token exp parser

  val run     : 'e exp parser -> token tklist -> 'e exp result
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
  and  type 'e exp     = 'e EOp.exp
  and  type 'exp result = 'exp EOp.result Lazy.t =
struct
  type token = EOp.token
  type 'tk tklist = 'tk EOp.tklist
  type 'e exp     = 'e EOp.exp
  type 'exp result = 'exp EOp.result Lazy.t

  type 'exp parser = token tklist -> 'exp result

  let return' = EOp.return'
  let fmap' = EOp.fmap'
  let ap'   = EOp.ap'

  let force = Lazy.force

  let l2e p l = force (p l)

  let bind  a b l  = lazy (EOp.bind (l2e a) (fun x -> l2e (b x)) l)
  let return a' l  = lazy (EOp.return a' l)

  let mzero  l     = lazy (EOp.mzero l)
  let mplus a b l  = lazy (EOp.mplus (l2e a) (l2e b) l)

  let and_parser a l = lazy (EOp.and_parser (l2e a) l)
  let not_parser a l = lazy (EOp.not_parser (l2e a) l)

  let forget a l   = lazy (EOp.forget (l2e a) l)

  let any    l     = lazy (EOp.any l)
  let satisfy name p l = lazy (EOp.satisfy name p l)
  let match_or_shift a l = lazy (EOp.match_or_shift (l2e a) l)

  let run    p l = p l

end

module type COMBINATOR2 =
sig
  type token
  type 'tk tklist
  type 'e exp
  type 'exp parser
  type 'exp result

  val return' : 'e -> 'e exp
  val fmap'   : ('e0 -> 'e1) -> 'e0 exp -> 'e1 exp
  val ap'     : ('e0 -> 'e1) exp -> 'e0 exp -> 'e1 exp

  (* val force : 'a Lazy.t -> 'a *)

  val bind    : 'e0 exp parser -> ('e0 exp -> 'e1 exp parser) -> 'e1 exp parser
  val return  : 'e exp -> 'e exp parser
  val mzero   : 'e exp parser
  val mplus   : 'e exp parser -> 'e exp parser -> 'e exp parser
  val forget  : 'e exp parser -> 'e exp parser
  val any     : token exp parser
  val satisfy : string -> (token -> bool) -> token exp parser
  val match_or_shift : token exp parser -> token exp parser

  val (>>=)  : 'e0 exp parser -> ('e0 exp -> 'e1 exp parser) -> 'e1 exp parser
  val (>>)   : 'e0 exp parser -> 'e1 exp parser -> 'e1 exp parser

  val (<|>)  : 'e exp parser -> 'e exp parser -> 'e exp parser
  val pure   : 'e exp -> 'e exp parser

  val lift_a : ('e0 -> 'e1) -> 'e0 exp parser -> 'e1 exp parser
  val (<$>)  : ('e0 -> 'e1) -> 'e0 exp parser -> 'e1 exp parser
  val ( *<$> )  : ('e0 -> 'e1) -> 'e0 exp parser -> 'e1 exp parser

  val and_parser : 'e exp parser -> unit exp parser
  val (~&)       : 'e exp parser -> unit exp parser
  val not_parser : 'e exp parser -> unit exp parser
  val (~!)       : 'e exp parser -> unit exp parser

  val call_parser : (unit -> 'e exp parser) -> 'e exp parser
  val (~$)        : (unit -> 'e exp parser) -> 'e exp parser

  val ap    : ('e0 -> 'e1) exp parser -> 'e0 exp parser -> 'e1 exp parser
  val (<*>) : ('e0 -> 'e1) exp parser -> 'e0 exp parser -> 'e1 exp parser
  val ( *<*> ) : ('e0 -> 'e1) exp parser -> 'e0 exp parser -> 'e1 exp parser

  val ibind  : 'e0 exp parser -> 'e1 exp parser -> 'e1 exp parser
  val ( *> ) : 'e0 exp parser -> 'e1 exp parser -> 'e1 exp parser
  val ( **> ) : 'e0 exp parser -> 'e1 exp parser -> 'e1 exp parser

  val skip   : 'e0 exp parser -> 'e1 exp parser -> 'e0 exp parser
  val ( <* ) : 'e0 exp parser -> 'e1 exp parser -> 'e0 exp parser
  val ( **< ) : 'e0 exp parser -> 'e1 exp parser -> 'e0 exp parser

  val ibind' : 'e0 exp parser -> 'e1 exp parser -> 'e1 exp parser
  val ( **!> ) : 'e0 exp parser -> 'e1 exp parser -> 'e1 exp parser

  val skip'  : 'e0 exp parser -> 'e1 exp parser -> 'e0 exp parser
  val ( **<! ) : 'e0 exp parser -> 'e1 exp parser -> 'e0 exp parser

  val some   : 'e exp parser -> 'e list exp parser
  val many   : 'e exp parser -> 'e list exp parser

  val optional : 'e exp parser -> 'e option exp parser
  val (~?)     : 'e exp parser -> 'e option exp parser

  val pred : string -> (token -> bool) -> token exp parser
  val just : string -> token -> token exp parser
  val untag : string -> (token -> 'e option) -> 'e exp parser

  val run : 'e exp parser -> token tklist -> 'e exp result
end

module Combinator2ByLazy (LOp : LAZY_BASIC_OP2) : COMBINATOR2
  (* Exporting type implement. *)
  with type token = LOp.token
  and  type 'tk tklist = 'tk LOp.tklist
  and  type 'e exp     = 'e LOp.exp
  and  type 'exp parser = 'exp LOp.parser
  and  type 'exp result = 'exp LOp.result  =
struct
  type token = LOp.token
  type ('tk) tklist = ('tk) LOp.tklist
  type 'e exp     = 'e LOp.exp
  type 'exp parser = 'exp LOp.parser
  type 'exp result = 'exp LOp.result

  let return' = LOp.return'
  let fmap' = LOp.fmap'
  let ap'   = LOp.ap'

  let force = Lazy.force

  let bind   = LOp.bind
  let return = LOp.return
  let mzero   = LOp.mzero
  let mplus  = LOp.mplus 
  let forget = LOp.forget
  let any    = LOp.any
  let satisfy = LOp.satisfy
  let match_or_shift = LOp.match_or_shift

  let (>>=)  = bind
  let (>>)   = fun m k -> m >>= (fun _ -> k)

  let (<|>)  = mplus
  let pure   = return

  let lift_a f lz =
    lz >>= fun a -> return (fmap' f a)
  let (<$>) = lift_a
  let ( *<$>) = lift_a

  let and_parser = LOp.and_parser
  let (~&) = and_parser

  let not_parser = LOp.not_parser
  let (~!) = not_parser

  let call_parser f = return (return' ()) >>= (fun _ -> f ())
  let (~$) = call_parser

  let ap mf lz =
    mf >>= fun f ->
      lz >>= fun l -> return (ap' f l)
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

  let ibind' az bz = ibind (forget az) bz
  let ( **!> ) = ibind'

  let skip'  az bz = skip az (forget bz)
  let ( **<! ) = skip'

  let cons = (fun h t -> h :: t)

(*           where many_v = some_v <|> pure [] *)
(*                 some_v = (:) <$> v <*> many_v *)
  let call = call_parser
  let rec many az () = call (some az) <|> pure (return' [])
  and     some az () = cons <$> az <*> call (many az)

  let many az = many az ()
  let some az = some az ()

  let optional az = ((fun x -> Some x) <$> az) <|> pure (return' None)
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

  let run = LOp.run
end

module Combinator2 (EOp : EAGER_BASIC_OP2) : COMBINATOR2
  with type token = Eager2Lazy2(EOp).token
  and  type 'e exp = 'e Eager2Lazy2(EOp).exp
  and  type 'tk tklist = 'tk Eager2Lazy2(EOp).tklist
  and  type 'exp parser = 'exp Eager2Lazy2(EOp).parser
  and  type 'exp result = 'exp Eager2Lazy2(EOp).result  =
struct
  module LOp = Eager2Lazy2(EOp)

  include Combinator2ByLazy(LOp)
end
