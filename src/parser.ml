(*  *)


module type BASIC_OP2  =
sig
  type token
  type 'e exp
  type 'exp seq
  type ('deriv, 'exp) result

  type ('deriv, 'exp) parser = 'deriv -> ('deriv, 'exp) result
  type ('deriv, 'exp) raw_parser = 'exp seq -> ('deriv, 'exp) result

  val return' : 'e -> 'e exp
  val fmap'   : ('e0 -> 'e1) -> 'e0 exp -> 'e1 exp
  val ap'     : ('e0 -> 'e1) exp -> 'e0 exp -> 'e1 exp

  val bind    : ('deriv, 'e0 exp) parser -> ('e0 exp -> ('deriv, 'e1 exp) parser) -> ('deriv, 'e1 exp) parser
  val return  : 'e exp -> ('deriv, 'e exp) parser
  val mzero   : ('deriv, 'e exp) parser
  val mplus   : ('deriv, 'e exp) parser -> ('deriv, 'e exp) parser -> ('deriv, 'e exp) parser
  val and_parser : ('deriv, 'e exp) parser -> ('deriv, unit exp) parser
  val not_parser : ('deriv, 'e exp) parser -> ('deriv, unit exp) parser
  val forget  : ('deriv, 'e exp) parser -> ('deriv, 'e exp) parser
  val derive_seq : (token exp seq -> 'deriv) -> token exp seq -> ('deriv, token exp) result
  val to_satisfy : ('deriv, token exp) parser -> (token -> bool) -> ('deriv, token exp) parser
  val match_or_shift : (token exp seq -> 'deriv) -> ('deriv -> token exp seq) -> ('deriv, token exp) parser -> ('deriv, token exp) parser
  (* val any     : ('deriv, token exp) parser *)
  (* val satisfy : string -> (token -> bool) -> ('deriv, token exp) parser *)

  val run     : ('deriv, 'e exp) parser -> 'deriv -> ('deriv, 'e exp) result
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
  and  type 'e exp     = 'e EOp.exp
  and  type 'exp seq   = 'exp EOp.seq
  and  type ('deriv, 'exp) result = ('deriv, 'exp) EOp.result Lazy.t
  and  type ('deriv, 'exp) parser = 'deriv -> ('deriv, 'exp) EOp.result Lazy.t
  and  type ('deriv, 'exp) raw_parser = 'exp EOp.seq -> ('deriv, 'exp) EOp.result Lazy.t
    =
struct
  type token = EOp.token
  type 'e exp     = 'e EOp.exp
  type 'exp seq   = 'exp EOp.seq
  type ('deriv, 'exp) result = ('deriv, 'exp) EOp.result Lazy.t

  type ('deriv, 'exp) parser = 'deriv -> ('deriv, 'exp) result
  type ('deriv, 'exp) raw_parser = 'exp seq -> ('deriv, 'exp) result

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

  let derive_seq d s = lazy (EOp.derive_seq d s)
  let to_satisfy a p l = lazy (EOp.to_satisfy (l2e a) p l)
  let match_or_shift to_d from_d a l = lazy (EOp.match_or_shift to_d from_d (l2e a) l)
  (* let any    l     = lazy (EOp.any l) *)
  (* let satisfy name p l = lazy (EOp.satisfy name p l) *)

  let run    p l = p l

end

module type COMBINATOR2 =
sig
  type token
  type 'e exp
  type 'exp seq
  type ('deriv, 'exp) parser
  type ('deriv, 'exp) result
  type ('deriv, 'exp) raw_parser

  val return' : 'e -> 'e exp
  val fmap'   : ('e0 -> 'e1) -> 'e0 exp -> 'e1 exp
  val ap'     : ('e0 -> 'e1) exp -> 'e0 exp -> 'e1 exp

  (* val force : 'a Lazy.t -> 'a *)

  val bind    : ('deriv, 'e0 exp) parser -> ('e0 exp -> ('deriv, 'e1 exp) parser) -> ('deriv, 'e1 exp) parser
  val return  : 'e exp -> ('deriv, 'e exp) parser
  val mzero   : ('deriv, 'e exp) parser
  val mplus   : ('deriv, 'e exp) parser -> ('deriv, 'e exp) parser -> ('deriv, 'e exp) parser
  val forget  : ('deriv, 'e exp) parser -> ('deriv, 'e exp) parser

  val derive_seq : (token exp seq -> 'deriv) -> token exp seq -> ('deriv, token exp) result
  val to_satisfy : ('deriv, token exp) parser -> (token -> bool) -> ('deriv, token exp) parser
  val match_or_shift : (token exp seq -> 'deriv) -> ('deriv -> token exp seq) -> ('deriv, token exp) parser -> ('deriv, token exp) parser
  (* val any     : ('deriv, token exp) parser *)
  (* val satisfy : string -> (token -> bool) -> ('deriv, token exp) parser *)

  val (>>=)  : ('deriv, 'e0 exp) parser -> ('e0 exp -> ('deriv, 'e1 exp) parser) -> ('deriv, 'e1 exp) parser
  val (>>)   : ('deriv, 'e0 exp) parser -> ('deriv, 'e1 exp) parser -> ('deriv, 'e1 exp) parser

  val (<|>)  : ('deriv, 'e exp) parser -> ('deriv, 'e exp) parser -> ('deriv, 'e exp) parser
  val pure   : 'e exp -> ('deriv, 'e exp) parser

  val lift_a : ('e0 -> 'e1) -> ('deriv, 'e0 exp) parser -> ('deriv, 'e1 exp) parser
  val (<$>)  : ('e0 -> 'e1) -> ('deriv, 'e0 exp) parser -> ('deriv, 'e1 exp) parser
  val ( *<$> )  : ('e0 -> 'e1) -> ('deriv, 'e0 exp) parser -> ('deriv, 'e1 exp) parser

  val and_parser : ('deriv, 'e exp) parser -> ('deriv, unit exp) parser
  val (~&)       : ('deriv, 'e exp) parser -> ('deriv, unit exp) parser
  val not_parser : ('deriv, 'e exp) parser -> ('deriv, unit exp) parser
  val (~!)       : ('deriv, 'e exp) parser -> ('deriv, unit exp) parser

  val call_parser : (unit -> ('deriv, 'e exp) parser) -> ('deriv, 'e exp) parser
  val (~$)        : (unit -> ('deriv, 'e exp) parser) -> ('deriv, 'e exp) parser

  val ap    : ('deriv, ('e0 -> 'e1) exp) parser -> ('deriv, 'e0 exp) parser -> ('deriv, 'e1 exp) parser
  val (<*>) : ('deriv, ('e0 -> 'e1) exp) parser -> ('deriv, 'e0 exp) parser -> ('deriv, 'e1 exp) parser
  val ( *<*> ) : ('deriv, ('e0 -> 'e1) exp) parser -> ('deriv, 'e0 exp) parser -> ('deriv, 'e1 exp) parser

  val ibind  : ('deriv, 'e0 exp) parser -> ('deriv, 'e1 exp) parser -> ('deriv, 'e1 exp) parser
  val ( *> ) : ('deriv, 'e0 exp) parser -> ('deriv, 'e1 exp) parser -> ('deriv, 'e1 exp) parser
  val ( **> ) : ('deriv, 'e0 exp) parser -> ('deriv, 'e1 exp) parser -> ('deriv, 'e1 exp) parser

  val skip   : ('deriv, 'e0 exp) parser -> ('deriv, 'e1 exp) parser -> ('deriv, 'e0 exp) parser
  val ( <* ) : ('deriv, 'e0 exp) parser -> ('deriv, 'e1 exp) parser -> ('deriv, 'e0 exp) parser
  val ( **< ) : ('deriv, 'e0 exp) parser -> ('deriv, 'e1 exp) parser -> ('deriv, 'e0 exp) parser

  val ibind' : ('deriv, 'e0 exp) parser -> ('deriv, 'e1 exp) parser -> ('deriv, 'e1 exp) parser
  val ( **!> ) : ('deriv, 'e0 exp) parser -> ('deriv, 'e1 exp) parser -> ('deriv, 'e1 exp) parser

  val skip'  : ('deriv, 'e0 exp) parser -> ('deriv, 'e1 exp) parser -> ('deriv, 'e0 exp) parser
  val ( **<! ) : ('deriv, 'e0 exp) parser -> ('deriv, 'e1 exp) parser -> ('deriv, 'e0 exp) parser

  val some   : ('deriv, 'e exp) parser -> ('deriv, 'e list exp) parser
  val many   : ('deriv, 'e exp) parser -> ('deriv, 'e list exp) parser

  val optional : ('deriv, 'e exp) parser -> ('deriv, 'e option exp) parser
  val (~?)     : ('deriv, 'e exp) parser -> ('deriv, 'e option exp) parser

  (* val pred : (token -> bool) -> ('deriv, token exp) parser *)
  (* val just : token -> ('deriv, token exp) parser *)
  val to_just : ('deriv, token exp) parser -> token -> ('deriv, token exp) parser
  (* val untag : (token -> 'e option) -> ('deriv, 'e exp) parser *)
  val to_untag : ('deriv, token exp) parser -> (token -> 'e option) -> ('deriv, 'e exp) parser

  val run : ('deriv, 'e exp) parser -> 'deriv -> ('deriv, 'e exp) result
end

module Combinator2ByLazy (LOp : LAZY_BASIC_OP2) : COMBINATOR2
  (* Exporting type implement. *)
  with type token = LOp.token
  and  type 'e exp     = 'e LOp.exp
  and  type 'exp seq   = 'exp LOp.seq
  and  type ('deriv, 'exp) parser = ('deriv, 'exp) LOp.parser
  and  type ('deriv, 'exp) raw_parser = ('deriv, 'exp) LOp.raw_parser
  and  type ('deriv, 'exp) result = ('deriv, 'exp) LOp.result  =
struct
  type token = LOp.token
  type 'e exp     = 'e LOp.exp
  type 'exp seq   = 'exp LOp.seq
  type ('deriv, 'exp) parser = ('deriv, 'exp) LOp.parser
  type ('deriv, 'exp) raw_parser = ('deriv, 'exp) LOp.raw_parser
  type ('deriv, 'exp) result = ('deriv, 'exp) LOp.result

  let return' = LOp.return'
  let fmap' = LOp.fmap'
  let ap'   = LOp.ap'

  let force = Lazy.force

  let bind   = LOp.bind
  let return = LOp.return
  let mzero   = LOp.mzero
  let mplus  = LOp.mplus 
  let forget = LOp.forget

  let derive_seq = LOp.derive_seq
  let to_satisfy = LOp.to_satisfy
  let match_or_shift = LOp.match_or_shift
  (* let any    = LOp.any *)
  (* let satisfy = LOp.satisfy *)

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

  let optional az = (fun x -> Some x) *<$> az <|> pure (return' None)
  let (~?) = optional

  let to_just any tk = (to_satisfy any) ((=) tk)
  let to_untag any f =
    (fun i -> match f i with
      | Some v -> v
      | None   -> failwith "")
      <$>
      (to_satisfy any) (fun i -> match f i with
        | Some _ -> true
        | None   -> false)

(*
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
*)

  let run = LOp.run
end

module Combinator2 (EOp : EAGER_BASIC_OP2) : COMBINATOR2
  with type token = Eager2Lazy2(EOp).token
  and  type 'e exp = 'e Eager2Lazy2(EOp).exp
  and  type 'exp seq   = 'exp Eager2Lazy2(EOp).seq
  and  type ('deriv, 'exp) parser = ('deriv, 'exp) Eager2Lazy2(EOp).parser
  and  type ('deriv, 'exp) raw_parser = ('deriv, 'exp) Eager2Lazy2(EOp).raw_parser
  and  type ('deriv, 'exp) result = ('deriv, 'exp) Eager2Lazy2(EOp).result  =
struct
  module LOp = Eager2Lazy2(EOp)

  include Combinator2ByLazy(LOp)
end
