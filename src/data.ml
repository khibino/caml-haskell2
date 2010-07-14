
let with_snd : ('a -> 'b) -> ('a * 'c) -> ('b * 'c) = fun f (a, b) -> (f a, b)
let with_fst : ('a -> 'b) -> ('c * 'a) -> ('c * 'b) = fun f (a, b) -> (a, f b)
