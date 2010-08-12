
open Simple.Combinator

module P = HsParser
module LO = HsLayout

let parse_in chan_in seq_parser =
  let _ = HsParserState.begin_parse_module (Symbol.intern "Main") in
  run seq_parser
    (LO.lazy_L
       (LO.input_of_L (HsUlexer.lazy_list chan_in)))

let do0 () = parse_in stdin P.test_s0
let do1 () = parse_in stdin P.test_s1
let do2 () = parse_in stdin P.test_s2
