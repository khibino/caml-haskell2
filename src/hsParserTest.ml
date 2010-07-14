
open Simple.Combinator

let do_in chan_in =
  let _ = HsParserState.begin_parse_module (Symbol.intern "Main") in
  run HsParser.test_s0
    (HsLayout.lazy_L
       (HsLayout.input_of_L (HsUlexer.lazy_list chan_in)))
