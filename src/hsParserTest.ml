
open Simple.Combinator

module P = HsParser
module LO = HsLayout

let parse lzl seq_parser =
  let _ = HsParserState.begin_parse_module (Symbol.intern "Main") in
  run seq_parser
    (LO.lazy_L
       (LO.input_of_L lzl))

let parse_chan chan =
  parse (HsUlexer.lazy_list_of_channel chan)

let parse_string str =
  print_endline ("input: " ^ str);
  parse (HsUlexer.lazy_list_of_string str)

let ch0 () = parse_chan stdin P.test_s0
let ch1 () = parse_chan stdin P.test_s1
let ch2 () = parse_chan stdin P.test_s2
let ch3 () = parse_chan stdin P.test_s3

let s1_lambda () = parse_string "\\ (1:xs,'a':ys) -> (xs, ys)" P.test_s1
let s1_lcomp  () = parse_string "[(x, y) | x <- [1,2,3], y <- ['a','b'] ]" P.test_s1
let s1_lbl_upd () = parse_string "x { Foo.a = y, Foo.b = z } { Foo.c = p, Foo.d = q }" P.test_s1

let s2_t_fun () = parse_string "(Either String a, b -> c)" P.test_s2
