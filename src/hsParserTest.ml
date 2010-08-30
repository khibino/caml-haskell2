
open Simple.Combinator

module P = HsParser
module LO = HsLayout

let parse_forest lzf seq_parser =
  let _ = HsParserState.begin_parse_module (Symbol.intern "Main") in
  run seq_parser lzf

let parse lzl seq_parser =
  parse_forest
    (LO.lazy_L
       (LO.input_of_L lzl))
    seq_parser

let parse_raw lzl seq_parser =
  parse_forest
    [LazyList.tree_of_lzlist lzl]
    seq_parser

let parse_chan chan =
  parse (HsUlexer.lazy_list_of_channel chan)

let parse_chan_raw chan =
  parse_raw (HsUlexer.lazy_list_of_channel chan)

let parse_string str =
  print_endline ("input: " ^ str);
  parse (HsUlexer.lazy_list_of_string str)

let parse_string_raw str =
  print_endline ("input: " ^ str);
  parse_raw (HsUlexer.lazy_list_of_string str)

let ch_id () = parse_chan stdin P.test_id
let ch_exp () = parse_chan stdin P.test_exp
let ch_lexp () = parse_chan stdin P.test_lexp

let ch_type () = parse_chan stdin P.test_type
let ch_apat () = parse_chan stdin P.test_apat

let ch_decls () = parse_chan_raw stdin P.test_decls
let ch_decl () = parse_chan_raw stdin P.test_decl
let ch_decls_cont () = parse_chan_raw stdin P.test_decls_cont

let s1_lambda () = parse_string "\\ (1:xs,'a':ys) -> (xs, ys)" P.test_exp
let s1_lcomp  () = parse_string "[(x, y) | x <- [1,2,3], y <- ['a','b'] ]" P.test_exp
let s1_lbl_upd () = parse_string "x { Foo.a = y, Foo.b = z } { Foo.c = p, Foo.d = q }" P.test_exp
let s1_do () = parse_string "do { let { x = 1; y = 2}; 1 }" P.test_exp

let s2_t_fun () = parse_string "(Either String a, b -> c)" P.test_type
