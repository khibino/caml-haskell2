

module Combinator = ParserDriver.Combinator(Token)
open Combinator

module P = HsParser
module LO = HsLayout
module ZL = LazyList

module D = ParserDerive

let debug = false

let parse_tokens lzl raw seq_parser =
  let parse_tokens zl = run seq_parser (P.make_derive zl) in
  parse_tokens
    (if raw then
        ZL.fomap
        (Data.with_fst (fun x -> Some x))
        (ZL.return (ZL.tree_of_lzlist lzl))
     else let lzl = LO.layout lzl in
          let _ = if debug then LO.show_out lzl in
          lzl)

let parse_as_main lzl raw seq_parser =
  let _ = HsParserState.begin_parse_module
    (Symbol.intern "Main")
  in parse_tokens lzl raw seq_parser

let parse_stdin_as_main raw seq_parser =
  parse_as_main
    (HsUlexer.lazy_list_of_channel stdin)
    raw seq_parser

let parse_stdin_raw_as_main seq_parser =
  parse_stdin_as_main true seq_parser

let parse_stdin = parse_stdin_raw_as_main

let parse_str_as_main raw str seq_parser =
  let _ = if debug then print_endline ("input: " ^ str) in
  parse_as_main (HsUlexer.lazy_list_of_string str) raw seq_parser

let parse_str_raw_as_main str seq_parser =
  parse_str_as_main true str seq_parser

let parse_str = parse_str_raw_as_main

module F = Printf

type error = string option
type test_result = ((int * int) * (string * error) list)

let empty_result : test_result = ((0, 0), [])
let success_p : test_result -> bool =
  function
    | ((x, y), _) when x == y -> true
    | _                       -> false

let show_result : test_result -> string =
  fun (((succ,total), info) as report) ->
    let (n, msg) = 
      List.fold_left
        (fun (i, res) (input, err) -> 
          let msg = match err with
            | None     -> F.sprintf "%d ok '%s'" i input
            | Some err -> F.sprintf "%d err %s '%s'" i err input
          in (i + 1, res ^ msg ^ "\n")
        )
        (0, "")
        (List.rev info)
    in msg
    ^ F.sprintf ". %d/%d - success rate%s\n"
      succ total (if success_p report then " - all test succeeded" else "")

let batch_str seq_parser strl =
  let report =
    List.fold_left
      (fun ((succ, total), errs) str ->
        match Lazy.force (parse_str str seq_parser) with
          | Some (_, rest) ->
            (match ZL.peek (D.deriv_source rest) with
              | None -> ((succ + 1, total + 1), (str, None) :: errs)
              | Some rest when fst (ZL.t_peek rest) = Token.EOF
                          -> ((succ + 1, total + 1), (str, None) :: errs)
              | Some rest -> ((succ, total + 1),
                              (str, Some ("failed. rest tokens: " ^
                                             ZL.foldr
                                            (fun a b -> Token.type_to_string (fst a) ^ " " ^ b)
                                            ""
                                            (ZL.tree_to_lzlist rest))) :: errs)
            )
          | None   -> ((succ, total + 1), (str, Some "failed.") :: errs))
      empty_result
      strl
  in
  let () = print_string (show_result report) in
  report

let ch_id () = parse_stdin P.test_id
let ch_exp () = parse_stdin P.test_exp
let ch_lexp () = parse_stdin P.test_lexp

let ch_type () = parse_stdin P.test_type

let ch_apat () = parse_stdin P.test_apat

let ch_decls () = parse_stdin P.test_decls
let ch_constrs () = parse_stdin D.constrs
let ch_export () = parse_stdin D.export
let ch_exports () = parse_stdin D.exports
let ch_import () = parse_stdin D.import
let ch_impspec () = parse_stdin D.impspec
let ch_body () = parse_stdin D.body

let s_var s = parse_str s D.var

let s_deriving s = parse_str s D.deriving

let s_lexp s = parse_str s P.test_lexp

let s_exp s = parse_str s P.test_exp

let s_rhs s = parse_str s P.test_rhs

let s_opt_where s = parse_str s P.test_opt_where_decls

let s_apat s = parse_str s P.test_apat

let s_funlhs s = parse_str s P.test_funlhs

let s_decl s = parse_str s P.test_decl

let s_topdecl s = parse_str s D.topdecl

let s_type s = parse_str s P.test_type

let s_constrs s = parse_str s D.constrs

let s_exports s = parse_str s D.exports

let s_import s = parse_str s D.import

let s_impspec s = parse_str s D.impspec

let s_impdecls s = parse_str s D.impdecls

let s_topdecls s = parse_str s D.topdecls

let s_body_raw s = parse_str s D.body
let s_body s = parse_str_as_main false s D.body

let s_module_raw s = parse_str s D.module_
let s_module s = parse_str_as_main false s D.module_

let not_qcon s = parse_str s ~!D.qcon
let not_qcon0 () = not_qcon "x"
let not_qcon1 () = not_qcon "Y"

let qcon s = parse_str s D.qcon
let qcon0 () = qcon "x"
let qcon1 () = qcon "Y"

let eof s = parse_str s P.test_eof

let file hs =
  parse_tokens
    (HsUlexer.lazy_list_of_channel (open_in_bin ("../sample/" ^ hs)))
    false
    D.module_

let all_batch () =
  let all =
    [batch_str D.var
        ["foo"; "(<|>)"];
     batch_str D.deriving
       ["deriving Show"; "deriving (Eq, Ord, Num)"];
     batch_str D.braced_fbind_list_1
       ["{ Foo.a = y, Foo.b = z }"];
     batch_str D.aexp_without_lu
       ["x"];
     batch_str (~!D.qcon **> D.aexp_without_lu)
       ["x"];
     batch_str (some D.braced_fbind_list_1)
       ["{ Foo.a = y, Foo.b = z }"];
     batch_str (P.parened (~!D.qcon **> D.aexp_without_lu  **> P.some D.braced_fbind_list_1))
       ["(x { Foo.a = y, Foo.b = z })"];
     batch_str (P.parened D.aexp)
       ["(x { Foo.a = y, Foo.b = z })"];
     batch_str (P.parened D.fexp)
       ["(x { Foo.a = y, Foo.b = z })"];
     batch_str (P.parened D.lexp)
       ["(x { Foo.a = y, Foo.b = z })"];
     batch_str (P.parened D.infixexp)
       ["(x { Foo.a = y, Foo.b = z })"];
     batch_str (P.parened D.exp)
       ["(x { Foo.a = y, Foo.b = z })"];
     batch_str D.aexp_without_lu
       ["(x { Foo.a = y, Foo.b = z })"];
     batch_str D.aexp
       ["(x { Foo.a = y, Foo.b = z })"];
     batch_str P.test_lexp
       [];
     batch_str P.test_exp
       ["read `extR` (id :: String -> String) `extR` chShow";
        "\\ (1:xs,'a':ys) -> (xs, ys)";
        "[1, 3 .. 11]";
        "[(x, y) | x <- [1,2,3], y <- ['a','b'] ]";
        "x { Foo.a = y, Foo.b = z } { Foo.c = p, Foo.d = q }";
        "(x { Foo.a = y, Foo.b = z })";
        "(x)";
        "(x { a = b })";
        "do { let { x = 1; y = 2}; 1 }";
        "p :: (Eq (f b), Functor f) => (a -> b) -> f a -> f b -> Bool"];
     batch_str P.test_rhs
       ["= read `extR` (id :: String -> String) `extR` chShow where { chShow [x] = x; chShow x = read x }"];
     batch_str P.test_opt_where_decls
       ["where { chShow [x] = x; chShow x = read x }"];
     batch_str P.test_apat
       ["[x]"; "((x, y), z)"];
     batch_str P.test_funlhs
       ["g [x]"];
     batch_str D.pat
       ["x:&xs"; "Foo x y"];
     batch_str P.test_decl
       ["g [x] = x";
        "chShow [x] = x";
        "chShow x = read x"];
     batch_str P.test_type
       ["(Either String a, b -> c)"];
     batch_str D.constrs
       ["Foo !a b !c | Bar"];
     batch_str D.exports
       ["( foo, Foo(..), Bar, Foo'(foo', bar'), module Bar, )"];
     batch_str D.import
       ["(<|>)"; "Foo (..)"; "Foo (bar, bar0)"];
     batch_str D.impspec
       ["( foo, Foo(..), Bar, Foo'(foo', bar'), ) ";
        "hiding ( foo, Foo(..), Bar, Foo'(foo', bar'), ) ";
        "hiding (foo)"; "hiding ()";
        "hiding ((<|>))"; "hiding ( many )";
        "hiding ((<|>), many, State, label)"];
     batch_str D.impdecl
       ["import qualified FooBar as BarFoo ( foo, Foo(..), Bar, Foo'(foo', bar'), ) ";
        "import FooBar ( foo, Foo(..), Bar, Foo'(foo', bar'), ) ";
        "import Text.Parsec hiding ((<|>), many, State, label)"];
     batch_str D.impdecls
       ["import qualified FooBar as BarFoo ( foo, Foo(..), Bar, Foo'(foo', bar'), )";
        "import Foo0"; "import Foo0 ; import Foo1"];
     batch_str D.topdecls
       ["foo 0 0 = 1 ; bar x = \\ y -> x + y";
        ""; "foo 0 0 = 1"; "foo 0 0 = 1 ; bar 1 1 = 2"];
     batch_str D.body
       ["{ import qualified FooBar as BarFoo ( foo, Foo(..), Bar, Foo'(foo', bar'), ) ; foo 0 0 = 1 ; foo x = \\ y -> x + y }";
        "{ }"; "{ import Foo0 }"; "{ foo = 1 }"; "{ foo 0 0 = 1 }";
        "{ import Foo0 ; import Foo1 }"; "{ import Foo0 ; foo 0 0 = 1 }"; "{ foo 0 0 = 1 ; bar 1 1 = 2 }";
        "{ foo 0 0 = 1 ; bar x = \\ y -> x + y }"; "{ import Foo0 ; import Foo1 ; foo 0 0 = 1 }"; "{ import Foo0 ; foo 0 0 = 1 ; bar 1 1 = 2 }";
        "{ foo 0 0 = 1 ; foo x = \\ y -> x + y ; main = print (foo 0 0) }"];
     batch_str D.module_
       ["{ foo = 1 }"]
    ]
  in
  let result = List.fold_left
    (fun res rep -> res && success_p rep)
    true all
  in print_endline
  (if result then "All test succeeded."
   else "Some test failed.")
