
(* module Combinator = Simple.Combinator *)
module Combinator = Simple.DebugCombinator
open Combinator

module P = HsParser
module LO = HsLayout
module ZL = LazyList

let do_parse lzl raw seq_parser =
  (* let run zl = (run seq_parser zl, zl) in *)
  let run zl = run seq_parser zl in
  run
    (if raw then ZL.return (ZL.tree_of_lzlist lzl)
     else let lzl = LO.layout lzl in
          let _ = LO.show_out lzl in
          lzl)

let parse_as_main lzl raw seq_parser =
  let _ = HsParserState.begin_parse_module
    (Symbol.intern "Main")
  in do_parse lzl raw seq_parser

let parse_stdin_as_main raw seq_parser =
  parse_as_main
    (HsUlexer.lazy_list_of_channel stdin)
    raw seq_parser

let parse_raw_as_main seq_parser =
  parse_stdin_as_main true seq_parser

let parse_str_as_main raw str seq_parser =
  print_endline ("input: " ^ str);
  parse_as_main (HsUlexer.lazy_list_of_string str) raw seq_parser

let parse_str_raw_as_main str seq_parser =
  parse_str_as_main true str seq_parser

let ch_id () = parse_raw_as_main P.test_id
let ch_exp () = parse_raw_as_main P.test_exp
let ch_lexp () = parse_raw_as_main P.test_lexp

let ch_type () = parse_raw_as_main P.test_type

let ch_apat () = parse_raw_as_main P.test_apat

let ch_decls () = parse_raw_as_main P.test_decls
let ch_constrs () = parse_raw_as_main P.constrs
let ch_export () = parse_raw_as_main P.export
let ch_exports () = parse_raw_as_main P.exports
let ch_import () = parse_raw_as_main P.import
let ch_impspec () = parse_raw_as_main P.impspec
let ch_decls_cont () = parse_raw_as_main P.test_decls_cont
let ch_body () = parse_raw_as_main P.body

let s_var s = parse_str_raw_as_main s P.var
let s_var0 () = s_var "foo"
let s_var1 () = s_var "(<|>)"

let s_deriving0 () = parse_str_raw_as_main "deriving Show" P.deriving
let s_deriving1 () = parse_str_raw_as_main "deriving (Eq, Ord, Num)" P.deriving

let s_lexp s = parse_str_raw_as_main s P.test_lexp

let s_exp s = parse_str_raw_as_main s P.test_exp

let s_exp0 () = s_exp "read `extR` (id :: String -> String) `extR` chShow"
let s_exp_lambda () = s_exp "\\ (1:xs,'a':ys) -> (xs, ys)"
let s_exp_aseq  () = s_exp "[1, 3 .. 11]"
let s_exp_lcomp  () = s_exp "[(x, y) | x <- [1,2,3], y <- ['a','b'] ]"
let s_exp_lbl_upd () = s_exp "x { Foo.a = y, Foo.b = z } { Foo.c = p, Foo.d = q }"
let s_exp_do () = s_exp "do { let { x = 1; y = 2}; 1 }"
let s_exp_t_class () = s_exp "p :: (Eq (f b), Functor f) => (a -> b) -> f a -> f b -> Bool"

let s_rhs s = parse_str_raw_as_main s P.test_rhs

let s_rhs0 () = s_rhs "= read `extR` (id :: String -> String) `extR` chShow where { chShow [x] = x; chShow x = read x }"

let s_opt_where s = parse_str_raw_as_main s P.test_opt_where_decls

let s_opt_where0 () = s_opt_where
  "where { chShow [x] = x; chShow x = read x }"

let s_decl s = parse_str_raw_as_main s P.test_decl

let s_topdecl s = parse_str_raw_as_main s P.topdecl

let s_type_fun () = parse_str_raw_as_main "(Either String a, b -> c)" P.test_type

let s_constrs () = parse_str_raw_as_main "Foo !a b !c | Bar" P.constrs
let s_exports () = parse_str_raw_as_main "( foo, Foo(..), Bar, Foo'(foo', bar'), module Bar, )" P.exports

let s_import s = parse_str_raw_as_main s P.import
let s_import0 () = s_import "(<|>)"
let s_import1 () = s_import "(Foo (..))"
let s_import2 () = s_import "(Foo (bar, bar0))"

let s_impspec s = parse_str_raw_as_main s P.impspec

let s_impspec0 () = s_impspec "( foo, Foo(..), Bar, Foo'(foo', bar'), ) "
let s_impspec_h0 () = s_impspec "hiding ( foo, Foo(..), Bar, Foo'(foo', bar'), ) "
let s_impspec_h_ex () = s_impspec "hiding (foo)"
let s_impspec_h1 () = s_impspec "hiding ()"
let s_impspec_h2 () = s_impspec "hiding ((<|>))"
let s_impspec_h3 () = s_impspec "hiding ( many )"
let s_impspec_h4 () = s_impspec "hiding ((<|>), many, State, label)"

let s_impdecl () = parse_str_raw_as_main "import qualified FooBar as BarFoo ( foo, Foo(..), Bar, Foo'(foo', bar'), ) " P.impdecl
let s_impdecl0 () = parse_str_raw_as_main "import FooBar ( foo, Foo(..), Bar, Foo'(foo', bar'), ) " P.impdecl
let s_impdecl1 () = parse_str_raw_as_main "import Text.Parsec hiding ((<|>), many, State, label)" P.impdecl

let s_impdecls s = parse_str_raw_as_main s P.impdecls

let s_impdecls_ex () = s_impdecls "import qualified FooBar as BarFoo ( foo, Foo(..), Bar, Foo'(foo', bar'), )"
let s_impdecls1 () = s_impdecls "import Foo0"
let s_impdecls2 () = s_impdecls "import Foo0 ; import Foo1"

let s_topdecls s = parse_str_raw_as_main s P.topdecls

let s_topdecls_ex () = s_topdecls "foo 0 0 = 1 ; bar x = \\ y -> x + y"
let s_topdecls0 () = s_topdecls ""
let s_topdecls1 () = s_topdecls "foo 0 0 = 1"
let s_topdecls2 () = s_topdecls "foo 0 0 = 1 ; bar 1 1 = 2"

let s_impdecls_semi () = parse_str_raw_as_main "import qualified FooBar as BarFoo ( foo, Foo(..), Bar, Foo'(foo', bar'), ) ; foo 0 0 = 1 ; foo x = \\ y -> x + y" P.test_impdecls_semi

let s_imptop s = parse_str_raw_as_main s P.test_imptop

let s_imptop0 () = s_imptop ""
let s_imptop10 () = s_imptop "import Foo0"
let s_imptop01 () = s_imptop "foo 0 0 = 1"
let s_imptop20 () = s_imptop "import Foo0 ; import Foo1"
let s_imptop11 () = s_imptop "import Foo0 ; foo 0 0 = 1"
let s_imptop02 () = s_imptop "foo 0 0 = 1 ; bar 1 1 = 2"
let s_imptop21 () = s_imptop "import Foo0 ; import Foo1 ; foo 0 0 = 1"
let s_imptop12 () = s_imptop "import Foo0 ; foo 0 0 = 1 ; bar 1 1 = 2"

let s_lbr_imptop s = parse_str_raw_as_main s P.test_lbr_imptop

let s_lbr_imptop0 () = s_lbr_imptop "{ "
let s_lbr_imptop10 () = s_lbr_imptop "{ import Foo0"
let s_lbr_imptop01 () = s_lbr_imptop "{ foo 0 0 = 1"
let s_lbr_imptop20 () = s_lbr_imptop "{ import Foo0 ; import Foo1"
let s_lbr_imptop11 () = s_lbr_imptop "{ import Foo0 ; foo 0 0 = 1"
let s_lbr_imptop02 () = s_lbr_imptop "{ foo 0 0 = 1 ; bar 1 1 = 2"
let s_lbr_imptop21 () = s_lbr_imptop "{ import Foo0 ; import Foo1 ; foo 0 0 = 1"
let s_lbr_imptop12 () = s_lbr_imptop "{ import Foo0 ; foo 0 0 = 1 ; bar 1 1 = 2"

let s_lbr_imptop_rbr s = parse_str_raw_as_main s P.test_lbr_imptop_rbr

let s_lbr_imptop_rbr0 () = s_lbr_imptop_rbr "{ }"
let s_lbr_imptop_rbr10 () = s_lbr_imptop_rbr "{ import Foo0 }"
let s_lbr_imptop_rbr01 () = s_lbr_imptop_rbr "{ foo 0 0 = 1 }"
let s_lbr_imptop_rbr20 () = s_lbr_imptop_rbr "{ import Foo0 ; import Foo1 }"
let s_lbr_imptop_rbr11 () = s_lbr_imptop_rbr "{ import Foo0 ; foo 0 0 = 1 }"
let s_lbr_imptop_rbr02 () = s_lbr_imptop_rbr "{ foo 0 0 = 1 ; bar 1 1 = 2 }"
let s_lbr_imptop_rbr21 () = s_lbr_imptop_rbr "{ import Foo0 ; import Foo1 ; foo 0 0 = 1 }"
let s_lbr_imptop_rbr12 () = s_lbr_imptop_rbr "{ import Foo0 ; foo 0 0 = 1 ; bar 1 1 = 2 }"

let s_body_raw s = parse_str_raw_as_main s P.body
let s_body s = parse_str_as_main false s P.body

let s_body_ex () = s_body "{ import qualified FooBar as BarFoo ( foo, Foo(..), Bar, Foo'(foo', bar'), ) ; foo 0 0 = 1 ; foo x = \\ y -> x + y }"

let s_body0 () = s_body "{ }"
let s_body10 () = s_body "{ import Foo0 }"
(* let s_body01 () = s_body "{ foo = 1 }" *)
let s_body01 () = s_body "foo = 1"
(* let s_body01 () = s_body "{ foo 0 0 = 1 }" *)
let s_body20 () = s_body "{ import Foo0 ; import Foo1 }"
let s_body11 () = s_body "{ import Foo0 ; foo 0 0 = 1 }"
let s_body02 () = s_body "{ foo 0 0 = 1 ; bar 1 1 = 2 }"
let s_body02' () = s_body "{ foo 0 0 = 1 ; bar x = \\ y -> x + y }"
let s_body21 () = s_body "{ import Foo0 ; import Foo1 ; foo 0 0 = 1 }"
let s_body12 () = s_body "{ import Foo0 ; foo 0 0 = 1 ; bar 1 1 = 2 }"

let s_body6 () = s_body "{ foo 0 0 = 1 ; foo x = \\ y -> x + y ; main = print (foo 0 0) }"

let s_module_raw s = parse_str_raw_as_main s P.module_
let s_module s = parse_str_as_main false s P.module_

let s_module_raw01 () = s_module_raw "{ foo = 1 }"

let s_module0 () = s_module ""
let s_module1 () = s_module "foo = 1"
(* let s_module1 () = s_module "foo 0 0 = 1\n" *)
let s_module2 () = s_module "foo 0 0 = 1\nfoo x = \\ y -> x + y\n"

(*let s_any s = parse_str_as_main false s P.test_any
let s_any1 () = s_any "foo = 1"*)

let s_anys s = parse_str_as_main false s P.test_anys
let s_anys1 () = s_anys "foo = 1"

let file hs =
  do_parse
    (HsUlexer.lazy_list_of_channel (open_in_bin ("../sample/" ^ hs)))
    false
    P.module_
