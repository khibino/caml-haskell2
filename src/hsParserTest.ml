
module Combinator = Simple.Combinator(Token)
(* module Combinator = Simple.DebugCombinator(Token) *)
open Combinator

module P = HsParser
module LO = HsLayout
module ZL = LazyList

let debug = false

let parse_tokens lzl raw seq_parser =
  (* let run zl = (run seq_parser zl, zl) in *)
  let parse_tokens zl = run seq_parser zl in
  parse_tokens
    (if raw then ZL.return (ZL.tree_of_lzlist lzl)
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
let show_result : test_result -> string =
  fun ((succ,total), info) ->
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
    in msg ^ F.sprintf ". %d/%d - success rate\n" succ total

let batch_str seq_parser strl =
  let report =
    List.fold_left
      (fun ((succ, total), errs) str ->
        match parse_str str seq_parser with
          | Some (_, rest) ->
            (match ZL.peek rest with
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
  in print_string (show_result report)

let ch_id () = parse_stdin P.test_id
let ch_exp () = parse_stdin P.test_exp
let ch_lexp () = parse_stdin P.test_lexp

let ch_type () = parse_stdin P.test_type

let ch_apat () = parse_stdin P.test_apat

let ch_decls () = parse_stdin P.test_decls
let ch_constrs () = parse_stdin P.constrs
let ch_export () = parse_stdin P.export
let ch_exports () = parse_stdin P.exports
let ch_import () = parse_stdin P.import
let ch_impspec () = parse_stdin P.impspec
let ch_decls_cont () = parse_stdin P.test_decls_cont
let ch_body () = parse_stdin P.body

let s_var s = parse_str s P.var
let s_var0 () = s_var "foo"
let s_var1 () = s_var "(<|>)"

let s_deriving0 () = parse_str "deriving Show" P.deriving
let s_deriving1 () = parse_str "deriving (Eq, Ord, Num)" P.deriving

let s_lexp s = parse_str s P.test_lexp

let s_exp s = parse_str s P.test_exp

let s_exp0 () = s_exp "read `extR` (id :: String -> String) `extR` chShow"
let s_exp_lambda () = s_exp "\\ (1:xs,'a':ys) -> (xs, ys)"
let s_exp_aseq  () = s_exp "[1, 3 .. 11]"
let s_exp_lcomp  () = s_exp "[(x, y) | x <- [1,2,3], y <- ['a','b'] ]"
let s_exp_lbl_upd () = s_exp "x { Foo.a = y, Foo.b = z } { Foo.c = p, Foo.d = q }"
let s_exp_do () = s_exp "do { let { x = 1; y = 2}; 1 }"
let s_exp_t_class () = s_exp "p :: (Eq (f b), Functor f) => (a -> b) -> f a -> f b -> Bool"

let s_rhs s = parse_str s P.test_rhs

let s_rhs0 () = s_rhs "= read `extR` (id :: String -> String) `extR` chShow where { chShow [x] = x; chShow x = read x }"

let s_opt_where s = parse_str s P.test_opt_where_decls

let s_opt_where0 () = s_opt_where
  "where { chShow [x] = x; chShow x = read x }"

let s_apat s = parse_str s P.test_apat

let s_apat0 () = s_apat "[x]"

let s_funlhs s = parse_str s P.test_funlhs

let s_funlhs0 () = s_funlhs "g [x]"

let s_decl s = parse_str s P.test_decl

let s_decl0 () = s_decl "g [x] = x"
let s_decl1 () = s_decl "chShow [x] = x"
let s_decl2 () = s_decl "chShow x = read x"

let s_topdecl s = parse_str s P.topdecl

let s_type_fun () = parse_str "(Either String a, b -> c)" P.test_type

let s_constrs () = parse_str "Foo !a b !c | Bar" P.constrs
let s_exports () = parse_str "( foo, Foo(..), Bar, Foo'(foo', bar'), module Bar, )" P.exports

let s_import s = parse_str s P.import
let s_import0 () = s_import "(<|>)"
let s_import1 () = s_import "Foo (..)"
let s_import2 () = s_import "Foo (bar, bar0)"

let s_impspec s = parse_str s P.impspec

let s_impspec0 () = s_impspec "( foo, Foo(..), Bar, Foo'(foo', bar'), ) "
let s_impspec_h0 () = s_impspec "hiding ( foo, Foo(..), Bar, Foo'(foo', bar'), ) "
let s_impspec_h_ex () = s_impspec "hiding (foo)"
let s_impspec_h1 () = s_impspec "hiding ()"
let s_impspec_h2 () = s_impspec "hiding ((<|>))"
let s_impspec_h3 () = s_impspec "hiding ( many )"
let s_impspec_h4 () = s_impspec "hiding ((<|>), many, State, label)"

let s_impdecl () = parse_str "import qualified FooBar as BarFoo ( foo, Foo(..), Bar, Foo'(foo', bar'), ) " P.impdecl
let s_impdecl0 () = parse_str "import FooBar ( foo, Foo(..), Bar, Foo'(foo', bar'), ) " P.impdecl
let s_impdecl1 () = parse_str "import Text.Parsec hiding ((<|>), many, State, label)" P.impdecl

let s_impdecls s = parse_str s P.impdecls

let s_impdecls_ex () = s_impdecls "import qualified FooBar as BarFoo ( foo, Foo(..), Bar, Foo'(foo', bar'), )"
let s_impdecls1 () = s_impdecls "import Foo0"
let s_impdecls2 () = s_impdecls "import Foo0 ; import Foo1"

let s_topdecls s = parse_str s P.topdecls

let s_topdecls_ex () = s_topdecls "foo 0 0 = 1 ; bar x = \\ y -> x + y"
let s_topdecls0 () = s_topdecls ""
let s_topdecls1 () = s_topdecls "foo 0 0 = 1"
let s_topdecls2 () = s_topdecls "foo 0 0 = 1 ; bar 1 1 = 2"

let s_impdecls_semi () = parse_str "import qualified FooBar as BarFoo ( foo, Foo(..), Bar, Foo'(foo', bar'), ) ; foo 0 0 = 1 ; foo x = \\ y -> x + y" P.test_impdecls_semi

let s_imptop s = parse_str s P.test_imptop

let s_imptop0 () = s_imptop ""
let s_imptop10 () = s_imptop "import Foo0"
let s_imptop01 () = s_imptop "foo 0 0 = 1"
let s_imptop20 () = s_imptop "import Foo0 ; import Foo1"
let s_imptop11 () = s_imptop "import Foo0 ; foo 0 0 = 1"
let s_imptop02 () = s_imptop "foo 0 0 = 1 ; bar 1 1 = 2"
let s_imptop21 () = s_imptop "import Foo0 ; import Foo1 ; foo 0 0 = 1"
let s_imptop12 () = s_imptop "import Foo0 ; foo 0 0 = 1 ; bar 1 1 = 2"

let s_lbr_imptop s = parse_str s P.test_lbr_imptop

let s_lbr_imptop0 () = s_lbr_imptop "{ "
let s_lbr_imptop10 () = s_lbr_imptop "{ import Foo0"
let s_lbr_imptop01 () = s_lbr_imptop "{ foo 0 0 = 1"
let s_lbr_imptop20 () = s_lbr_imptop "{ import Foo0 ; import Foo1"
let s_lbr_imptop11 () = s_lbr_imptop "{ import Foo0 ; foo 0 0 = 1"
let s_lbr_imptop02 () = s_lbr_imptop "{ foo 0 0 = 1 ; bar 1 1 = 2"
let s_lbr_imptop21 () = s_lbr_imptop "{ import Foo0 ; import Foo1 ; foo 0 0 = 1"
let s_lbr_imptop12 () = s_lbr_imptop "{ import Foo0 ; foo 0 0 = 1 ; bar 1 1 = 2"

let s_lbr_imptop_rbr s = parse_str s P.test_lbr_imptop_rbr

let s_lbr_imptop_rbr0 () = s_lbr_imptop_rbr "{ }"
let s_lbr_imptop_rbr10 () = s_lbr_imptop_rbr "{ import Foo0 }"
let s_lbr_imptop_rbr01 () = s_lbr_imptop_rbr "{ foo 0 0 = 1 }"
let s_lbr_imptop_rbr20 () = s_lbr_imptop_rbr "{ import Foo0 ; import Foo1 }"
let s_lbr_imptop_rbr11 () = s_lbr_imptop_rbr "{ import Foo0 ; foo 0 0 = 1 }"
let s_lbr_imptop_rbr02 () = s_lbr_imptop_rbr "{ foo 0 0 = 1 ; bar 1 1 = 2 }"
let s_lbr_imptop_rbr21 () = s_lbr_imptop_rbr "{ import Foo0 ; import Foo1 ; foo 0 0 = 1 }"
let s_lbr_imptop_rbr12 () = s_lbr_imptop_rbr "{ import Foo0 ; foo 0 0 = 1 ; bar 1 1 = 2 }"

let s_body_raw s = parse_str s P.body
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

let s_module_raw s = parse_str s P.module_
let s_module s = parse_str_as_main false s P.module_

let s_module_raw01 () = s_module_raw "{ foo = 1 }"

let s_module0 () = s_module ""
let s_module1 () = s_module "foo = 1"
let s_module2 () = s_module "foo 0 0 = 1\nfoo x = \\ y -> x + y\n"

let s_anys s = parse_str_as_main false s P.test_anys
let s_anys1 () = s_anys "foo = 1"

let not_qcon s = parse_str s ~!P.qcon
let not_qcon0 () = not_qcon "x"
let not_qcon1 () = not_qcon "Y"

let qcon s = parse_str s P.qcon
let qcon0 () = qcon "x"
let qcon1 () = qcon "Y"

let file hs =
  parse_tokens
    (HsUlexer.lazy_list_of_channel (open_in_bin ("../sample/" ^ hs)))
    false
    P.module_

let all_batch () =
  batch_str P.var
    ["foo"; "(<|>)"];
  batch_str P.deriving
    ["deriving Show"; "deriving (Eq, Ord, Num)"];
  batch_str (P.braced_fbind_list_1 ())
    ["{ Foo.a = y, Foo.b = z }"];
  batch_str (~$P.aexp_without_lu)
    ["x"];
  batch_str (~!P.qcon **> ~$P.aexp_without_lu)
    ["x"];
  batch_str (some ~$P.braced_fbind_list_1)
    ["{ Foo.a = y, Foo.b = z }"];
  batch_str (P.parened (~!P.qcon **> ~$P.aexp_without_lu  **> P.some' ~$P.braced_fbind_list_1))
    ["(x { Foo.a = y, Foo.b = z })"];
  batch_str (P.parened (P.aexp ()))
    ["(x { Foo.a = y, Foo.b = z })"];
  batch_str (P.parened (P.fexp ()))
    ["(x { Foo.a = y, Foo.b = z })"];
  batch_str (P.parened (P.lexp ()))
    ["(x { Foo.a = y, Foo.b = z })"];
  batch_str (P.parened (P.infixexp ()))
    ["(x { Foo.a = y, Foo.b = z })"];
  batch_str (P.parened (P.exp ()))
    ["(x { Foo.a = y, Foo.b = z })"];
  batch_str (P.aexp_without_lu ())
    ["(x { Foo.a = y, Foo.b = z })"];
  batch_str (P.aexp ())
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
  batch_str (P.pat ())
    ["x:&xs"; "Foo x y"];
  batch_str P.test_decl
    ["g [x] = x";
     "chShow [x] = x";
     "chShow x = read x"];
  batch_str P.test_type
    ["(Either String a, b -> c)"];
  batch_str P.constrs
    ["Foo !a b !c | Bar"];
  batch_str P.exports
    ["( foo, Foo(..), Bar, Foo'(foo', bar'), module Bar, )"];
  batch_str P.import
    ["(<|>)"; "Foo (..)"; "Foo (bar, bar0)"];
  batch_str P.impspec
    ["( foo, Foo(..), Bar, Foo'(foo', bar'), ) ";
     "hiding ( foo, Foo(..), Bar, Foo'(foo', bar'), ) ";
     "hiding (foo)"; "hiding ()";
     "hiding ((<|>))"; "hiding ( many )";
     "hiding ((<|>), many, State, label)"];
  batch_str P.impdecl
    ["import qualified FooBar as BarFoo ( foo, Foo(..), Bar, Foo'(foo', bar'), ) ";
     "import FooBar ( foo, Foo(..), Bar, Foo'(foo', bar'), ) ";
     "import Text.Parsec hiding ((<|>), many, State, label)"];
  batch_str P.impdecls
    ["import qualified FooBar as BarFoo ( foo, Foo(..), Bar, Foo'(foo', bar'), )";
     "import Foo0"; "import Foo0 ; import Foo1"];
  batch_str P.topdecls
    ["foo 0 0 = 1 ; bar x = \\ y -> x + y";
     ""; "foo 0 0 = 1"; "foo 0 0 = 1 ; bar 1 1 = 2"];
  batch_str P.body
    ["{ import qualified FooBar as BarFoo ( foo, Foo(..), Bar, Foo'(foo', bar'), ) ; foo 0 0 = 1 ; foo x = \\ y -> x + y }";
     "{ }"; "{ import Foo0 }"; "{ foo = 1 }"; "{ foo 0 0 = 1 }";
     "{ import Foo0 ; import Foo1 }"; "{ import Foo0 ; foo 0 0 = 1 }"; "{ foo 0 0 = 1 ; bar 1 1 = 2 }";
     "{ foo 0 0 = 1 ; bar x = \\ y -> x + y }"; "{ import Foo0 ; import Foo1 ; foo 0 0 = 1 }"; "{ import Foo0 ; foo 0 0 = 1 ; bar 1 1 = 2 }";
     "{ foo 0 0 = 1 ; foo x = \\ y -> x + y ; main = print (foo 0 0) }"];
  batch_str P.module_
    ["{ foo = 1 }"];
