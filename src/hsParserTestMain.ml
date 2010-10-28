
(* let _ = T.P.parse_chan stdin HsParser.test_exp *)

let main () = HsParserTest.parse_str_as_main false "foo = 1" HsParser.module_

let _ = main ()
