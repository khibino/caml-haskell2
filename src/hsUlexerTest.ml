

let do_test () =
  let rec do_test context rv =
    let ((tk, _) as  tkwl, context) = HsUlexer.do_lex context in
    if tk == Token.EOF then List.rev rv
    else do_test context (tkwl :: rv)
  in do_test (HsUlexer.make_context stdin) []



