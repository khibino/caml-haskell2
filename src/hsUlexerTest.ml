
let do_test chan_in =  LazyList.to_list (HsUlexer.lazy_list_of_channel chan_in)

let do_stdin () = do_test stdin

let file hs = do_test (open_in_bin ("../sample/" ^ hs))
