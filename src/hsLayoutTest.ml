
let do_test chan_in = LazyList.to_list (HsLayout.input_of_L (HsUlexer.lazy_list chan_in))

let do_stdin () = do_test stdin
