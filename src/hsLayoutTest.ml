
let do_chan_input chan_in =
  LazyList.show_token_tree Token.to_string
    (HsLayout.input_of_L (HsUlexer.lazy_list chan_in))

let do_input () = do_chan_input stdin

let do_chan_out chan_in =
  LazyList.show_token_tree Token.to_string
    (HsLayout.lazy_L
       (HsLayout.input_of_L (HsUlexer.lazy_list chan_in)))

let do_out () = do_chan_out stdin
