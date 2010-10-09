
module LO = HsLayout

let do_chan_input chan_in =
  List.map (fun e -> Token.type_to_string (fst e))
    (LazyList.to_list
       (LO.input_of_L (HsUlexer.lazy_list_of_channel chan_in)))

let do_input () = do_chan_input stdin

let do_chan_out chan_in =
  LO.show_out
    (LO.lazy_L
       (LO.input_of_L (HsUlexer.lazy_list_of_channel chan_in)))

let do_out () = do_chan_out stdin

let file hs = do_chan_out (open_in_bin ("../sample/" ^ hs))
