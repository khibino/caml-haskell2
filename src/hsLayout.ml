
module TK = Token

open LazyList

let input_of_L lex_lzl =
  let rec scan_rec = function
    | lazy (Cons (((TK.K_LET(_) | TK.K_WHERE(_) | TK.K_DO(_) | TK.K_OF(_)), _) as car,
                  (lazy (Cons ((want_lbr, region), _)) as cdr))) ->
      begin
        match want_lbr with
          | TK.SP_LEFT_BRACE(_) -> lazy (Cons (car, (scan_rec cdr)))
          | not_lbr
            -> lazy (Cons (car, (((TK.BLK_OPEN (if not_lbr == TK.EOF then 0 else TK.offset region)),
                                  region) ^: scan_rec cdr)))
      end
    | lazy (Cons ((tk0, reg0) as car, (lazy (Cons ((tk1, reg1), _)) as cdr)))
        when TK.other_line_p reg0 reg1
          -> lazy (Cons (car, ((TK.BLK_LEVEL (TK.offset reg1), reg1) ^: scan_rec cdr)))

    | lazy (Cons (car, cdr))           -> lazy (Cons (car, scan_rec cdr))
    | lazy Nil as lzl                  -> lzl
  in
  let scan = function
    | lazy (Cons (((TK.SP_LEFT_BRACE | TK.K_MODULE), _), _)) as lzl -> scan_rec lzl
    | lazy (Cons ((_, region), _)) as lzl
      -> scan_rec (cons (TK.BLK_OPEN (TK.offset region), region) lzl)
    | lazy Nil as lzl -> lzl
  in
  scan lex_lzl
