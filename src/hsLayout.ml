
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
        when TK.other_line_p reg0 reg1 && tk1 != TK.EOF
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


let lazy_L : 'a t -> 'a forest_t =

  (* let rec lazy_L : (('a forest_t * int list) -> 'a forest_t) (f, ms) = *)
  let rec lazy_L (tfo, ms) =
    let rec_cons tk ts ms =
      Node (tk, lazy_L (ts, ms))
    in

    let rec_L (ta, ma as pair) = match pair with
      | (lazy (Node ((TK.BLK_LEVEL n, reg), ts)),      m :: _)
          when m = n  -> return (lazy (rec_cons (TK.SP_SEMI, reg) ts ma))
      | (lazy (Node ((TK.BLK_LEVEL n, reg),  _)),      m :: ms)
          when n < m  -> return (lazy (rec_cons (TK.SP_RIGHT_BRACE, reg) (return ta) ms))
      | (lazy (Node ((TK.BLK_LEVEL n, reg), ts)),      ms)
        ->               lazy_L (ts, ms)

      | (lazy (Node ((TK.BLK_OPEN n, reg), ts)),       m :: _)
          when n > m  -> return (lazy (rec_cons (TK.SP_LEFT_BRACE, reg) ts (n :: ma)))
      | (lazy (Node ((TK.BLK_OPEN n, reg), ts)),       [])
          when n > 0  -> return (lazy (rec_cons (TK.SP_LEFT_BRACE, reg) ts [n]))
      | (lazy (Node ((TK.BLK_OPEN n, reg), ts)),       ms)
        ->               return (lazy (Node
                                         ((TK.SP_LEFT_BRACE, reg),
                                          return (Lazy.lazy_from_val
                                                    (rec_cons
                                                       (TK.SP_RIGHT_BRACE, reg)
                                                       (return (t_cons (TK.BLK_LEVEL n, reg) ts)) ms)))))

      | (lazy (Node ((TK.SP_RIGHT_BRACE, _ as rb), ts)), 0 :: ms)
        ->               return (lazy (rec_cons rb ts ms))
      | (lazy (Node ((TK.SP_RIGHT_BRACE, _), _)), _)
        ->               failwith "parse error. - layout - Note 3"

      | (lazy (Node ((TK.SP_LEFT_BRACE, _ as lb), ts)), ms)
        ->               return (lazy (rec_cons lb ts (0 :: ms)))


      | (lazy (Node ((t, reg) as tk, ts)),             m :: ms)
          when (m != 0 && t != TK.EOF)
            ->           of_list [lazy (rec_cons tk ts ma) ;
                                  lazy (rec_cons (TK.SP_RIGHT_BRACE, reg) (return ta) ms)]

      | (lazy (Node ((t, reg) as tk, ts)),             ms)
          when (t != TK.EOF)
            ->           return (lazy (rec_cons tk ts ms))

      | (lazy (Node ((TK.EOF, reg), (lazy Nil))),      [])
        ->               return (Lazy.lazy_from_val (Node ((TK.EOF, reg), nil)))

      | (lazy (Node ((TK.EOF, reg), ts)),              m :: ms)
        -> if m == 0 then failwith "parse error. - layout - Note 6"
          else           return (lazy (rec_cons (TK.SP_RIGHT_BRACE, reg) (return ta) ms))

      | (lazy (Node ((tk, _), _)),                     _)
        ->               failwith "parse error. - layout - Invalid input pattern."
    in
    foldr (fun lz res -> rec_L (lz, ms) ++ res) nil tfo
    (*List.rev
      (List.fold_left (fun res lz -> List.rev_append (rec_L (lz, ms)) res) [] f)*)

  in
  (fun lz ->
    let lout = lazy_L (return (tree_of_lzlist lz), []) in
    match to_list lout with
      | [_] -> lout
      | _   -> failwith "parse error. - layout - Wrong input token list." )

let show_out lzl = show_token_forest TK.to_string lzl

let layout lzl = lazy_L (input_of_L lzl)
