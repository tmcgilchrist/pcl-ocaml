module type S =
sig 

  type sexp = Atom of string | List of sexp list
    
  val sexp_parser : ('st,sexp) CharParse.CharPrim.parser

  val parse_sexp_file : string -> sexp list option

end


module M : S = 
struct

  open CharParse.M
  open CharParse.CharPrim
  open CharParse.CharComb

  type sexp = Atom of string | List of sexp list

  let rec p_sexp st =
  (   ((many1 alphaNum) >>= fun a -> return (Atom (implode a)))
  <|> ((between (char '(') (char ')') (sepBy p_sexp spaces))
        >>= fun l -> return (List l))
  <?> "sexp") st

  let sexp_parser = p_sexp

  let parse_sexp_file name =
    let stream = LazyList.M.ofChannel (open_in name) in
    let file_parse = 
      spaces >> (sepEndBy sexp_parser spaces) >>= fun l -> eof >> return l
    in
      match parse name stream file_parse with
          Success x -> Some x
        | Failure err -> print_string (Error.M.errorToString err); None

end
