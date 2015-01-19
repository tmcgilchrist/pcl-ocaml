module type S =
sig 

  (* a traditional lisp sexp syntax
   * num = float | int
   * atom = num | symbol
   * sexp = atom | sexp list
   * symbols may not begin with +,-, or #, but they are valid elsewhere
   *)

  type num = Real of float | Int of int
    
  type atom = Num of num  | Symbol of string

  type sexp = Atom of atom | List of sexp list
    
  val sexp_parser : ('st,sexp) CharParse.CharPrim.parser

  val parse_sexp_file : string -> sexp list option

end


module M : S = 
struct

  type num = Real of float | Int of int
    
  type atom = Num of num  | Symbol of string
    
  type sexp = Atom of atom | List of sexp list
    
  module L = Language.M(Language.SExpLD)
  open L
  open CharParse.CharPrim
  open CharParse.CharComb
  
  
  let p_num st =
  (   (integerOrFloat
   >>= fun nf ->
         return (match nf with Nat n -> Int n | Float f -> Real f))
  <?> "num") st
        
  let p_atom st =
  (   (p_num >>= fun n -> return (Num n))  
      (* I've arbitrarily decided that a symbol can be an identifier or a
       * string *)
  <|> (   (identifier >>= fun i -> return (Symbol i))
      <|> (stringLiteral >>= fun s -> return (Symbol ("\"" ^ s ^ "\""))))
  <?> "atom") st

  let rec p_sexp st =
  (   (p_atom >>= fun a -> return (Atom a))
  <|> (parens (many p_sexp) >>= fun l -> return (List l)) 
  <?> "sexp") st

  let sexp_parser = p_sexp

  let parse_sexp_file name =
    let stream = LazyList.M.ofChannel (open_in name) in
    let file_parse = 
      whiteSpace >> (many1 sexp_parser) >>= fun l -> eof >> return l
    in
      match parse name stream file_parse with
          Success x -> Some x
        | Failure err -> print_string (Error.M.errorToString err); None

end
