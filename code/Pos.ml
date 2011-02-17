module type S =
sig

  (** The definition of the POS signature *)
  
  type pos = (string*int*int)
    
  (** create an initial pos given a file name *)
  val init_pos  : string -> pos
    
  (** given a pos, filename returns the file name *)
  val filename  : pos -> string
    
  (** line p returns the line number of the pos p *)
  val line      : pos -> int
    
  (** column p returns the column number of pos p *)
  val column    : pos -> int

  val linecol   : pos -> int*int
    
  val incLine   : pos -> int -> pos

  val incColumn : pos -> int -> pos

  val incLC     : pos -> int*int -> pos

  val setName   : pos -> string -> pos

  val setLine   : pos -> int -> pos

  val setColumn : pos -> int -> pos

  val setLC     : pos -> int*int -> pos

  val toString  : pos -> string

  (* functions specifically for string parsers *)

  val updatePosChar   : pos -> char -> pos

  val updatePosChars  : pos -> char list -> pos

  val updatePosString : pos -> string -> pos
end




module M : S =
struct
  
  type pos = string*int*int (* filename, row, column *)
    
  
  let init_pos nm = (nm,1,0)
    
  let filename (nm,_,_) = nm
    
  let line (_,l,_) = l
    
  let column (_,_,c) = c

  let linecol (_,l,c) = (l,c)
    
  let incLine (n,l,c) k = (n,l+k,c)

  let incColumn (n,l,c) k = (n,l,c+k)

  let incLC (n,l,c) (kl,kc) = (n,l+kl,c+kc)

  let setName (_,l,c) n = (n,l,c)

  let setLine (n,_,c) l = (n,l,c)

  let setColumn (n,l,_) c = (n,l,c)

  let setLC (n,_,_) (kl,kc) = (n,kl,kc)

  let toString (n,l,c) =
    let showLineColumn = "(line " ^ string_of_int l ^
                         ", column " ^ string_of_int c ^
                         ")" 
    in
      match n with
          "" -> showLineColumn
        | _ -> "\"" ^ n ^ "\" " ^ showLineColumn

  (* functions specifically for string parsers *)

  let updatePosChar (n,l,c) ch =
    match ch with
        '\n' -> (n,l+1,0)
      | '\t' -> (n,l,c + 8 - ((c - 1) mod 8))
      | _    -> (n,l,c+1)

  let updatePosChars = List.fold_left updatePosChar

  let updatePosString pos str =
    (* this is gross, it would be nice if ocaml had folding on strings... *)
    let rec scan n p =
      if n < 0 then p
      else scan (n-1) (updatePosChar p (String.get str n)) 
    in
      scan ((String.length str) - 1) pos
end
