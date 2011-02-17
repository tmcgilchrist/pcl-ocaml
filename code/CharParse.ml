
module CharPrim = Prim.M (Token.CharTok)
module CharComb = Combinator.M (CharPrim)
module CharExpr = Expr.M (CharPrim)

module type S =
sig


  (* Type of a character parser with state 'st and return type 'a *)
  type ('st,'a) char_parser = ('st, 'a) CharPrim.parser

  (* These two functions aren't really related to parsers, but are
   * often useful since the library deals with char lists, not strings
   * (at least for now *)
  val explode : string -> char list 
  val implode : char list -> string


  (* Primitive char parsers *)
  val satisfy : (char -> bool) -> ('st,char) char_parser

  val string : string -> ('st,string) char_parser

  val chars  : char list -> ('st,char list) char_parser


  (* char parsers *)
  val oneOf    : char list -> ('st,char) char_parser

  val noneOf   : char list -> ('st,char) char_parser

  val char     : char -> ('st,char) char_parser

  val space    : ('st,char) char_parser

  val newline  : ('st,char) char_parser

  val tab      : ('st,char) char_parser

  val spaces   : ('st,unit) char_parser

  val upper    : ('st,char) char_parser

  val lower    : ('st,char) char_parser

  val alphaNum : ('st,char) char_parser

  val letter   : ('st,char) char_parser

  val digit    : ('st,char) char_parser

  val hexDigit : ('st,char) char_parser

  val octDigit : ('st,char) char_parser

  val anyChar  : ('st,char) char_parser

  val manyString : ('st, char) char_parser -> ('st, string) char_parser

  (* for running a char parser *)
  val parse_string : string -> (unit, 'a) char_parser -> 'a option

end



module M : S = 
struct

open CharPrim

(* useful *)
let explode s =
  let rec exp n acc =
    if n < 0 then acc 
    else exp (n-1) (s.[n]::acc)
  in
    exp ((String.length s)-1) []

let implode cs =
  let str = String.create (List.length cs) in
  let rec imp n = function 
      [] -> ()
    | (c::cs) -> str.[n] <- c; imp (n+1) cs
  in
    imp 0 cs; str



(* Type of a character parser with state 'st and return type 'a *)
type ('st,'a) char_parser = ('st,'a) parser

let satisfy f = token (fun c -> if f c then Some c else None)

let chars s = tokens s

(* XXX this can be done more intelligently - maybe add a token_arr parser
 * to Prim  *)
let string s = tokenCol s String.get (String.length s)


let oneOf cs = satisfy (fun c -> List.mem c cs)

let noneOf cs = satisfy (fun c -> not (List.mem c cs))

let char c = satisfy ((=) c) <?> ("\"" ^ Token.CharTok.toString c ^ "\"")

let space st = 
  let isSpace c =
    (c = ' ') || (c = '\t') || (c = '\n') || 
    (c = '\011') || (c = '\012') || (c = '\r') 
  in
    (satisfy isSpace <?> "space") st

let spaces st = ((skipMany space) <?> "white space") st

let newline st = (char '\n' <?> "newline") st

let tab st = (char '\t' <?> "tab") st

(* some helper functions *)
let isUpper = (fun c -> let n = Char.code c in n >= 65 && n <= 90)

let isLower = (fun c -> let n = Char.code c in n >= 97 && n <= 122)

let isDigit = (fun c -> let n = Char.code c in n >= 48 && n <= 57)

let isHexDigit c = (isDigit c) ||
  (let n = Char.code c in (n >= 65 && n <= 70) ||
                          (n >= 97 && n <= 102))

let isOctDigit = (fun c -> let n = Char.code c in n >= 48 && n <= 55)

let isAlpha c = (isUpper c) || (isLower c)

let isAlphaNum c = (isAlpha c) || (isDigit c)


let upper st    = (satisfy isUpper     <?> "uppercase letter") st

let lower st    = (satisfy isLower     <?> "lowercase letter") st

let alphaNum st = (satisfy isAlphaNum  <?> "letter or digit") st

let letter st   = (satisfy isAlpha     <?> "letter") st

let digit st    = (satisfy isDigit     <?> "digit") st

let hexDigit st = (satisfy isHexDigit  <?> "hexadecimal digit") st

let octDigit st = (satisfy isOctDigit  <?> "octal digit") st

let anyChar st  = (satisfy (fun _ -> true)) st


let manyString p = manyColOp p ~one:false ~make:String.create ~set:String.set
                             ~get:String.get ~length:String.length


let parse_string s p =
  match parse "test" (LazyList.M.ofString s) 
              (p >>= fun r -> CharComb.eof >> return r)
  with
    Success x -> Some x
  | Failure err -> print_string (Error.M.errorToString err); None

end



                           
