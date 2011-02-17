module type LanguageDef =
sig

  val commentStart    : string
  val commentEnd      : string
  val commentLine     : string
  val nestedComments  : bool
  val identStart      : ('st,char) CharParse.M.char_parser
  val identLetter     : ('st,char) CharParse.M.char_parser
  val opStart         : ('st,char) CharParse.M.char_parser
  val opLetter        : ('st,char) CharParse.M.char_parser
  val reservedNames   : string list
  val reservedOpNames : string list
  val caseSensitive   : bool

  (* if the language doesn't have these, they must always fail and
   * consume no input *)
  val negSign : ('st,char) CharParse.M.char_parser
  val posSign : ('st,char) CharParse.M.char_parser

end


(* XXX add some others too *)
module SMLLD : LanguageDef =
struct
  open CharParse.CharPrim
  open CharParse.M

  let commentStart = "(*"
  let commentEnd = "*)"
  let commentLine = ""
  let nestedComments = true
  let identStart = letter (* of course, a leading prime is used in the
                             case of tyvars, but it's easiest to handle
                             these seperately *)
  let identLetter st = (alphaNum <|> (char '\'') <|> (char '_')) st
  let opStart st = oneOf (explode "!%&$#+-/:<=>>@\\~'^|*") st
  let opLetter = opStart
  let reservedNames = (* as per pgs 3,11 of the Def *)
    ["abstype"; "and"; "andalso"; "as"; "case"; "datatype"; "do";
     "else"; "end"; "eqtype"; "exception"; "fn"; "functor"; "fun";
     "handle"; "if"; "in"; "include"; "infix"; "infixr"; "let";
     "local"; "nonfix"; "of"; "op"; "open"; "orelse"; "raise"; "rec";
     "sharing"; "sig"; "signature"; "struct"; "structure";"then";
     "type"; "val"; "with"; "withtype"; "where"; "while"]

  let reservedOpNames =
    [":>"; ":"; "..."; "_"; "|"; "=>"; "->"; "#"; "*"; "="]

  let caseSensitive = true

  let negSign st = (char '~') st
  let posSign = mzero

end

module SExpLD : LanguageDef =
struct
  open CharParse.CharPrim
  open CharParse.M

  let commentStart = "#|"
  let commentEnd = "|#"
  let commentLine = ";"
  let nestedComments = false
  
  let starters = explode "!$_/:?<=#>%&*@[]{|}^~"
  let others = explode "+-"

  let identStart st = 
    (alphaNum <|> oneOf starters) st
  let identLetter st = (identStart <|> (oneOf others)) st
  let opStart = mzero
  let opLetter = mzero
  let reservedNames = []

  let reservedOpNames = []

  let caseSensitive = true

  let negSign st = (char '-') st
  let posSign st = (char '+') st

end

module CalcLD : LanguageDef =
struct
  open CharParse.CharPrim
  open CharParse.M

  let commentStart = ""
  let commentEnd = ""
  let commentLine = ""
  let nestedComments = false
  
  let identStart st = letter st
  let identLetter st = (identStart <|> digit <|> char '_') st
  let opStart = mzero
  let opLetter = mzero
  let reservedNames = ["val";"in"]

  let reservedOpNames = ["+";"++";"-";"*";"/"]

  let caseSensitive = true

  let negSign st = (char '-') st
  let posSign st = (char '+') st

end


module type S =
sig

  module Lang : LanguageDef

  type nat_float = Nat of int | Float of float

  val identifier     : ('st,string) CharParse.M.char_parser
  val reserved       : string -> ('st,unit) CharParse.M.char_parser
  val operator       : ('st,string) CharParse.M.char_parser
  val reservedOp     : string -> ('st,unit) CharParse.M.char_parser


  val escMapParsers  : ('st,char) CharParse.M.char_parser list
  val charEscape     : ('st,char) CharParse.M.char_parser
  val charLetter     : ('st,char) CharParse.M.char_parser
  val charLiteral    : ('st,char) CharParse.M.char_parser
  val stringLiteral  : ('st,string) CharParse.M.char_parser

  val natural        : ('st,int) CharParse.M.char_parser
  val integer        : ('st,int) CharParse.M.char_parser
  val float          : ('st,float) CharParse.M.char_parser
  val naturalOrFloat : ('st,nat_float) CharParse.M.char_parser
  val integerOrFloat : ('st,nat_float) CharParse.M.char_parser
  val decimal        : ('st,int) CharParse.M.char_parser
  val hexadecimal    : ('st,int) CharParse.M.char_parser
  val octal          : ('st,int) CharParse.M.char_parser

  val symbol         : string -> ('st,string) CharParse.M.char_parser
  val symbolChar     : char -> ('st,char) CharParse.M.char_parser
  val lexeme         : ('st,'a) CharParse.M.char_parser -> 
                         ('st,'a) CharParse.M.char_parser
  val whiteSpace     : ('st,unit) CharParse.M.char_parser

  val parens         : ('st,'a) CharParse.M.char_parser -> 
                         ('st,'a) CharParse.M.char_parser
  val braces         : ('st,'a) CharParse.M.char_parser -> 
                         ('st,'a) CharParse.M.char_parser
  val angles         : ('st,'a) CharParse.M.char_parser -> 
                         ('st,'a) CharParse.M.char_parser
  val brackets       : ('st,'a) CharParse.M.char_parser -> 
                         ('st,'a) CharParse.M.char_parser

  val semi           : ('st,char) CharParse.M.char_parser
  val comma          : ('st,char) CharParse.M.char_parser
  val colon          : ('st,char) CharParse.M.char_parser
  val dot            : ('st,char) CharParse.M.char_parser

  val semiSep        : ('st,'a) CharParse.M.char_parser -> 
                         ('st,'a list) CharParse.M.char_parser
  val semiSep1       : ('st,'a) CharParse.M.char_parser -> 
                         ('st,'a list ) CharParse.M.char_parser
  val commaSep       : ('st,'a) CharParse.M.char_parser -> 
                         ('st,'a list) CharParse.M.char_parser
  val commaSep1      : ('st,'a) CharParse.M.char_parser -> 
                         ('st,'a list) CharParse.M.char_parser

end

module M (LD : LanguageDef) : S =
struct

  module Lang = LD

  open CharParse
  open CharPrim
  open M
  open CharComb

  type nat_float = Nat of int | Float of float


 (* white space, comments, and symbols *)

  let ignoreSpace  s = (space >> return ()) s

  let startEnd = 
    let nub = 
      List.fold_left (fun l c -> if List.mem c l then l else (c::l)) []
    in
      nub ((explode Lang.commentStart) @ (explode Lang.commentEnd))

  let oneLineComment s =
      (attempt (string (Lang.commentLine))
   >>  skipMany (satisfy ((!=) '\n'))
   >>  return ()) s


  let rec multiLineComment st =
    let inComment = if Lang.nestedComments then comMulti else comSingle in    
      (attempt (string Lang.commentStart) >> inComment) st

  and comMulti s = (
      (attempt (string Lang.commentEnd) >> return ())
  <|> (multiLineComment >> comMulti)
  <|> (skipMany1 (noneOf startEnd) >> comMulti)
  <|> (oneOf startEnd >> comMulti)
  <?> "end of comment") s

  and comSingle s = (
      (attempt (string Lang.commentEnd) >> return ())
  <|> (skipMany1 (noneOf startEnd)   >> comSingle)
  <|> (oneOf startEnd                >> comSingle)
  <?> "end of comment") s

  let whiteSpace s =
    (match ("" = Lang.commentLine, "" = Lang.commentStart) with
        (true,true) -> skipMany (ignoreSpace <?> "")
      | (true,_)    -> skipMany (ignoreSpace <|> multiLineComment <?> "")
      | (_,true)    -> skipMany (ignoreSpace <|> oneLineComment   <?> "")
      | _  -> skipMany (ignoreSpace <|> multiLineComment <|> 
                          oneLineComment <?> "")) s

  let lexeme p = (p >>= fun x -> (whiteSpace >> return x))

  let symbol name = lexeme (string name)
  let symbolChar name = lexeme (char name)


  (* Identifiers and reserved words *)

  let caseString name =
    if Lang.caseSensitive then string name else
      let l = String.length name in
      let rec walk n =
        if n <= 0 then return () else
          let c = name.[l-n] in
          (char (Char.lowercase c) <|> char (Char.uppercase c)) <?> name 
       >> walk (n-1)
      in
        walk l >> return name

  let reserved name = 
    lexeme (attempt (
       caseString name
    >> (notFollowedBy (Lang.identLetter) <?> ("end of " ^ name))))

  let ident st =
    (   (Lang.identStart
     >>= fun c -> many (Lang.identLetter)
     >>= fun cs -> return ((String.make 1 c) ^ (implode cs)))
      (* XXX do something faster! *)
    <?> "identifier") st

  let theReservedNames =
    if Lang.caseSensitive then List.sort String.compare Lang.reservedNames
    else List.sort String.compare (List.map String.lowercase Lang.reservedNames)

  let isReserved names name =
    let rec scan = function
        [] -> false
      | (r::rs) ->
          let i = String.compare r name in
            if i < 0 then scan rs else
              if i = 0 then true
              else false
    in
      scan names

  let isReservedName name =
    isReserved theReservedNames 
      (if Lang.caseSensitive then name
        else String.lowercase name)
              
  let identifier st =
    (lexeme( attempt( 
      ident >>= fun name -> 
        if (isReservedName name) then unexpected ("reserved word " ^ name)
        else return name))) st
                    


  (* Operators and reserved ops *)
  let theReservedOps = List.sort String.compare Lang.reservedOpNames
  let isReservedOp name = isReserved theReservedOps name

  let oper st =
    ((   Lang.opStart
     >>= fun c -> many Lang.opLetter (* XXX speed *)
     >>= fun cs -> return ((String.make 1 c) ^ (implode cs)))) st

  let operator st =
    lexeme (attempt (
         oper
     >>= fun name -> 
           if isReservedOp name then unexpected ("reserved operator " ^ name)
           else return name)) st

  let reservedOp name =
    lexeme (attempt (
      string name >> (notFollowedBy (Lang.opLetter) <?> ("end of " ^ name))))


  (* Numbers *)
  let digitToInt c =
    if '0' <= c && c <= '9' then Char.code c - Char.code '0'
    else if 'a' <= c && c <= 'f' then Char.code c - Char.code 'a' + 10
    else if 'A' <= c && c <= 'F' then Char.code c - Char.code 'A' + 10
    else failwith "digitToInt on a non-digit (pcl internal error)"

  let number base baseDigit =
    many1 baseDigit >>= fun digits -> 
      return (List.fold_left (fun x d -> base*x + (digitToInt d)) 0 digits)

  let decimal st = number 10 digit st
  let hexadecimal st = (oneOf ['x';'X'] >> number 16 hexDigit) st
  let octal st = (oneOf ['o';'O'] >> number 8 octDigit) st

  let zeroNumber st =
    (char '0' >> (hexadecimal <|> octal <|> decimal <|> return 0) <?> "") st

  let nat st = (zeroNumber <|> decimal) st

  type negation = Neg | Pos | No

  let sign st = (lexeme
  (   (Lang.negSign >> return Neg)
  <|> (Lang.posSign >> return Pos)
  <|> (return No) 
  <?> "sign")) st

  let getIntNeg = function
      Neg -> (fun x -> -x)
    | _   -> (fun x -> x)

  let getFloatNeg = function
      Neg -> (fun x -> -.x)
    | _ -> (fun x -> x)

  let int st =
  (   sign
  >>= fun s -> nat
  >>= fun n -> return ((getIntNeg s) n)) st

  let exponent st =
  (   oneOf ['e';'E']
  >>  sign
  >>= fun s -> decimal <?> "exponent"
  >>= fun e -> return (10.0**(float ((getIntNeg s) e))) <?> "exponent") st

  let fraction st = 
  (  (char '.'
  >>  many1 digit <?> "fraction"
  >>= fun digits ->
        return (List.fold_right (fun d f -> (f +. float (digitToInt d))/.10.0)
                   digits 0.0)) 
  <?> "fraction") st

  let fractExponent n =
         (fraction
      >>= fun fract -> option 1.0 exponent
      >>= fun expo -> return ((float n +. fract)*.expo))
  <|>    (exponent
      >>= fun expo -> return (float n *. expo))

  let fractFloat n = fractExponent n >>= fun f -> return (Float f)

  let decimalFloat st =
  (   decimal
  >>= fun n -> option (Nat n) (fractFloat n)) st

  let zeroNumFloat st =
  (   (   hexadecimal <|> octal
      >>= fun n -> return (Nat n))
  <|> decimalFloat
  <|> fractFloat 0
  <|> return (Nat 0)) st

  let natFloat st = ((char '0' >> zeroNumFloat) <|> decimalFloat) st

  let floating st = (decimal >>= fun n -> fractExponent n) st

  let natural st = (lexeme nat <?> "natural") st
  let integer st = (lexeme int <?> "integer") st
  let float st = (lexeme floating <?> "float") st
  let naturalOrFloat st = (lexeme natFloat <?> "number") st
  let integerOrFloat st =
  (  (sign
  >>= fun s -> naturalOrFloat
  >>= fun n -> 
        match (n,s) with
            (Nat n, Neg) -> return (Nat (-n))
          | (Float f, Neg) -> return (Float (-.f))
          | (n,_) -> return n) 
  <?> "number") st
    
  


  (* chars and strings *)

  (* XXX - these really needs to be parameterized on a function to deal
   * with escape codes *)

(* XXX - writing out this whole list is awful.  Is there a better way
 * that doesn't force me to recreate it every time I use it? *)


  let escMapParsers = 
    [ (fun st -> (char 'a' >> return '\007') st);
      (fun st -> (char 'b' >> return '\b') st);
      (fun st -> (char 't' >> return '\t') st);
      (fun st -> (char 'n' >> return '\n') st);
      (fun st -> (char 'v' >> return '\011') st);
      (fun st -> (char 'f' >> return '\012') st);
      (fun st -> (char 'r' >> return '\r') st);
      (fun st -> (char '\\' >> return '\\') st);
      (fun st -> (char '\'' >> return '\'') st);
      (fun st -> (char '\"' >> return '\"') st) ]

  let charEsc st = choice escMapParsers st

  let charNum st = 
  (   (   (    count 3 digit
           >>= fun ds -> return 
                 (List.fold_left (fun x d -> 10*x + (digitToInt d)) 0 ds))
      <|> (    char 'u' 
           >>  count 4 hexDigit 
           >>= fun ds -> return
             (List.fold_left (fun x d -> 16*x +(digitToInt d)) 0 ds)))
  >>= fun code -> if code > 255 then unexpected "character prim out of range"
                  else return (Char.chr code)) st

  let escapeCode st = (charEsc <|> charNum <?> "escape code") st

  let charEscape st = (char '\\' >> escapeCode) st 
  let charLetter st = satisfy (fun c -> (c != '\'') && (c != '\\') &&
                                        (c > '\026')) st


  (* doing the sml thing here.  Should be parameterized XXX *)
  let charLiteral st = (lexeme (
      (attempt (string "#\""))
  >>  (charLetter <|> charEscape <?> "literal character")
  >>= fun c -> (char '\"' <?> "end of char literal") >> return c)
  <?> "character") st

  let escapeGap st = 
  (   many1 space
  >>  char '\\' <?> "end of string gap") st

  let stringEscape st =
  (   char '\\'
  >>  (escapeGap >> return None)
  >>  (escapeCode >>= fun esc -> return (Some esc))) st

  let stringLetter st =
    (satisfy (fun c -> (c != '"') && (c != '\\') && (c > '\026'))) st

  let stringChar st =
  (   (   stringLetter
      >>= fun c -> return (Some c))
  <|> stringEscape
  <?> "string character") st

  (* what we really need is "many" array/string primatives *)
  let stringLiteral st = ((lexeme (
      between (char '"') (char '"' <?> "end of string") (many stringChar)
  >>= fun str -> return 
         (implode 
            (List.fold_right (fun c s -> match c with None -> s
                                                    | Some c -> c::s) str []))))
  <?> "literal string") st

  (* bracketing *)
  let parens p = between (symbolChar '(') (symbolChar ')') p
  let braces p = between (symbolChar '{') (symbolChar '}') p
  let angles p = between (symbolChar '<') (symbolChar '>') p
  let brackets p = between (symbolChar '[') (symbolChar ']') p

  let semi st  = symbolChar ';' st
  let comma st = symbolChar ',' st
  let colon st = symbolChar ':' st 
  let dot st   = symbolChar '.' st

  let semiSep p = sepBy p semi
  let semiSep1 p = sepBy1 p semi

  let commaSep p = sepBy p comma
  let commaSep1 p = sepBy1 p comma

end
