module type S = 
sig

module P : Prim.S

(* Common combinators *)

val app    : ('a -> 'b) -> ('st,'a) P.parser -> ('st,'b) P.parser

val choice : ('st,'a) P.parser list -> ('st,'a) P.parser
                 
val option : 'a -> ('st,'a) P.parser -> ('st,'a) P.parser
                 
val optional : ('st,'a) P.parser -> ('st,unit) P.parser

val optionRet : ('st,'a) P.parser -> ('st,'a option) P.parser
                 
val between : ('st,'op) P.parser -> ('st,'cl) P.parser ->
              ('st,'a) P.parser -> ('st,'a) P.parser
                 
val skipMany1 : ('st,'a) P.parser -> ('st,unit) P.parser
                 
val many1 : ('st,'a) P.parser -> ('st,'a list) P.parser
                 
val sepBy1 : ('st,'a) P.parser -> ('st,'sep) P.parser ->
             ('st,'a list) P.parser
                 
val sepBy :  ('st,'a) P.parser -> ('st,'sep) P.parser ->
             ('st,'a list) P.parser

val sepByArr : ('st,'a) P.parser -> ('st,'sep) P.parser ->
               ('st,'a array) P.parser
                 
val sepEndBy1 : ('st,'a) P.parser -> ('st,'sep) P.parser -> 
                ('st,'a list) P.parser
                 
val sepEndBy : ('st,'a) P.parser -> ('st,'sep) P.parser -> 
               ('st,'a list) P.parser

val sepEndByArr : ('st,'a) P.parser -> ('st,'sep) P.parser ->
                  ('st, 'a array) P.parser
                 
val endBy1 : ('st,'a) P.parser -> ('st,'sep) P.parser -> 
             ('st,'a list) P.parser
                 
val endBy : ('st,'a) P.parser -> ('st,'sep) P.parser -> 
            ('st,'a list) P.parser
                 
val count : int -> ('st,'a) P.parser -> 
            ('st,'a list) P.parser

val foldl   : ('st,'a -> 'a) P.parser -> 'a -> ('st,'a) P.parser
                 
val chainr1 : ('st,'a) P.parser -> 
              ('st,'a -> 'a -> 'a) P.parser ->
              ('st,'a) P.parser

val chainl1 : ('st,'a) P.parser -> 
              ('st,'a -> 'a -> 'a) P.parser ->
              ('st,'a) P.parser
                 
val chainr : ('st,'a) P.parser -> 
             ('st,'a -> 'a -> 'a) P.parser ->
             'a -> 
             ('st,'a) P.parser
                 
val chainl : ('st,'a) P.parser -> 
             ('st,'a -> 'a -> 'a) P.parser ->
             'a -> 
             ('st,'a) P.parser
                 
                 
val anyToken : ('st,P.Tok.t) P.parser
                 
val notFollowedBy : ('st,P.Tok.t) P.parser -> ('st,unit) P.parser

val eof : ('st,unit) P.parser
                 
val manyTill : ('st,'a) P.parser -> ('st,'last) P.parser ->
               ('st,'a list) P.parser

end



module M (Prim : Prim.S) : 
  S with type 'a P.state = 'a Prim.state 
    and type ('a,'b) P.rcon = ('a,'b) Prim.rcon
    and type P.Tok.t = Prim.Tok.t=
struct

module P = Prim
open Prim

let pred p = token (fun t -> if p t then Some t else None)

let app f p = p >>= (fun r -> return (f r))

let choice ps = List.fold_right (<|>) ps mzero

let option x p = p <|> return x

let optional p = 
    (p >> return ()) <|> return ()
        
let optionRet p = option None (app (fun r -> Some r) p)
(* ocaml could really use first class datatype constructors... *)

let between op cl p =
  (op >> p >>= (fun x -> cl >> return x))

let skipMany1 p = p >> skipMany p
                                 
let many1 p = 
  p 
  >>= fun x -> many p 
  >>= fun xs -> return (x::xs)
                                 
let sepBy1 p sep =
  p 
  >>= fun x -> many (sep >> p)
  >>= fun xs -> return (x::xs)    
                                 
let sepBy p sep = (sepBy1 p sep) <|> return []
         
let sepByArr p sep =
  manyArrOp (p >>= fun x -> ((sep >> return x) <|> return x)) ~one:false
                        
let rec sepEndBy1 p sep = 
  p
  >>= fun x -> ((sep
                 >>  sepEndBy p sep
                 >>= fun xs -> return (x::xs))
             <|> return [x])

and sepEndBy p sep = sepEndBy1 p sep <|> return []
            
let sepEndByArr p sep = 
  manyArrOp (p >>= fun x -> ((sep >> return x) <|> return x)) ~one:false
                         
                     
let endBy1 p sep = many1 (p >>= fun x -> sep >> return x)

let endBy p sep = many (p >>= fun x -> sep >> return x)

let count n p = 
  if n <= 0 then return [] 
  else
    let rec sequence =
      function [] -> return []
        | (p::ps) -> p >>= fun r -> sequence ps >>= fun rs -> return (r::rs)
    in
    let rec replicate n a acc = if n = 0 then acc 
                                else replicate (n-1) a (a::acc)
    in sequence (replicate n p [])
           
let rec foldl op a =
  (    (op >>= fun f -> foldl op (f a))
   <|> return a)

let chainl1 p op =
  let rec rest x = (op >>= fun f -> p >>= fun y -> rest (f x y))
               <|> return x 
  in p >>= fun x -> rest x
                                 
let chainr1 p op = 
  let rec rest x = (op 
                    >>= fun f -> (scan ()) 
                    >>= fun y -> return (f x y))
               <|> (return x)
  and scan () = p >>= fun x -> rest x
  in scan ()
                                 
let chainr p op x = chainr1 p op <|> return x

let chainl p op x = chainl1 p op <|> return x
                                 
let anyToken st = (token (fun x -> Some x)) st

let notFollowedBy p = attempt ((p >>= fun c -> unexpected (P.Tok.toString c))
                             <|> return ())
                                 
let eof st = ((notFollowedBy anyToken) <?> "end of input") st

let manyTill p en =
  let rec scan () =
    (en >> return []) <|> (p >>= fun x -> scan () >>= fun xs -> return (x::xs))
  in scan ()

end
