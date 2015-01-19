(* 
 * This example is taken from 
 * FUNCTIONAL PEARLS
 * Monadic Parsing in Haskell
 * Graham Hutton, Erik Meijer
 * 
 * Grammar:
 * num ::= n
 * expr ::= expr addop term | term
 * term ::= term mulop factor | factor
 * factor ::= num | (expr)
 *
 * addop ::= + | -
 * mulop ::= * | /
 *)

open CharParse.CharPrim
open CharParse.CharComb
open CharParse.M


let num =
(    (many1 digit) 
  >>= fun digits -> return (List.fold_left
                             (fun n d -> (Char.code d)- 48 +(10*n)) 0  digits))
  <?> "decimal number"

let addop =
(    (char '+' >> return (+))
 <|> (char '-' >> return (-))) <?> "+ or -"

let mulop =
(    (char '*' >> return ( * ))
 <|> (char '/' >> return (/))) <?> "* or /"


let rec expr st = (parseLazy (lazy (chainl1 term addop))) st

and term st = chainl1 factor mulop st

and factor st = 
      (num
   <|> (between (char '(') (char ')') expr)) st


let eval_calc s = 
  match parse "calc" (LazyList.M.ofString s) expr with
      Success x -> Some x
    | Failure err -> 
        print_string (Error.M.errorToString err); None
