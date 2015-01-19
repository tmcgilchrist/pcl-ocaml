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

module L = Language.M(Language.SMLLD)
open L

let num = L.integer

let addop = ((symbol "+" >> return (+)) 
         <|> (symbol "-" >> return (-)))

let mulop =
(    (symbol "*" >> return ( * ))
 <|> (symbol "/" >> return (/)))


let rec expr st = (chainl1 term addop) st

and term st = (chainl1 factor mulop) st

and factor st = (num <|> (parens expr)) st

let calc st = (whiteSpace >> expr >>= fun r -> eof >> return r) st

let eval_calc s = 
  match parse "calc" (LazyList.M.ofString s) calc with
      Success x -> Some x
    | Failure err -> 
        print_string (Error.M.errorToString err); None
