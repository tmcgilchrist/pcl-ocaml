open CharParse.CharPrim
open CharParse.CharComb

module L = Language.M(Language.CalcLD)
open L

module Exp = Expr.M (CharParse.CharPrim)
open Exp


let binary name f assoc = Infix (assoc,(reservedOp name >> return f))
let prefix name f = Prefix (reservedOp name >> return f)
let postfix name f = Postfix (reservedOp name >> return f)


let rec expr st = 
  let table = [ [postfix "++" (fun x -> x + 1)];

                [binary "*" ( * ) AssocLeft; binary "/" ( / ) AssocLeft ];

                [binary "+" (+) AssocLeft; binary "-" (-) AssocLeft ];

                [Nonfix (    symbol "if" >>  identifier 
                         >>= fun tf -> symbol "then" >> expr 
                         >>= fun e1 -> symbol "else" >> expr
                         >>= fun e2 -> symbol "end"
                         >> if tf = "t" then return e1 else return e2)]
              ]
  in
    (buildExpressionParser table (term : ('st,int) parser) <?> "expression") st


and term st =  (parens expr <|> natural <?> "simple expression") st



let calc st = (whiteSpace >> expr >>= fun r -> eof >> return r) st

let eval_calc s = 
  match parse "calc" (LazyList.M.ofString s) calc with
      Success x -> Some x
    | Failure err -> 
        print_string (Error.M.errorToString err); None
