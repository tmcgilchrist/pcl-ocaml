(* symbol.sml
 *
 * (C) 2001 Lucent Technologies, Bell Labs
 *)
let varInt = 0 and sigInt = 1 and strInt = 2 and fsigInt = 3 and 
    fctInt = 4 and tycInt = 5 and labInt = 6 and tyvInt = 7 and
    fixInt = 8

type symbol = SYMBOL of int * string
type namespace =
     VALspace | TYCspace | SIGspace | STRspace | FCTspace | FIXspace |
     LABspace | TYVspace | FSIGspace 

let eq(SYMBOL(a1,b1),SYMBOL(a2,b2)) = a1=a2 && b1=b2
let symbolGt(SYMBOL(_,s1), SYMBOL(_,s2)) = s1 > s2
let symbolCMLt (SYMBOL (a1, s1), SYMBOL (a2, s2)) =
  a1 < a2 || (a1 = a2 && s1 < s2)

let compare (SYMBOL(a1,s1)) (SYMBOL(a2,s2)) = 
  match a1-a2 with  
      0 -> String.compare s1 s2
	  | i -> i

let varSymbol (name: string) =
  SYMBOL(Hashtbl.hash name + varInt,name)
let tycSymbol (name: string) =
	SYMBOL(Hashtbl.hash name + tycInt, name)
let fixSymbol (name: string) =
	SYMBOL(Hashtbl.hash name + fixInt, name)
let labSymbol (name: string) =
	SYMBOL(Hashtbl.hash name + labInt, name)
let tyvSymbol (name: string) =
	SYMBOL(Hashtbl.hash name + tyvInt, name)
let sigSymbol (name: string) =
        SYMBOL(Hashtbl.hash name + sigInt, name)
let strSymbol (name: string) =
	SYMBOL(Hashtbl.hash name + strInt, name)
let fctSymbol (name: string) =
	SYMBOL(Hashtbl.hash name + fctInt, name)
let fsigSymbol (name: string) =
	SYMBOL(Hashtbl.hash name + fsigInt, name)

let var'n'fix name =
  let h = Hashtbl.hash name
	in (SYMBOL(h+varInt,name),SYMBOL(h+fixInt,name))


let name (SYMBOL(_,name)) = name
let number (SYMBOL(number,_)) = number
let nameSpace (SYMBOL(number,name)) : namespace =
  match number - Hashtbl.hash name with
	  0 -> VALspace
  | 5 -> TYCspace
  | 1 -> SIGspace
  | 2 -> STRspace
  | 4 -> FCTspace
  | 8 -> FIXspace
  | 6 -> LABspace
  | 7 -> TYVspace
	| 3 -> FSIGspace
	| _ -> failwith "Symbol.nameSpace"
  
let nameSpaceToString (n : namespace) : string =
  match n with
    VALspace -> "variable or constructor"
  | TYCspace -> "type constructor"
  | SIGspace -> "signature"
  | STRspace -> "structure"
  | FCTspace -> "functor"
  | FIXspace -> "fixity"
  | LABspace -> "label"
	| TYVspace -> "type variable"
	| FSIGspace -> "functor signature"

let describe s = String.concat " " [nameSpaceToString (nameSpace s); name s]

let symbolToString(SYMBOL(number,name)) : string =
  match number - Hashtbl.hash name with
 	  0 -> "VAL$"^name
  | 1 -> "SIG$"^name
  | 2 -> "STR$"^name
  | 3 -> "FSIG$"^name
  | 4 -> "FCT$"^name
  | 5 -> "TYC$"^name
  | 6 -> "LAB$"^name
  | 7 -> "TYV$"^name
  | 8 -> "FIX$"^name
  | _ -> failwith "Symbol.toString"

