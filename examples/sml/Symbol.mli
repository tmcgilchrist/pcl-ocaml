(* Copyright 1989 by AT&T Bell Laboratories *)

type symbol = SYMBOL of int * string (* XXX this should be abstract *)
type namespace =
    VALspace | TYCspace | SIGspace | STRspace | FCTspace | FIXspace |
    LABspace | TYVspace | FSIGspace

val eq: symbol * symbol -> bool
val symbolGt : symbol * symbol -> bool
val symbolCMLt : symbol * symbol -> bool
val varSymbol: string -> symbol
val tycSymbol: string -> symbol
val sigSymbol: string -> symbol
val strSymbol: string -> symbol
val fctSymbol: string -> symbol
val fsigSymbol: string -> symbol
val fixSymbol: string -> symbol
val labSymbol: string -> symbol
val tyvSymbol: string -> symbol
val var'n'fix : string -> symbol * symbol
val name: symbol -> string
val number: symbol -> int
val nameSpace : symbol -> namespace
val nameSpaceToString : namespace -> string
val describe : symbol -> string
val symbolToString : symbol -> string
val compare : symbol -> symbol -> int
  (* Probably should merge STRspace val FCTspace into one namespace.
     Similarly for SIGspace val FSIGspace. *)

