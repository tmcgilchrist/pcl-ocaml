type ('st,'a) parser = ('st,'a) CharParse.CharPrim.parser

type scon = 
    IntScon of Ast.literal | WordScon of Ast.literal | RealScon of float
  | StringScon of string | CharScon of char

val  scon_p : ('a,scon) parser

(* identifiers *)
val lab_p : ('a,Ast.symbol) parser


(* types *)

val tyvar_p : ('a,Ast.tyvar) parser

val ty_p : ('a,Ast.ty) parser


(* patterns *)
val pat_p : ('a,Ast.pat) parser


(* bindings *)

val exbind_p : ('a,Ast.eb list) parser

val conbind_p : ('a,Ast.dbrhs) parser

val datbind_p : ('a,Ast.db list) parser

val typbind_p : ('a,Ast.tb list) parser
 
val fvalbind_p : ('a,Ast.fb list) parser

val valbind_p : ('a,Ast.vb list) parser


(* declarations *)

val dec_p : ('a,Ast.dec) parser


(* match *)

val match_p : ('a,Ast.case list) parser


(* expressions *)

val atexp_p : ('a,Ast.exp) parser

val appexp_p : ('a,Ast.exp) parser

val exp_p : ('a,Ast.exp) parser

(* modules *)

(* specifications *)

val strdesc_p : ('a,Ast.strd list) parser

val exdesc_p : ('a, Ast.exd list) parser

val condesc_p : ('a, Ast.cond list) parser

val datdesc_p : ('a, Ast.datd list) parser

val typdesc_p : ('a, Ast.typd list) parser

val valdesc_p : ('a, Ast.vald list) parser


val spec_p : ('a,Ast.spec) parser

(* signatures *)

val sigexp_p : ('a, Ast.sigexp) parser

val sigdec_p : ('a, Ast.sigb list) parser


(* structures *)

val strexp_p : ('a, Ast.strexp) parser

val strbind_p : ('a, Ast.strb list) parser

val strdec_p : ('a, Ast.strdec list) parser

(* functors *)

val fctdec_p : ('a, Ast.fctb list) parser

(* topdec, program *)

val topdec_p : ('a, Ast.topdec list) parser

val program_p : ('a,Ast.topdec list list) parser


val string_to_type : string -> Ast.ty option
val string_to_pat : string -> Ast.pat option
val string_to_dec : string -> Ast.dec option
val string_to_exp : string -> Ast.exp option
val string_to_sigexp : string -> Ast.sigexp option
val string_to_strdec : string -> Ast.strdec list option
val string_to_program : string -> Ast.topdec list list option
val file_to_program : string -> Ast.topdec list list option

