(* Copyright 1992 by AT&T Bell Laboratories 
 *
 *)

type fixity
type symbol = Symbol.symbol
val infixleft : int -> fixity
val infixright : int -> fixity
val nonfix : fixity

type literal = int
    
(* to mark positions in files *)
type srcpos = Pos.M.pos
type region = srcpos * srcpos

type path = symbol list
type 'a fixitem (* = {item: 'a, fixity: symbol option, region: region} *)
    
type 'a sigConst 
    = NoSig
    | Transparent of 'a
    | Opaque of 'a

type inf_op = Op | NoOp
          
(* EXPRESSIONS *)

type exp
    = VarExp of path		(* variable *)
    | FnExp of case list		(* abstraction *)
    | FlatAppExp of exp list
                                  (* expressions prior to fixity parsing *)
    | AppExp of exp * exp (* fun , arg *)
				(* application *)
    | CaseExp of exp * case list(* exp, rules *)
				(* case expression *)
    | LetExp of dec * exp list (* let expression, (dec,exp) *)
    | SeqExp of exp list		(* sequence of expressions *)
    | IntExp of literal		(* integer *)
    | WordExp of literal	(* word literal *)
    | RealExp of float    (* floating point *)
    | StringExp of string	(* string *)
    | CharExp of char     (* char *)
    | RecordExp of (symbol * exp) list	(* record *)
    | ListExp of exp list	(*  [list,in,square,brackets] *)
    | TupleExp of exp list	(* tuple (derived form) *)
    | SelectorExp of symbol	(* selector of a record field *)
    | ConstraintExp of exp * ty
				(* type constraint *)
    | HandleExp of exp * case list
				(* exception handler *)
    | RaiseExp of exp		(* raise an exception *)
    | IfExp of exp*exp*exp (* test,then,else *)
				(* if expression (derived form) *)
    | AndalsoExp of exp * exp	(* andalso (derived form) *)
    | OrelseExp of exp * exp	(* orelse (derived form) *)
    | VectorExp of exp list       (* vector *)
    | WhileExp of exp * exp (* test,exp *)
				(* while (derived form) *)
    | MarkExp of exp * region	(* mark an expression *)

(* RULE for case functions and exception handler *)
and case = Rule of pat*exp

(* PATTERN *)
and pat 
    = WildPat				(* empty pattern *)
	  | VarPat of path*inf_op   (* variable pattern *)
	  | IntPat of literal			(* integer *)
	  | WordPat of literal			(* word literal *)
	  | StringPat of string			(* string *)
	  | CharPat of char			(* char *)
	  | RecordPat of (symbol * pat) list * bool (* (rec,flexibility) *)
				(* record *)
    | ListPat of pat list		       (*  [list,in,square,brackets] *)
	  | TuplePat of pat list		(* tuple *)
    | FlatAppPat of pat list
                                        (* patterns prior to fixity parsing *)
	  | AppPat of path*pat (* application, (constr,arg) *)
	  | ConstraintPat of pat*ty (* pat, constraint *)
						  (* constraint *)
	  | LayeredPat of pat * pat (* (var,exp) *) (* as expressions *)
    | VectorPat of pat list                 (* vector pattern *)
	  | MarkPat of pat * region	(* mark a pattern *)
	  | OrPat of pat list			(* or-pattern *)

(* STRUCTURE EXPRESSION *) 
and strexp 
    = VarStr of path			(* variable structure *)
	  | BaseStr of strdec list (* defined structure *)
    | ConstrainedStr of strexp * sigexp sigConst (* signature constrained *)
	  | AppStr of path * strexp list (* application (external) *)
	  | LetStr of strdec list * strexp (* let in structure *)
	  | MarkStr of strexp * region (* mark *)


and strdec
    = DecStrD of dec
    | DefStrD of strb list
    | LocalStrD of strdec list * strdec list


(* SIGNATURE EXPRESSION *)
and sigexp 
    = VarSig of symbol			 (* signature variable *)
    | AugSig of sigexp * ((path * tyvar list * ty) list)
        (* sig augmented with where spec *)
	  | BaseSig of spec list		 (* defined signature *)
	  | MarkSig of sigexp * region	 (* mark *)
        
(* SPECIFICATION FOR SIGNATURE DEFINITIONS *)
and spec 
    = StrSpec of strd list
  (* structure *)
    | TycSpec of typd list * bool
        (* type, eq? *)
	  | ValSpec of vald list  (* value *)
	  | DataDefSpec of datd list
        (* datatype *)
    | DataReplSpec of symbol * path
	  | ExceSpec of exd list (* exception *)
	  | ShareStrSpec of spec * path list (* structure sharing *)
	  | ShareTycSpec of spec * path list (* type sharing *)
	  | IncludeSpec of sigexp			(* include specif *)
    | SeqSpec of spec list 
	  | MarkSpec of spec * region	(* mark a spec *)
     

and strd = Strd of symbol*sigexp
         | MarkStrd of strd * region

and typd = Typd of symbol * tyvar list * ty option
         | MarkTypd of typd * region

and vald = Vald of symbol * ty
         | MarkVald of vald * region

and cond = Cond of (symbol * ty option)
         | MarkCond of cond * region

and datd = DatdClauses of symbol * tyvar list * cond list
         | MarkDatd of datd * region

and exd = Exd of symbol * ty option
        | MarkExd of exd * region

        
(* DECLARATIONS (let and structure) *)
and dec 
    = ValDec of (vb list * tyvar list)		(* values *)
	  | FunDec of (fb list * tyvar list)		(* recurs functions *)
	  | TypeDec of tb list				(* type dec *)
	  | DatatypeDec of (db list)*(tb list)
				(* datatype dec *)
	  | AbstypeDec of (db list)*(tb list)*dec
							  (* abstract type *)
	  | ExceptionDec of eb list			(* exception *)
	  | AbsDec of strb list				(* abstract struct *)
	  | LocalDec of dec * dec				(* local dec *)
	  | SeqDec of dec list				(* sequence of dec *)
	  | OpenDec of path list			(* open structures *)
	  | FixDec of fixity * symbol list  (* fixity *)
	  | MarkDec of dec * region		(* mark a dec *)

(* VALUE BINDINGS *)
and vb = Vb of pat*exp
       | Rvb of pat*exp
	     | MarkVb of vb * region

(* RECURSIVE FUNCTIONS BINDINGS *)
and fb = Fb of clause list
	     | MarkFb of fb * region

(* CLAUSE: a definition for a single pattern in a function binding *)
and clause = Clause of pat list * ty option * exp

(* TYPE BINDING *)
and tb = Tb of symbol * ty * tyvar list
	     | MarkTb of tb * region

(* DATATYPE BINDING *)
and db = Db of symbol * (tyvar list) * dbrhs
        | MarkDb of db * region

(* DATATYPE BINDING RIGHT HAND SIDE *)
and dbrhs = Constrs of (symbol * ty option) list
          | Repl of symbol list

(* EXCEPTION BINDING *)
and eb = EbGen of symbol * ty option (* Exception definition *)
	     | EbDef of symbol * path	  (* defined by equality *)
	     | MarkEb of eb * region

(* STRUCTURE BINDING *)
and strb = Strb of symbol * strexp
         | MarkStrb of strb * region

(* FUNCTOR BINDING *)
and fctarg = FctVarArg of symbol * sigexp
           | FctSpecArg of spec
           
           (* name, args, def *)
and fctb = Fctb of symbol * fctarg list * strexp
         | MarkFctb of fctb * region

(* SIGNATURE BINDING *)
and sigb = Sigb of symbol * sigexp
         | MarkSigb of sigb * region

(* TYPE VARIABLE *)
and tyvar = Tyv of symbol
          | MarkTyv of tyvar * region

(* TYPES *)
and ty 
    = VarTy of tyvar			(* type variable *)
    | ConTy of symbol list * ty list	(* type constructor *)
    | RecordTy of (symbol * ty) list 	(* record *)
    | TupleTy of ty list		(* tuple *)
    | MarkTy of ty * region	        (* mark type *)
    | ArrowTy of ty*ty  (* arrow type *)

(* PROGRAMS *)
type topdec =
   TopStrDec of strdec list
 | TopSigDec of sigb list
 | TopFctDec of fctb list
