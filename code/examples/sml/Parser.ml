open CharParse

type ('st,'a) parser = ('st,'a) CharPrim.parser

module MLLang = Language.M(Language.SMLLD)


open CharPrim
open CharComb
open CharExpr
open MLLang


open Ast
open Symbol


(* constants *)
type scon = 
    IntScon of Ast.literal | WordScon of Ast.literal | RealScon of float
  | StringScon of string | CharScon of char

let scon_p st =
    (* word constants are a special case *)
  (   ((   attempt (M.chars ['0';'w'])
       >>  (    (    M.char 'x'
                 >>  hexadecimal)
            <|> decimal)
       >>= fun i -> return (WordScon i))
       <?> "word constant")
  <|> ((    integerOrFloat
       >>= fun nf -> return (match nf with
                               Nat i -> IntScon i
                             | Float f -> RealScon f))
      <?> "numerical constant")
  <|> ((   attempt (M.chars ['#';'\"'])
       >>  (charLetter <|> charEscape)
       >>= fun c -> symbolChar '\"'
       >>  return (CharScon c))
      <?> "character constant")
  <|> ((stringLiteral >>= fun s -> return (StringScon s))
       <?> "string constant"))
  st
     
    

(* identifiers *)
let vid_p st =
  ((identifier <|> operator <|> (symbol "*" >> return "*")) 
   <?> "value identifier") st

let tycon_p st = (identifier <|> operator) st

let lab_p st =
  (    (    identifier
        <|> (    natural   
             >>= fun n -> if n = 0 then fail "0 is not a valid record label"
                                   else return (string_of_int n)))
   >>= fun l -> return (labSymbol l)) 
  st

let make_path el sym st =
  (  sepBy1 (    el
             >>= fun s -> (   (   peek dot
                               >> return (strSymbol s))
                          <|> return (sym s)))
            dot)
  st

let type_path_p st = make_path (identifier <|> operator) tycSymbol st

let vid_path_p st = (    (make_path vid_p varSymbol) 
                     <|> (reservedOp "=" >> return [varSymbol "="])) st

let patv_path_p st = (make_path vid_p varSymbol) st

let str_path_p st = (make_path identifier strSymbol) st

let fct_path_p st = (make_path identifier fctSymbol) st


(* util *)
let pos_wrap p =
      getPosition
  >>= fun pos1 -> p
  >>= fun r -> getPosition
  >>= fun pos2 -> 
       return (pos1,r,Pos.M.setColumn pos2 (max 0 ((Pos.M.column pos2)-1)))


(* types *)
let tyvar_p st =
  ((let eqtyvar = 
      M.char '\'' >> identifier >>= fun s -> return ("\'" ^ s) 
    in
          M.char '\''
      >>  (eqtyvar <|> identifier)          
      >>= fun s -> return (Tyv (tyvSymbol ("\'" ^ s)))) <?> "type variable")
  st


let tyvarseq_p st =
  attempt 
  (   (   pos_wrap tyvar_p
       >>= fun (p1,r,p2) -> return [MarkTyv (r, (p1,p2))])
   <|> (parens (commaSep1 (    pos_wrap tyvar_p
                           >>= fun (pos1,r,pos2) -> 
                                return (MarkTyv (r, (pos1,pos2)))))))
  st




let rec tyseq_p st = 
  (commaSep1 ty_p) st

and simple_ty_p st =
        (* type variable *)
 ( (   (    pos_wrap tyvar_p
        >>= fun (p1,tyv,p2) -> return (MarkTy (VarTy tyv,(p1,p2))))

          (* record type *)
   <|> (   pos_wrap (braces (commaSep (    lab_p
                                        >>= fun l -> colon 
                                        >>  ty_p 
                                        >>= fun t -> return (l,t)))
                        
                        <?> "record type")
        >>= fun (p1,r,p2) -> return (MarkTy (RecordTy r, (p1,p2))))

       (* type construction with more than one argument, or just a type
        * in parens *)
   <|> (    symbolChar '(' 
        >>  ty_p
        >>= fun t -> (    (   (comma (* type construction *)
                           >>  tyseq_p
                           >>= fun ts -> symbolChar ')'
                           >>  pos_wrap type_path_p
                           >>= fun (p1,r,p2) -> 
                                 return (MarkTy (ConTy (r,t::ts), (p1,p2))))
                          <?> "type construction")
                      <|> (    symbolChar ')'
                           >>  return t)))
    
       (* type construction with no arguments *)
   <|> ((   pos_wrap type_path_p 
        >>= fun (p1,r,p2) -> return (MarkTy (ConTy (r,[]), (p1,p2))))
       <?> "type construction"))

(* type construction with one argument *)
  >>= (fun t -> (   (    pos_wrap type_path_p
                     >>= fun (p1,r,p2) -> return (MarkTy (ConTy (r,[t]),
                                                          (p1,p2)))
                    <?> "type construction")
                <|> return t)))
 st

and tuple_tycon_p st = 
      (* first, all the complicated types which start with a type *)
  (    pos_wrap simple_ty_p
   >>= fun (p1,t,p2) -> 
       (   ((   reservedOp "*"
            >>  sepBy1 simple_ty_p (reservedOp "*")
            >>= fun ts -> getPosition
            >>= fun p2 -> 
              return (MarkTy (TupleTy (t::ts), 
                              (p1, Pos.M.incColumn p2 (-1)))))
           <?> "tuple type")

          (* if it's not one of these, it's just a type on its own *)
        <|> return t))
  st

and ty_p st =
  (   (    pos_wrap tuple_tycon_p
       >>= fun (p1,t,p2) ->
                (* arrow type *)
           (   ((    reservedOp "->"
                 >>  ty_p
                 >>= fun t2 -> getPosition
                 >>= fun p2 ->
                       return (MarkTy (ArrowTy (t,t2), 
                                       (p1,Pos.M.incColumn p2 (-1)))))
                <?> "arrow")
            <|> return t))
   <?> "type") 
  st
  


    
(* patterns *)
let rec atpat_p st =
       (* wildcard pattern *)
  ((   (    pos_wrap (symbolChar '_') 
        >>= fun (p1,_,p2) -> return (MarkPat(WildPat,(p1,p2))))
       
   <|> (    pos_wrap scon_p
        >>= fun (p1,sc,p2) -> 
             (match sc with 
                IntScon i -> return (IntPat i)
              | WordScon i -> return (WordPat i)
              | StringScon s -> return (StringPat s)
              | CharScon c -> return (CharPat c)
              | RealScon _ -> 
                  fail ("floating point number may not be used in patterns"))
        >>= fun sc -> return (MarkPat(sc,(p1,p2))))
       
   <|> (    pos_wrap 
              (attempt 
                (    option NoOp (reserved "op" >> return Op) 
                 >>= fun o -> patv_path_p 
                 >>= fun v -> (    (   (attempt 
                                        (    (reservedOp ":" >> return ()) 
                                         <|> reserved "as"))
                                    >> mzero)
                               <|> return ())
                 >>  return (v,o)))
        >>= fun (p1,(v,o),p2) -> return (MarkPat(VarPat(v,o),(p1,p2))))

   <|> (    pos_wrap (braces (patrow_p <|> return ([],false))) 
        >>= fun (p1,(r,b),p2) -> return (MarkPat (RecordPat (r,b),(p1,p2))))
       
   <|> (    getPosition
        >>= fun p1 -> symbolChar '('
        >>  (    (    pat_p
                  >>= fun p -> 
                      (    (    comma 
                            >>  sepBy1 pat_p comma 
                            >>= fun ps -> getPosition
                            >>= fun p2 -> symbolChar ')'
                            >>  return (MarkPat (TuplePat (p::ps),(p1,p2))))
                       <|> (    symbolChar ')'
                            >>  return p)))
             <|> (symbolChar ')' >> return (RecordPat ([],false)))))

   <|> (    pos_wrap (brackets (commaSep pat_p))
        >>= fun (p1,l,p2) -> return (MarkPat (ListPat l,(p1,p2))))
       
) <?> "atomic pattern") st

and patrow_p st = 
   let lab_or_var st = 
     (    (identifier >>= fun i -> return (`Alpha i))
      <|> (    natural   
           >>= fun n -> if n = 0 then fail "0 is not a valid record label"
                                 else return (`Num (string_of_int n))))
     st
   in    
   (    (reservedOp "..." >> return ([],true))
    <|> (    lab_or_var
         >>= fun s -> 
               (    (    symbolChar '='
                     >>  pat_p
                     >>= fun p -> 
                           let l = match s with
                                     `Alpha s -> labSymbol s
                                   | `Num s -> labSymbol s 
                           in
                           (   (    comma
                                >>  patrow_p
                                >>= fun (pts,b) -> return ((l,p)::pts,b))
                            <|> return ([(l,p)],false)))
                <|> (match s with
                       `Num _ -> fail "record pattern missing identifier"
                     | `Alpha s -> 
                       let l = labSymbol s 
                       in
                           optionRet (colon >> ty_p)
                       >>= fun ty -> optionRet (reserved "as" >> pat_p)
                       >>= fun asp -> optionRet (comma >> patrow_p)
                       >>= fun rest ->
                             let (tl,b) = 
                               match rest with
                                 None -> ([],false)
                               | Some r -> r
                             in
                             let vp = VarPat ([varSymbol s],NoOp) in
                             return (match (ty,asp) with
                               (None,None) -> 
                                 ((l,vp)::tl,b)
                             | (Some ty,None) ->
                                 ((l,ConstraintPat (vp,ty))::tl,b)
                             | (None,Some asp) ->
                                 ((l,LayeredPat (vp,asp))::tl,b)
                             | (Some ty,Some asp) ->
                                 ((l,LayeredPat (ConstraintPat (vp,ty), asp))
                                  ::tl,b))))))
    st

(* apppats and infpats are both just parsed as big FlatAppPats, to be
 * sorted out later, which is easier once we know how many there are and
 * what sorts of identifiers we are dealing with, etc. *)

and apppat_p st =
  (    pos_wrap (many1 atpat_p)
   >>= fun (p1,pts,p2) ->
        match pts with
          [] -> failwith "pcl error: empty list from many1, please report"
        | [x] -> return x
        | _ -> return (MarkPat (FlatAppPat pts, (p1,p2))))
  st
                                
and pat_p st =
  ((   (    apppat_p
       <|> (    option NoOp (reserved "op" >> return Op)
            >>= fun op ->  vid_p
            >>= fun v -> optionRet (reservedOp ":" >> ty_p)
            >>= fun t -> reserved "as"
            >>  pat_p
            >>= fun p ->
                  return (LayeredPat (VarPat ([varSymbol v],op),
                                      match t with
                                        None -> p
                                      | Some t -> ConstraintPat (p,t)))))
   >>= (fun p -> (    (    reservedOp ":" >> ty_p 
                       >>= fun t -> return (ConstraintPat (p,t)))
                  <|> return p))) <?> "pattern")
  st

(* XXX This is wrong in that you can't write
 *  "p : ty1 : ty2"
 * (though you can write "(p : ty1) : ty2")  *)


(* bindings *)
let exbind_p st = 
  (sepBy1 
     (    pos_wrap 
          (    optional (reserved "op")
           >>  vid_p
           >>= fun nm ->
                (    (    symbolChar '='
                      >>  (optional (reserved "op"))
                      >>  patv_path_p
                      >>= fun def -> return (EbDef (varSymbol nm,def)))
                 <|> (    optionRet (reserved "of" >> ty_p)
                      >>= fun ty -> return (EbGen (varSymbol nm, ty)))))
      >>= fun (p1,e,p2) -> return (MarkEb(e,(p1,p2))))
     (reserved "and"))
  st
 
let conbind_p st = 
  (    (sepBy1 (    optional (reserved "op")
                >>  vid_p
                >>= fun nm -> optionRet (reserved "of" >> ty_p)
                >>= fun ty -> return (varSymbol nm,ty))
               (reservedOp "|"))
   >>= fun cs -> return (Constrs cs))
  st

let datbind_p st = 
  (sepBy1 
    (    pos_wrap (    option [] tyvarseq_p
                   >>= fun tyvars -> tycon_p
                   >>= fun tycon -> symbol "="
                   >>  conbind_p
                   >>= fun cons -> return (tycSymbol tycon, tyvars, cons))
     >>= fun (p1,(s,t,dbs),p2) -> return (MarkDb (Db (s,t,dbs),(p1,p2))))
    (reserved "and"))
  st

let typbind_p st =
  (sepBy1 
    (    pos_wrap (    option [] tyvarseq_p
                   >>= fun tyvars -> tycon_p
                   >>= fun tycon -> symbol "="
                   >>  ty_p
                   >>= fun ty -> return (tycSymbol tycon, tyvars, ty))
     >>= fun (p1,(s,tvs,ty),p2) -> return (MarkTb (Tb (s,ty,tvs),(p1,p2))))
    (reserved "and"))
  st


let rec fvalbind_p st = 
  (sepBy1
   (    pos_wrap (sepBy1 (    atpat_p 
                          >>= fun p -> many1 atpat_p 
                          >>= fun ps -> optionRet (colon >> ty_p)
                          >>= fun ty -> symbolChar '=' >> exp_p
                          >>= fun e -> return (Clause (p::ps,ty,e)))
                         (symbolChar '|'))
    >>= fun (p1,clauses,p2) -> return (MarkFb (Fb clauses, (p1,p2))))
   (reserved "and"))
  st

and valbind_p st = 
  (sepBy1 (    pos_wrap (    optionRet (many1 (reserved "rec"))
                         >>= fun r -> pat_p 
                         >>= fun p -> symbolChar '=' >> exp_p
                         >>= fun e ->
                              match r with
                                None -> return (Vb (p,e))
                              | Some _ -> return (Rvb (p,e)))
           >>= fun (p1,vb,p2) -> return (MarkVb (vb,(p1,p2))))
          (reserved "and"))
  st


(* declarations *)

and dec_p' st = 
 ((pos_wrap    
   (    (    reserved "val" 
         >>  option [] tyvarseq_p 
         >>= fun ts -> valbind_p 
         >>= fun vs -> return (ValDec (vs,ts)))

    <|> (    reserved "fun"
         >>  option [] tyvarseq_p
         >>= fun ts -> fvalbind_p
         >>= fun fs -> return (FunDec (fs,ts)))

    <|> (reserved "type" >> typbind_p >>= fun ts -> return (TypeDec ts))

    <|> (    reserved "datatype"
         >>  (    (    attempt datbind_p 
                   >>= fun ds -> option [] (reserved "withtype" >> typbind_p)
                   >>= fun ts -> return (DatatypeDec (ds,ts)))
              <|> (    tycon_p
                   >>= fun tc -> symbolChar '=' >> reserved "datatype"
                   >>  type_path_p
                   >>= fun tp -> return (DatatypeDec ([Db(tycSymbol tc,[],
                                                          Repl(tp))],
                                                      [])))))

    <|> (    reserved "abstype"
         >>  datbind_p
         >>= fun dbs -> option [] (reserved "withtype" >> typbind_p)
         >>= fun tps -> reserved "with" >> dec_p
         >>= fun d -> reserved "end" >> return (AbstypeDec (dbs,tps,d)))

    <|> (reserved "exception" >> app (fun x -> ExceptionDec x) exbind_p)

    <|> (    reserved "local"
         >>  dec_p
         >>= fun d1 -> reserved "in"
         >>  dec_p
         >>= fun d2 -> reserved "end" >> return (LocalDec (d1,d2)))

    <|> (    (    (reserved "infix" >> return infixleft)
              <|> (reserved "infixr" >> return infixright)
              <|> (reserved "nonfix" >> return (fun _ -> nonfix)))
         >>= fun f -> option 0 (    lexeme M.digit 
                                >>= fun c -> return (Char.code c - 48))
         >>= fun d -> many1 (app fixSymbol vid_p)
         >>= fun vs -> return (FixDec (f d,vs))))

   >>= fun (p1,d,p2) -> return (MarkDec (d,(p1,p2)))
  ) <?> "dec")
  st

and dec_p st =
  ((   sepBy1 dec_p' (optional semi)
   >>= fun ds -> match ds with
                   [d] -> return d
                 | _   -> return (SeqDec ds))
   <?> "declaration")
  st


(* match *)

and match_p st =
  ((sepBy1 ((   pat_p
            >>= fun p -> reservedOp "=>" >> exp_p 
            >>= fun e -> return (Rule (p,e))))
          (symbolChar '|')) <?> "cases")
  st


(* expressions *)

and atexp_p st =
  ( pos_wrap
    (    (    scon_p
          >>= fun sc -> return 
               (match sc with
                  IntScon i -> IntExp i
                | WordScon w -> WordExp w
                | RealScon f -> RealExp f
                | StringScon s -> StringExp s
                | CharScon c -> CharExp c))

     <|> (optional (reserved "op") >> vid_path_p >>= fun p -> return (VarExp p))
     
     <|> (braces (    commaSep (    lab_p 
                                >>= fun l -> symbolChar '=' >> exp_p
                                >>= fun e -> return (l,e))
                  >>= fun row -> return (RecordExp row)))

     <|> (reservedOp "#" >> lab_p >>= fun l -> return (SelectorExp l))

     <|> (parens (    (    exp_p
                       >>= fun e1 -> 
                            (    (    comma >> commaSep1 exp_p 
                                  >>= fun es -> return (TupleExp (e1::es)))
                             <|> (    semi >> semiSep1 exp_p
                                  >>= fun es -> return (SeqExp (e1::es)))
                             <|> (return e1)))
                  <|> return (RecordExp [])))
     
     <|> (brackets (commaSep exp_p) >>= fun es -> return (ListExp es))
                     
     <|> (    reserved "let" >> dec_p
          >>= fun d -> reserved "in" >> semiSep1 exp_p
          >>= fun es -> reserved "end" >> return (LetExp (d,es)))

    )
  >>= fun (p1,e,p2) -> return (MarkExp (e,(p1,p2))))
  st

and appexp_p st = 
  (    many1 atexp_p
   >>= fun ats ->
        match ats with
          [a] -> return a
        | _ -> return (FlatAppExp ats))
  st

and exp_p st =
  let table = 
    [[Postfix (colon >> app (fun t e -> ConstraintExp (e,t)) ty_p)];

     [Infix (AssocRight,
             reserved "andalso" >> return (fun e1 e2 -> AndalsoExp (e1,e2)))];
     
     [Infix (AssocRight,
             reserved "orelse" >> return (fun e1 e2 -> OrelseExp (e1,e2)))];
      
     [Postfix (reserved "handle" >> app (fun m e -> HandleExp (e,m)) match_p)];

     [Prefix (reserved "raise" >> return (fun e -> RaiseExp e))];

     [Prefix (    reserved "if" >> exp_p 
              >>= fun e1 -> reserved "then" >> exp_p
              >>= fun e2 -> reserved "else" 
              >>  return (fun e -> IfExp (e1,e2,e)))];

     [Prefix (    reserved "while" 
              >>  exp_p 
              >>= fun e1 -> reserved "do"
              >>  return (fun e -> WhileExp (e1,e)))];

     [Nonfix (    reserved "case"
              >>  exp_p
              >>= fun e -> reserved "of"
              >>  match_p
              >>= fun m -> return (CaseExp (e,m)))];

     [Nonfix (reserved "fn" >> match_p >>= fun m -> return (FnExp m))]
    ] 
  in
    ((buildExpressionParser table appexp_p) <?> "expression") st

(* specifications *)
let exdesc_p st =
  (sepBy1 
    (pos_wrap
      (    vid_p 
       >>= fun v -> optionRet (reserved "of" >> ty_p)
       >>= fun ty -> return (Exd (varSymbol v,ty)))
     >>= fun (p1,e,p2) -> return (MarkExd (e,(p1,p2))))
    (reserved "and"))
   st

let condesc_p st = 
  (sepBy1 
    (pos_wrap
       (    vid_p
        >>= fun nm -> optionRet (reserved "of" >> ty_p)
        >>= fun ty -> return (Cond (varSymbol nm,ty)))
     >>= fun (p1,e,p2) -> return (MarkCond (e,(p1,p2))))
    (reservedOp "|"))
  st

let datdesc_p st =
  (sepBy1 
    (pos_wrap 
      (    option [] tyvarseq_p
       >>= fun tyvars -> tycon_p
       >>= fun tycon -> symbol "=" >> condesc_p
       >>= fun conds -> return (tycSymbol tycon, tyvars, conds))
     >>= fun (p1,(s,t,cds),p2) ->
          return (MarkDatd (DatdClauses (s,t,cds),(p1,p2))))
    (reserved "and"))
  st

let typdesc_p st =
  (sepBy1 
    (pos_wrap
      (    option [] tyvarseq_p
       >>= fun tyvars -> tycon_p
       >>= fun tycon -> optionRet (symbol "=" >> ty_p)
       >>= fun ty -> return (tycSymbol tycon, tyvars, ty))
     >>= fun (p1,(tc,tv,t),p2) -> return (MarkTypd (Typd (tc,tv,t),(p1,p1))))
    (reserved "and"))
  st

let valdesc_p st = 
  (sepBy1
    (pos_wrap
      (    vid_p
       >>= fun v -> colon >> ty_p
       >>= fun t -> return (varSymbol v,t))
     >>= fun (p1,(s,t),p2) -> return (MarkVald (Vald (s,t),(p1,p2))))
    (reserved "and"))
  st


let rec strdesc_p st = 
  (sepBy1
    (pos_wrap
      (    identifier
       >>= fun strid -> colon >> sigexp_p
       >>= fun sigexp -> return (strSymbol strid,sigexp))
     >>= fun (p1,(sid,sexp),p2) -> return (MarkStrd (Strd (sid,sexp), (p1,p2))))
    (reserved "and"))
  st
  

and spec_p st =
  let simplespec st = 
    (pos_wrap 
      (    (reserved "val" >> valdesc_p >>= fun vl -> return (ValSpec vl))
     
       <|> (    reserved "type" >> typdesc_p 
            >>= fun tl -> return (TycSpec (tl, false)))
       
       <|> (    reserved "eqtype" >> typdesc_p
            >>= fun tl -> return (TycSpec (tl, true)))
           
       <|> (    reserved "datatype"
            >>  (    (    (attempt datdesc_p)
                      >>= fun ds -> return (DataDefSpec ds))
                 <|> (    tycon_p
                      >>= fun tc -> reservedOp "=" >> reserved "datatype" 
                      >>  type_path_p
                      >>= fun path -> 
                           return (DataReplSpec (tycSymbol tc,path)))))

       <|> (reserved "exception" >> exdesc_p >>= fun el -> return (ExceSpec el))

       <|> (    reserved "structure" >> strdesc_p
            >>= fun sl -> return (StrSpec sl))
      )
     >>= fun (p1,s,p2) -> return (MarkSpec (s,(p1,p2))))
    st
  in
  let reduce specs =
    match specs with
      [] -> SeqSpec []
    | [s] -> s
    | _ -> SeqSpec specs
  in

  (* this gets messy to resolve an ambiguity in the definition *)
  let simpleseq st = 
    (app (fun specs -> reduce (List.concat specs)) 
         (many (sepBy1 (simplespec <?> "sig member") semi))) st
  in
  let sharingdec st =
    (    (reserved "sharing")
     >>  (    (    reserved "type"
               >>  type_path_p
               >>= fun t1 -> reservedOp "="
               >>  sepBy1 type_path_p (reservedOp "=")
               >>= fun ts -> return (fun s -> ShareTycSpec (s,(t1::ts))))
          <|> (    str_path_p
               >>= fun s1 -> reservedOp "="
               >>  sepBy1 str_path_p (reservedOp "=")
               >>= fun ss -> return (fun s -> ShareStrSpec (s,(s1::ss))))))
    st
  in
    (chainl simpleseq (    sharingdec 
                       >>= fun f -> optional semi >> 
                            return (fun l r -> 
                                     match r with 
                                       SeqSpec ss -> SeqSpec ((f l)::ss)
                                     | _ -> SeqSpec [f l;r]))
            (SeqSpec []))
                                     
    st

(* signatures *)

and sigexp_p st =
  let simplesig st =
    (pos_wrap
      (    (    reserved "sig" >> spec_p
            >>= fun specs -> reserved "end" 
            >>  match specs with
                  SeqSpec s -> return (BaseSig s)
                | _ -> return (BaseSig [specs]))
       <|> (identifier >>= fun i -> return (VarSig (sigSymbol i))))
     >>= fun (p1,s,p2) -> return (MarkSig (s,(p1,p2))))
    st
  in
  let wherespec_p wh = 
         (    wh >> reserved "type"
          >>  option [] tyvarseq_p
          >>= fun tyvs -> type_path_p
          >>= fun path -> reservedOp "=" >> ty_p
          >>= fun ty -> return (path, tyvs, ty))
  in
   (pos_wrap
     (    simplesig
      >>= fun s -> 
           (option []
              (    wherespec_p (reserved "where")
               >>= fun w1 -> many (wherespec_p (    (reserved "where")
                                                <|> (reserved "and")))
               >>= fun ws -> return (w1::ws)))
      >>= fun ws -> return (s,ws))
    >>= fun (p1,(s,ws),p2) -> 
         match ws with [] -> return s
                     | _ -> return (MarkSig (AugSig (s,ws),(p1,p2))))
   st
         

let sigdec_p st =
  (    reserved "signature"
   >>  (sepBy1
         (pos_wrap
           (    identifier
            >>= fun sid -> reservedOp "=" >> sigexp_p
            >>= fun sexp -> return (Sigb (sigSymbol sid, sexp)))
          >>= fun (p1,s,p2) -> return (MarkSigb (s,(p1,p2))))
        (reserved "and")))
  st

(* structures *)
let ascrip_p st =
  (    (    reservedOp ":" >> sigexp_p 
        >>= fun se -> return (fun s -> ConstrainedStr (s,Transparent se)))
   <|> (    reservedOp ":>" >> sigexp_p
        >>= fun se -> return (fun s -> ConstrainedStr (s,Opaque se))))
  st


let rec strexp_p st =
  let simpleexp_p st = 
    (pos_wrap
      (    (    reserved "struct" >> (strdec_p <|> return [])
            >>= fun sd -> reserved "end" >> return (BaseStr sd))

       (* XXX can end up parsing path twice *)
       <|> (    attempt (    fct_path_p
                         >>= fun p -> symbolChar '(' >> return p)
            >>= fun p -> 
                  (many (    strexp_p 
                         <|> (strdec_p >>= fun sd -> return (BaseStr sd))))
            >>= fun se -> symbolChar ')' >> return (AppStr (p,se)))
     
       <|> (str_path_p >>= fun v -> return (VarStr v))
          
       <|> (    reserved "let" >> strdec_p
            >>= fun sd -> reserved "in" >> strexp_p 
            >>= fun se -> reserved "end" >> return (LetStr (sd,se)))
      )
     >>= fun (p1,s,p2) -> return (MarkStr (s,(p1,p2))))
    st
  in
    (    simpleexp_p 
     >>= fun se -> foldl ascrip_p se)
    st
      
and strbind_p st = 
  (sepBy1
    (pos_wrap
      (    identifier
       >>= fun i -> option (fun x -> x) ascrip_p
       >>= fun af -> reservedOp "=" >> strexp_p
       >>= fun se -> return (Strb (strSymbol i, af se)))
     >>= fun (p1,sb,p2) -> return (MarkStrb (sb,(p1,p2))))
    (reserved "and"))
  st
 

and strdec_p st =
  let simpledec_p st = 
    (    (dec_p' >>= fun d -> return (DecStrD d))

     <|> (reserved "structure" >> strbind_p >>= fun sb -> return (DefStrD sb))

     <|> (    reserved "local" >> strdec_p 
          >>= fun sd1 -> reserved "in" >> strdec_p
          >>= fun sd2 -> reserved "end" >> return (LocalStrD (sd1,sd2))))
    st
  in
    (sepBy1 simpledec_p (optional semi))
    st


(* functors *)
let fctarg_p st =
    (    (    identifier
          >>= fun sid -> colon >> sigexp_p
          >>= fun sexp -> return (FctVarArg (strSymbol sid,sexp)))
     <|> (    (    spec_p
               >>= fun s -> 
                    match s with SeqSpec [] -> mzero 
                               | _ -> return (FctSpecArg s))))
    st

let fctdec_p st =
  (    reserved "functor"
   >>  sepBy1 (pos_wrap
                (    identifier
                 >>= fun fid -> parens (many fctarg_p)
                 >>= fun args -> option (fun x -> x) ascrip_p 
                 >>= fun af -> reservedOp "=" >> strexp_p
                 >>= fun def -> return (Fctb (fctSymbol fid, args, af def)))
               >>= fun (p1,f,p2) -> return (MarkFctb (f,(p1,p2))))
              (reserved "and"))
  st


(* topdec, program *)
let topdec_p st = 
  ((many1 (    (strdec_p >>= fun sd -> return (TopStrDec sd))
           <|> (sigdec_p >>= fun sd -> return (TopSigDec sd))
           <|> (fctdec_p >>= fun fd -> return (TopFctDec fd))))
   <?> "topdec")
  st

let program_p st =
  (sepBy1 
     ((    topdec_p 
      <|> (    exp_p 
           >>= fun e ->
                let itb = Vb (VarPat ([varSymbol "it"],NoOp),e) in
                return [TopStrDec [DecStrD (ValDec ([itb],[]))]]))
       <?> "topdec or exp")
     semi)
  st




let string_to_type s =
    match parse "type" (LazyList.M.ofString s) 
          (ty_p >>= fun r -> eof >> return r) with
        Success x -> Some x
      | Failure err -> print_string (Error.M.errorToString err); None

let string_to_pat s =
    match parse "pat" (LazyList.M.ofString s) 
          (pat_p >>= fun r -> eof >> return r) with
        Success x -> Some x
      | Failure err -> print_string (Error.M.errorToString err); None

let string_to_dec s =
    match parse "dec" (LazyList.M.ofString s) 
          (dec_p >>= fun r -> eof >> return r) with
        Success x -> Some x
      | Failure err -> print_string (Error.M.errorToString err); None

let string_to_exp s =
    match parse "exp" (LazyList.M.ofString s) 
          (exp_p >>= fun r -> eof >> return r) with
        Success x -> Some x
      | Failure err -> print_string (Error.M.errorToString err); None

let string_to_sigexp s =
    match parse "sigexp" (LazyList.M.ofString s) 
          (sigexp_p >>= fun r -> eof >> return r) with
        Success x -> Some x
      | Failure err -> print_string (Error.M.errorToString err); None

let string_to_strdec s =
    match parse "strdec" (LazyList.M.ofString s) 
          (strdec_p >>= fun r -> eof >> return r) with
        Success x -> Some x
      | Failure err -> print_string (Error.M.errorToString err); None

let string_to_program s =
    match parse "program" (LazyList.M.ofString s) 
          (program_p >>= fun r -> eof >> return r) with
        Success x -> Some x
      | Failure err -> print_string (Error.M.errorToString err); None

let file_to_program f =
    match parse "program" (LazyList.M.ofChannel (open_in f)) 
          (whiteSpace >> program_p >>= fun r -> eof >> return r) with
        Success x -> Some x
      | Failure err -> print_string (Error.M.errorToString err); None
