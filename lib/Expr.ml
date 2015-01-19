module type S =
sig

  module P : Prim.S

  type assoc = AssocNone | AssocLeft | AssocRight

  type ('st,'a) operator = Infix of assoc * ('st,'a -> 'a -> 'a) P.parser
                   | Prefix of ('st, 'a -> 'a) P.parser
                   | Postfix of ('st, 'a -> 'a) P.parser
                   | Nonfix of ('st,'a) P.parser


  (* Nonfix can be used to run an arbitrary parser which has the same
   * return type as the rest of the parsers.  This is convenient to have
   * here for reasons of precedence.  The parser may, in particular, make
   * recursive calls to the generated expression parser if the table
   * is defined in such a scope as to have access to this parser.
   * However, if it begins or ends with a recursive call to this
   * expression parser, it will break the appropriate precedence
   * rules.  In those cases, Infix, Prefix, or Postfix should be
   * used.  This design suggested by Phil Wadler.
   *
   * This useful if you had to parse, for example, the
   *    "case <exp> of <match>" SML construction, which isn't
   * obviously Prefix Postfix or Infix
   *
   * If Parsec's original expression parser is enough for you, you can
   * just ignore the NonFix constructor *)


  type ('st,'a) operator_table  = ('st, 'a) operator list list

  val buildExpressionParser : ('st,'a) operator_table
                           -> ('st,'a) P.parser
                           -> ('st,'a) P.parser

end



module M (P : Prim.S) :
  S with type 'a P.state = 'a P.state
    and type ('a,'b) P.rcon = ('a,'b) P.rcon
    and type P.Tok.t = P.Tok.t =
struct

  module P = P
  open P

  module Comb = Combinator.M (P)
  open Comb

  type assoc = AssocNone | AssocLeft | AssocRight

  type ('st,'a) operator = Infix of assoc * ('st,'a -> 'a -> 'a) P.parser
                   | Prefix of ('st, 'a -> 'a) P.parser
                   | Postfix of ('st, 'a -> 'a) P.parser
                   | Nonfix of ('st,'a) P.parser

  type ('st,'a) operator_table  = ('st, 'a) operator list list

  let buildExpressionParser operators simpleExpr =
    let makeParser term ops =

      let (rassoc,lassoc,nassoc,prefix,postfix,nonfix) =
        let splitOp op (ra,la,na,pre,post,non) =
          match op with
            Infix (ass,op) ->
              (match ass with
                 AssocNone -> (ra,la,op::na,pre,post,non)
               | AssocLeft -> (ra,op::la,na,pre,post,non)
               | AssocRight -> (op::ra,la,na,pre,post,non))
          | Prefix op -> (ra,la,na,op::pre,post,non)
          | Postfix op -> (ra,la,na,pre,op::post,non)
          | Nonfix op -> (ra,la,na,pre,post,op::non)
        in
          List.fold_right splitOp ops ([],[],[],[],[],[])
      in



      let (raOp,laOp,naOp,preOp,postOp,nonOp) =
        (choice rassoc, choice lassoc, choice nassoc, choice prefix <?> "",
         choice postfix <?> "", choice nonfix <?> "")
      in

      let (ambigR,ambigL,ambigN) =
        let ambiguous assoc op =
          attempt (    op
                    >>  fail ("ambiguous use of a " ^ assoc
                               ^ "associative operator"))
        in
          (ambiguous "right" raOp, ambiguous "left" laOp, ambiguous "non" naOp)
      in

      (* let id x = x in *)

      let termP st =
         (    (many preOp)
          >>= fun pre -> (term <|> nonOp)
          >>= fun x -> (many postOp)
          >>= fun post ->
          return (
            let x = List.fold_right (fun f x -> f x) pre x in
            let x = List.fold_left (fun x f -> f x) x post in
            x)
         ) st
      in

      let rec rassocP x =
        (    (    raOp
              >>= fun f -> (termP >>= fun z -> (rassocP z <|> return z))
              >>= fun y -> return (f x y))
         <|> ambigL
         <|> ambigR)
      in

      let rec lassocP x =
        (    (    laOp
              >>= fun f -> termP
              >>= fun y -> let r = f x y in (lassocP r <|> return r))
         <|> ambigR
         <|> ambigN)
      in

      let rec nassocP x =
        (    naOp
         >>= fun f -> termP
         >>= fun y -> (ambigR <|> ambigL <|> ambigN <|> return (f x y)))
      in
        (    termP
         >>= fun x -> (rassocP x <|> lassocP x <|> nassocP x <|> return x))
        <?> "operator"
    in
      List.fold_left makeParser simpleExpr operators

end
