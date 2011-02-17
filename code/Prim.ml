module type S =
sig

  (* The Prim signature defines the types of the basic functions and 
     combinators associated with general parsers *)

  module Tok : Token.S

  (* This is the type of parsers, parameterized on the type of input,
     user definable state, and return type *)
  type 'a state
  type ('a,'b) rcon
  type ('st, 'a) parser = 'st state -> ('st,'a) rcon
    
  type 'a parse_output = Success of 'a | Failure of Error.M.parse_error
    
  (* monadic fun *)
  val return : 'a -> ('st, 'a) parser
      
  val (>>=)  : ('st, 'a) parser -> ('a -> ('st, 'b) parser) -> 
               ('st, 'b) parser
    
  val mzero  : ('st, 'a) parser
    
  val mplus  : ('st, 'a) parser -> ('st, 'a) parser -> 
               ('st, 'a) parser

  val (>>)   : ('st, 'a) parser -> ('st, 'b) parser ->
               ('st, 'b) parser

  (* this takes a ('st, 'a) parser lazy, and creates a ('st, 'a)
   * parser, where the original will only be parsed once state is passed in *)
  val parseLazy : ('st, 'a) parser Lazy.t -> ('st, 'a) parser

  (* other simple parsing operators *)
    
  (* <?> creates a label to be used in error messages *)
  val (<?>)   : ('st, 'a) parser -> string -> ('st, 'a) parser
      
  (* <|> is the choice operator  For parsers p and q, p <|> q is a new
     parser which tries p first, and then q *)
  val (<|>)   : ('st, 'a) parser -> ('st, 'a) parser -> ('st, 'a) parser
    
  (* attempt causes a parser to use infinite lookahead (parsec's "try")*)
  val attempt    : ('st, 'a) parser -> ('st, 'a) parser
    
  (* peek p succeeds if p would succeed, but consumes no input.  This
   * is parsec's Combinator.lookAhead *)
  val peek       : ('st, 'a) parser -> ('st, 'a) parser

  (* primative token recognizers *)
  val token   : (Tok.t -> 'a option) -> ('st, 'a) parser

  val tokenPrimEx : (Pos.M.pos -> Tok.t -> Tok.t LazyList.M.l_list -> 
                       'st -> 'st) option ->
                    (Tok.t -> 'a option) ->
                    ('st, 'a) parser

  (* positions *)
  val getPosition : ('st,Pos.M.pos) parser

  val setPosition : Pos.M.pos -> ('st,unit) parser



  val unexpected : string -> ('st,'a) parser
      
  val fail    : string -> ('st, 'a) parser

  val getState    : ('st, 'st) parser

  val setState    : 'st -> ('st, unit) parser

  val updateState : ('st -> 'st) -> ('st, unit) parser

  (* parsers unfolded for space *)
  (* As the Parsec folks explain, many _can_ be implemented in terms of
   * the more primative parsers, but for the best space efficiency,
   * it should be defined with access to the parser type *)
  val many : ('st, 'a) parser -> ('st, 'a list) parser

  (* if one is true, then the empty sequence is not accepted *) 
  val manyArrOp : ('st, 'a) parser -> one:bool -> ('st, 'a array) parser

  val manyColOp : ('st, 'a) parser -> one:bool ->
                  make:(int -> 'col) -> set:('col -> int -> 'a -> unit) ->
                  get:('col -> int -> 'a) -> length:('col -> int) -> 
                  ('st, 'col) parser

  val skipMany : ('st, 'a) parser -> ('st, unit) parser


  (* parsers unfolded for speed *)
  (* similar *)
  val tokens : Tok.t list -> ('st,Tok.t list) parser

  val tokenArr : Tok.t array -> ('st, Tok.t array) parser

 (* take in an ordered collection of tokens, a way to access the collection,
  * and a length.  0 should be first index *)
  val tokenCol : 'a -> ('a -> int -> Tok.t) -> int -> ('st,'a) parser


  (* val tokenArr : Tok.t array -> ('st,Tok.t array) parser *)

  (* running a parser *)

  val parse : string -> Tok.t LazyList.M.l_list -> (unit, 'a) parser -> 
              'a parse_output

  val parseSt : string -> Tok.t LazyList.M.l_list -> 'st -> 
                ('st, 'a) parser -> 'a parse_output


end



module M (T : Token.S) : S with type Tok.t = T.t =
struct

  open Error.M
  open LazyList.M
  

  module Tok = T

  type 'a consumed =
      Consumed of 'a Lazy.t
    | Empty of 'a

  type 'st state = { s_input : Tok.t l_list;
                     s_pos   : Pos.M.pos;
                     s_user  : 'st}

  type ('st,'a) result =
      Ok of 'a * 'st state * parse_error
    | Error of parse_error

  type ('st,'a) rcon = ('st,'a) result consumed

  type ('st, 'a) parser = 'st state -> ('st, 'a) rcon
    
  type 'a parse_output = Success of 'a | Failure of parse_error

  (* Generic helper functions *)
  (* Most of this follows parsec *)

  let unknownError state = newErrorUnknown (state.s_pos)

  let setExpectErrors err = function
      []      -> setErrorMessage (Expect "") err
    | [msg]   -> setErrorMessage (Expect msg) err
    | (m::ms) -> 
        List.fold_right (fun msg err -> addErrorMessage (Expect msg) err)
                        ms (setErrorMessage (Expect m) err) 

  let mergeErrorResult err1 result =
    match result with
        Ok (x,state,err2) -> Ok (x,state,(mergeError err1 err2))
      | Error err2        -> Error (mergeError err1 err2)


  (* The library functions *)
  (* again, this mostly follows parsec *)

  let return x = (fun state -> Empty (Ok (x,state, unknownError state)))

  let (>>=) p f =
    (fun state ->
      match p state with
          Consumed result1 -> 
            (* the laziness is key here; result1 isn't evaled *)
            Consumed (lazy
              (match Lazy.force result1 with
                  Ok (x,state1,err1) ->
                    (match (f x) state1 with
                        Empty result2   -> mergeErrorResult err1 result2
                      | Consumed result2 -> Lazy.force result2)
                         (* this force is OK, because if the fact that the
                          * parser consumes data was enough, we wouldn't
                          * have made it here yet (since the first parser
                          * also consumed data) *)
                | Error e            -> Error e))
        | Empty result1 ->
            (match result1 with
                Ok (x,state1,err1) ->
                  (match (f x) state1 with
                      Empty result2 -> Empty (mergeErrorResult err1 result2)
                    | consumed         -> consumed)
              | Error err1 -> Empty (Error err1))
    )
           
  let mzero = fun state -> Empty (Error (unknownError state))
    
  let mplus p1 p2 = 
    fun state ->
      match (p1 state) with
          Empty (Error err) -> 
            (match (p2 state) with
                Empty result -> Empty (mergeErrorResult err result)
              | consumed     -> consumed)
        | consumed -> consumed

  let (>>) p1 p2 = p1 >>= (fun _ -> p2)

  let parseLazy p =
    fun state -> (Lazy.force p) state


  (* other simple parsing operators *)
      
  (* this is needed for <?> *)
  let labels p msgs =
    fun state ->
      match p state with
          Empty result -> 
            Empty (match result with
                Error err -> Error (setExpectErrors err msgs)
              | Ok (x,state1,err) ->
                  if errorIsUnknown err then result
                  else Ok (x,state1,(setExpectErrors err msgs)))
        | other       -> other


  (* <?> creates a label to be used in error messages *)
  let (<?>) p m = labels p [m]
    
  (* <|> is the choice operator.  For parsers p and q, p <|> q is a new
     parser which tries p first, and then q *)
  let (<|>) p1 p2 = mplus p1 p2
    
  (* attempt causes a parser to use infinite lookahead (parsec's "try")*)
  let attempt p =
    fun state -> 
      match p state with
          Consumed delayed_result ->
            (match Lazy.force delayed_result with
                (Error err) -> Empty (Error (setErrorPos state.s_pos err))
              | Ok _        -> Consumed delayed_result)
              (* Lazy struct garuntees this result is memoized *)
        | empty -> empty
    
  let peek p =
    fun state ->
      match p state with
          Consumed delayed_result ->
            (match Lazy.force delayed_result with
                Error err -> Empty (Error (setErrorPos state.s_pos err))
              | Ok (r,st,err) -> Empty (Ok(r,state,err)))
        | e -> e

  (* primative token recognizers *)
  let tokenPrimEx nextstate test =
    let sysUnExpectError m p = Error (newErrorMessage (SysUnExpect m) p) in
      fun {s_input=s_input;s_pos=s_pos;s_user=s_user} ->
        (match expose s_input with
            None         -> Empty (sysUnExpectError "" s_pos)
          | Some (c,cs)  -> 
              (match test c with
                  None -> Empty (sysUnExpectError 
                                   ("\"" ^ Tok.toString c ^ "\"") s_pos)
                | Some x ->
                    Consumed (lazy
                      (let newpos = Tok.nextPos s_pos c in
                       let newuser = 
                         match nextstate with
                             None -> s_user
                           | Some f -> f s_pos c cs s_user
                       in
                       let newstate = {s_input=cs;s_pos=newpos;
                                       s_user=newuser} 
                       in
                         Ok (x,newstate,newErrorUnknown newpos))))
        )
           
                    

  let token test = tokenPrimEx None test


  (* positions *)
  let getPosition st = Empty (Ok (st.s_pos,st,unknownError st))

  let setPosition p {s_input=s_input;s_user=s_user} = 
    let newstate = {s_input=s_input;s_pos=p;s_user=s_user} in
    Empty (Ok ((),newstate,unknownError newstate))

  let unexpected msg =
    (fun state -> 
      Empty (Error (newErrorMessage (UnExpect msg) state.s_pos)))


  let fail msg = 
    fun state -> Empty (Error (newErrorMessage (Message msg) state.s_pos))

  (* parser state combinators *)
  let updateParserState f =
    fun state -> 
      let newstate = f state
      in Empty (Ok (state,newstate,unknownError newstate))
        
  let setParserState st = updateParserState (fun _ -> st)

                                            

  (* user state *)
  let getState = fun state ->
    Empty (Ok (state.s_user,state,unknownError state))

  let setState st = fun {s_input=s_input;s_pos=s_pos} -> 
    let newstate = {s_input=s_input;s_pos=s_pos;s_user=st} in
      Empty (Ok ((),newstate,unknownError newstate))
        
  let updateState f =
    fun {s_input=s_input;s_pos=s_pos;s_user=s_user} -> 
      let new_user = f s_user in
      let newstate = {s_input=s_input;s_pos=s_pos;s_user=new_user} in
        Empty (Ok ((),newstate,unknownError newstate))




  (* Parsers unfolded for space *)

  let manyAccum accum p =
    fun state ->
      match p state with
          Empty result -> 
            (match result with
                Ok (x,state',err) -> 
                  failwith ("pcl: combinator many is applied to a parser that "
                          ^ "accepts an empty string.")
              | Error err -> Empty (Ok ([],state,err)))
        | consumed ->
            let rec walk xs state r =
              match r with
                  Empty (Error err) -> Ok (xs, state, err)
                | Empty ok          -> failwith
                    ("pcl: combinator many is applied to a parser that "
                   ^ "accepts an empty string.")
                | Consumed result ->
                    (match Lazy.force result with
                        Error err -> Error err
                      | Ok (x,state',err) ->
                          let ys = accum x xs in
                            (walk ys state' (p state')))
            in
              Consumed (lazy (walk [] state consumed))

  let many p =
    (manyAccum (fun x xs -> x :: xs) p) >>= (fun xs -> return (List.rev xs))

  let skipMany p =
    (manyAccum (fun _ _ -> []) p) >>= (fun _ -> return ())

  let manyArrOp p ~one =
    let pos = ref 0 in
    let index = ref 0 in
    let index_arr = ref (Array.make 4 [||]) in
    let len = ref 0 in
    let total_len = ref 0 in
    let insert t =
      if !len = 0 then 
        ((!index_arr).(0) <- Array.make 8 t; pos := 1; len := 8)
      else if !pos = !len then
        (let newlen = 2 * !len in
         let ind_len = Array.length !index_arr in
           total_len := !len + !total_len;
           index := !index + 1;
           if !index = ind_len then
             (index_arr := Array.init (2*ind_len) 
                 (fun n -> (!index_arr).(n mod ind_len)));
           (!index_arr).(!index) <- Array.make newlen t;
           pos := 1;
           len := newlen)
      else
        ((!index_arr).(!index).(!pos) <- t;
         pos := !pos + 1)
    in
      fun state ->
        match p state with
            Empty result -> 
              (match result with
                  Ok (x,state',err) -> 
                    failwith ("pcl: combinator manyArrOp is applied to a " ^
                              "parser that accepts an empty string.")
                | Error err -> 
                    if one then Empty (Error err) else 
                      Empty (Ok ([||],state,err)))
          | consumed ->
              let rec walk state r = 
                match r with
                    Empty (Error err) -> 
                      let count = ref 0 in
                      let scan_ind = ref 0 in
                      let access n = 
                        let arr = (!index_arr).(!scan_ind) in
                        let i = n - !count in
                          if i < (Array.length arr) then
                            arr.(i)
                          else
                            (count := !count + (Array.length arr);
                             scan_ind := !scan_ind + 1;
                             ((!index_arr).(!scan_ind)).(0))
                      in                             
                        Ok (Array.init (!total_len + !pos) access,
                            state, err)
                  | Empty ok          -> failwith
                      ("pcl: combinator manyArrOp is applied to a parser that "
                     ^ "accepts an empty string.")
                  | Consumed result ->
                      (match Lazy.force result with
                          Error err -> Error err
                        | Ok (x,state',err) ->
                            insert x; walk state' (p state'))
              in
                Consumed (lazy (walk state consumed))


  let manyColOp p ~one ~make ~set ~get ~length =
    let pos = ref 0 in
    let index = ref 0 in
    let index_arr = ref (Array.make 4 (make 0)) in
    let len = ref 16 in
    let total_len = ref 0 in
    !index_arr.(0) <- make 16;
    let insert t =
      if !pos = !len then
        (let newlen = 2 * !len in
         let ind_len = Array.length !index_arr in
         let newcol = make newlen in
           total_len := !len + !total_len;
           index := !index + 1;
           if !index = ind_len then
             (index_arr := Array.init (2*ind_len) 
                 (fun n -> (!index_arr).(n mod ind_len)));
           set newcol 0 t;            
           (!index_arr).(!index) <- newcol;
           pos := 1;
           len := newlen)
      else
        (set !index_arr.(!index) !pos t;
         pos := !pos + 1)
    in
      fun state ->
        match p state with
            Empty result -> 
              (match result with
                  Ok (x,state',err) -> 
                    failwith ("pcl: combinator manyColOp is applied to a "
                            ^ " parser that accepts an empty string.")
                | Error err -> 
                    if one then Empty (Error err) else 
                      Empty (Ok (make 0,state,err)))
          | consumed ->
              let rec walk state r = 
                match r with
                    Empty (Error err) -> 
                      let count = ref 0 in
                      let scan_ind = ref 0 in
                      let access n = 
                        let col = (!index_arr).(!scan_ind) in
                        let col_len = length col in
                        let i = n - !count in
                          if i < col_len then
                            get col i
                          else
                            (count := !count + col_len;
                             scan_ind := !scan_ind + 1;
                             get !index_arr.(!scan_ind) 0)
                      in
                      let size = (!total_len + !pos) in
                      let ret = make size in
                      for i = 0 to size-1 do
                        set ret i (access i)
                      done;
                      Ok (ret, state, err)
                  | Empty ok          -> failwith
                      ("pcl: combinator manyColOp is applied to a parser that "
                     ^ "accepts an empty string.")
                  | Consumed result ->
                      (match Lazy.force result with
                          Error err -> Error err
                        | Ok (x,state',err) ->
                            insert x; walk state' (p state'))
              in
                Consumed (lazy (walk state consumed))


(* for speed *)

  let tokens s =
    fun {s_input=s_input;s_pos=s_pos;s_user=s_user} ->
      let print_list s = 
        match s with
            [] -> ""
          | s ->
              let rec p_help s =
                (match s with
                    [] -> "\"" (* should never hit *)
                  | (c::[]) -> (Tok.toString c) ^ "\""
                  | (c::cs) -> (Tok.toString c) ^ (p_help cs))
              in
                "\"" ^ (p_help s)
      in
      let ok cs =
        let newpos = List.fold_left Tok.nextPos s_pos s in
        let newstate = {s_input=cs;s_pos=newpos;s_user=s_user} in
          Ok (s,newstate,newErrorUnknown newpos)
      in
      let errEof () = Error (setErrorMessage (Expect (print_list s))
                                (newErrorMessage (SysUnExpect "") s_pos))
      in
      let errExpect c = 
        Error (setErrorMessage (Expect (print_list s))
                  (newErrorMessage (SysUnExpect 
                                      ("\"" ^ Tok.toString c ^ "\"")) s_pos))
      in
      let rec walk xs cs =
        match (xs,expose cs) with
            ([],_) -> ok cs
          | (xs,None) -> errEof ()
          | (x::xs,Some(c,cs)) ->
              if x = c then walk xs cs
              else errExpect c
      in
      let walk1 xs cs =
        match (xs,expose cs) with
            ([],_) -> Empty (ok cs)
          | (xs,None) -> Empty (errEof ())
          | (x::xs,Some(c,cs)) ->
              if x = c then Consumed (lazy (walk xs cs))
              else Empty (errExpect c)
      in
        walk1 s s_input
    

  let tokenCol col acc length =
    fun {s_input=s_input;s_pos=s_pos;s_user=s_user} ->
      let getTok = acc col in
      let print_col () = 
        if length = 0 then ""
        else
          let rec p_help k =
            if k = length then "\""
            else Tok.toString (getTok k) ^ (p_help (k+1))
          in
            "\"" ^ (p_help 0)
      in
      let ok cs pos =
        let newstate = {s_input=cs;s_pos=pos;s_user=s_user} in
          Ok (col,newstate,newErrorUnknown pos)
      in
      let errEof () = Error (setErrorMessage (Expect (print_col ()))
                                (newErrorMessage (SysUnExpect "") s_pos))
      in
      let errExpect c = 
        Error (setErrorMessage (Expect (print_col ()))
                  (newErrorMessage (SysUnExpect 
                                      ("\"" ^ Tok.toString c ^ "\"")) s_pos))
      in
      let rec walk ind cs pos =
        if ind = length then
          ok cs pos
        else
          match expose cs with
            None -> errEof ()
          | Some (c,cs) ->
              let t = getTok ind in
                if t = c then walk (ind+1) cs (Tok.nextPos pos t)
                else errExpect c
      in
      let walk1 cs =
        if length  = 0 then
          Empty (ok cs s_pos)
        else
          match expose cs with
            None -> Empty (errEof ())
          | Some(c,cs) ->
              let t = getTok 0 in
                if t = c then 
                  Consumed (lazy (walk 1 cs (Tok.nextPos s_pos t)))
                else Empty (errExpect c)
      in
        walk1 s_input

  let tokenArr ar = tokenCol ar Array.get (Array.length ar)

(* running a parser *)

  let parseSt name toks st p = 
    let result =
      match p {s_user=st;s_pos=Pos.M.init_pos name;s_input=toks} with
          Consumed result -> Lazy.force result
        | Empty result -> result
    in
      match result with
          Ok (x,_,err) -> Success x
        | Error err  -> Failure err
            
  let parse name toks p =
    parseSt name toks () p

end
