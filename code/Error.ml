module type S =
sig

  (* This is straight out of parsec.  Seems reasonable for now *)
  type message = 
      SysUnExpect of string (* library generated badness *)
    | UnExpect    of string (* unexpected something *)
    | Expect      of string (* expecting something *)
    | Message     of string (* raw message *)
        
  type parse_error


  val errorPos       : parse_error -> Pos.M.pos

  val errorMessages  : parse_error -> message list

  val errorIsUnknown : parse_error -> bool


  val newErrorUnknown : Pos.M.pos -> parse_error

  val newErrorMessage : message -> Pos.M.pos -> parse_error

  val addErrorMessage : message -> parse_error -> parse_error

  val setErrorPos     : Pos.M.pos -> parse_error -> parse_error

  val setErrorMessage : message -> parse_error -> parse_error

  val mergeError      : parse_error -> parse_error -> parse_error

  (* this is for users of pcl *)
  val errorToString   : parse_error -> string

  (* this if for debugging *)
  val printError : Format.formatter -> parse_error -> unit

end



module M : S =
struct   
  (*** types ***)

  (* This is straight out of parsec.  Seems reasonable for now *)
  type message = 
      SysUnExpect of string (* library generated badness *)
    | UnExpect    of string (* unexpected something *)
    | Expect      of string (* expecting something *)
    | Message     of string (* raw message *)
        
  type parse_error = Pos.M.pos * message list
             
  (*** Some helper functions on messages ***)
   
  let message_to_enum m =
    match m with
        SysUnExpect _ -> 0
      | UnExpect _    -> 1
      | Expect _      -> 2
      | Message _     -> 3

  let msg_eq m1 m2 = (message_to_enum m1) = (message_to_enum m2)

  let msg_compare m1 m2 = (message_to_enum m1) - (message_to_enum m2)

  let get_string m =
    match m with
        SysUnExpect s -> s
      | UnExpect s    -> s
      | Expect s      -> s
      | Message s     -> s


  (*** Structure Implementation ***)
    
  let errorPos ((pos,_) : parse_error) = pos

  let errorMessages (_,ms) = List.sort msg_compare ms

  let errorIsUnknown (_,m) = m = []
    

  let newErrorUnknown pos = (pos, [])

  let newErrorMessage m pos = (pos, [m])

  let addErrorMessage m (pos,ms) = (pos, m::ms)

  let setErrorPos pos (_,ms) = (pos,ms)

  (* this makes me wish ocaml had a one-character composition operator
   * like sml or haskell *)
  let setErrorMessage m (pos,ms) = 
    (pos, m::(List.filter (fun x -> not (msg_eq m x)) ms))

  let mergeError (pos,ms) (_,ms') = (pos,ms@ms')

  let printError f (p,ms) =
    let m_to_s = function
        SysUnExpect s -> " SysUnExpect : " ^ s ^ "\n"
      | UnExpect s    -> " UnExepect : " ^ s ^ "\n"
      | Expect s      -> " Expect : " ^ s ^ "\n"
      | Message s     -> " Message : " ^ s ^ "\n"
    in
      Format.fprintf f "Pos: %s \n" (Pos.M.toString p);
      List.iter (fun m -> Format.fprintf f "%s" (m_to_s m)) ms
    
    

  (* parsec sets this up to handle several languages, but only does english!
   * I'm not going to bother, for now. *)
  let errorToString err = 
    let ms = errorMessages err in
      if [] = ms then "unknown parse error" else

    let rec takewhile p gs = 
      function [] -> (gs,[])
             | ((m::ms) as rs) -> 
                 if p m then (takewhile p (m::gs) ms) 
                        else (gs, rs) in
    let (sysunexpect,msgs1) = takewhile (msg_eq (SysUnExpect "")) [] ms in
    let (unexpect,msgs2) = takewhile (msg_eq (UnExpect "")) [] msgs1 in
    let (expect, messages) = takewhile (msg_eq (Expect "")) [] msgs2 in
    let rec clean = 
      function [] -> []
        | (s::ss) -> if s = "" then clean ss 
                                  else s::(clean (List.filter ((<>) s) ss)) in
    let showMany pre msgs =
      let rec commasOr ms =
        match ms with
            [] -> ""
          | [m] -> m
          | (m::ms) -> m ^ ", or " ^ (commasOr ms) in
      match (pre,(clean (List.map get_string msgs))) with
          (_,[]) -> ""
        | ("",ms) -> commasOr ms
        | (pre,ms) -> pre ^ " " ^ (commasOr ms) in

    let showExpect = showMany "expecting" expect in
    let showUnExpect = showMany "unexpected" unexpect in
    let showSysUnExpect =
      if not (unexpect = []) || sysunexpect = [] then ""
      else 
        match get_string (List.hd sysunexpect) with
            "" -> "unexpected end of input"
          | m -> "unexpected " ^ m in
    let showMessage = showMany "" messages in
      (Pos.M.toString (errorPos err)) ^ ":\n" ^ 
      (String.concat "\n" 
         (clean [showSysUnExpect;showUnExpect;showExpect;showMessage])) ^
      "\n"
end
