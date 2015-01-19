
(* Code and ideas taken from:
 *
 * http://enfranchisedmind.com/blog/archive/2007/01/01/177
 * and
 * http://nopaste.tshw.de/1182645499e7603/
 * 
 * via #ocaml
 *)

module type S = 
sig

  type 'a l_node =
      Nil
    | Cons of 'a * 'a l_list
  and 'a l_list = 'a l_node lazy_t


  val from      : (int -> 'a option) -> 'a l_list
    
  val ofList    : 'a list -> 'a l_list
    
  val ofString  : string -> char l_list
    
  val ofStream  : 'a Stream.t -> 'a l_list
    
  val ofChannel : in_channel -> char l_list
    
  val expose    : 'a l_list -> ('a * 'a l_list) option

end



module M : S =
struct

  type 'a l_node =
      Nil
      | Cons of 'a * 'a l_list
  and 'a l_list = 'a l_node lazy_t


  let from f =
    let rec get_next k =
      match f k with
          Some a -> Cons (a, lazy (get_next (k+1)))
        | None -> Nil 
    in
      lazy (get_next 0)

  let ofList l =
    let rec get_next = function
        (x::xs) -> Cons (x , lazy (get_next xs))
      | []      -> Nil
    in
      lazy (get_next l)

  let ofString s =
    let rec get_next k =
      try (
        Cons (String.get s k, lazy (get_next (k+1)))
      ) with (Invalid_argument _) -> Nil
    in
      lazy (get_next 0)

  let ofStream s =
    let rec get_next () =
      try (
        Cons (Stream.next s, lazy (get_next ()))
      ) with Stream.Failure -> Nil
    in
      lazy (get_next ())
        
  let ofChannel c = 
    ofStream (Stream.of_channel c)

  let expose l =
    match Lazy.force l with
        Nil -> None
      | Cons (a,l) -> Some (a,l)
end
