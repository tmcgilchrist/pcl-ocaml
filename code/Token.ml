module type S =
sig 

  type t

  val toString : t -> string

  val eq       : t -> t -> bool

  val nextPos  : Pos.M.pos -> t -> Pos.M.pos

end



module CharTok : S with type t = char =
struct

  type t = char

  let toString c = (String.make 1 c)

  let eq = (=)

  let nextPos = Pos.M.updatePosChar

end
