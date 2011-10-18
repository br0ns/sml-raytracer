signature Primitive =
sig
  type t
  val distance : {origin : Math3D.vector, direction : Math3D.vector} * t -> Math3D.scalar option
  val normal : Math3D.vector * t -> Math3D.vector
end
