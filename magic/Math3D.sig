signature Math3D =
sig
  type scalar = real
  type angle = real
  type vector = scalar * scalar * scalar
  type vector2d = scalar * scalar
  type basis = vector * vector * vector

  exception Null

  val null : vector
  val isNull : vector -> bool

  val ** : scalar * vector -> vector
  val // : vector * scalar -> vector
  val ++ : vector * vector -> vector
  val -- : vector * vector -> vector
  val neg : vector -> vector

  val sum : vector list -> vector

  val dot : vector * vector -> scalar
  val cross : vector * vector -> vector

  val length : vector -> scalar

  val normalize : vector -> vector

  (* radius, polar, azimuthal *)
  val toSpherical : vector -> scalar * scalar * scalar
  val fromSpherical : scalar * scalar * scalar -> vector

  val toBasis : basis -> vector -> vector
  val fromBasis : basis -> vector -> vector

  (* Rotation vector must be a unit vector *)
  val rotate : vector -> angle -> vector -> vector
  val rotateX : angle -> vector -> vector
  val rotateY : angle -> vector -> vector
  val rotateZ : angle -> vector -> vector

  val project : vector * vector -> vector -> vector2d
  val projectXY : vector -> vector2d
  val projectXZ : vector -> vector2d
  val projectYZ : vector -> vector2d

  val orto : vector -> vector
  val mirror : vector -> vector -> vector
  (* 0..1 *)
  val fuzz : real -> vector -> vector
  val distribute : int -> vector -> vector list

  (* p --> v is quivalent to v -- p *)
  val --> : vector * vector -> vector
end
