signature Color =
sig
  type t
  val grayscalei : int -> t
  val grayscaler : real -> t
  val fromRGBi : int * int * int -> t
  val fromRGBr : real * real * real -> t
  val toRGBi : t -> int * int * int
  val toRGBr : t -> real * real * real

  val avg : t * t -> t
  val avgMany : t list -> t
  val add : t * t -> t
  val addMany : t list -> t
  val sub : t * t -> t
  val mul : t * t -> t
  val div : t * t -> t

  val Black : t
  val White : t
  val Gray : t
  val LightGray : t
  val DarkGray : t
  val Red : t
  val Green : t
  val Blue : t
  val Yellow : t
  val Magenta : t
  val Cyan : t
end
