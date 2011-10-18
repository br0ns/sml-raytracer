signature Image =
sig
  type t
  val create : int * int -> Color.t -> t

  val setPixel : t -> Color.t -> int * int -> unit
  val getPixel : t -> int * int -> Color.t

  val width : t -> int
  val height : t -> int

  val resize : t -> int * int -> unit
  val scale : t -> real -> unit

  val toBMP : string -> t -> unit
  val fromBMP : string -> t

  val view : string -> t -> unit
end
