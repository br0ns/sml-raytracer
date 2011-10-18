structure Color :> Color =
struct
type word = Word8.word
type t = word * word * word

val fromInt = Word8.fromInt
val toInt = Word8.toInt

fun fromRGBi (r, g, b) =
    let
      fun c x =
          fromInt (Int.max (0, Int.min (255, x)))
    in
      (c r, c g, c b)
    end

fun fromRGBr (r, g, b) =
    let
      fun c x = round (255.0 * x)
    in
      fromRGBi (c r, c g, c b)
    end

fun grayscalei g = fromRGBi (g, g, g)
fun grayscaler g = fromRGBr (g, g, g)

fun toRGBi (r, g, b) =
    (toInt r, toInt g, toInt b)

fun toRGBr color =
    let
      fun c x = real x / 255.0
    in
      case toRGBi color of
        (r, g, b) => (c r, c g, c b)
    end

fun avg (c1, c2) =
    case (toRGBi c1, toRGBi c2) of
      ((r1, g1, b1), (r2, g2, b2)) =>
      fromRGBi ((r1 + r2) div 2, (g1 + g2) div 2, (b1 + b2) div 2)

fun avgMany cs =
    let
      val n = length cs
      fun loop (r, g, b) nil = fromRGBi (r div n, g div n, b div n)
        | loop (r, g, b) (c :: cs) =
          loop (case toRGBi c of
                  (r', g', b') => (r + r', g + g', b + b')
               ) cs
    in
      loop (0, 0, 0) cs
    end

fun add (c1, c2) =
    case (toRGBi c1, toRGBi c2) of
      ((r1, g1, b1), (r2, g2, b2)) =>
      fromRGBi (r1 + r2, g1 + g2, b1 + b2)

fun addMany cs =
    fromRGBi (foldl
                (fn (c, (r, g, b)) =>
                    case toRGBi c of
                      (r', g', b') => (r + r', g + g', b + b')
                )
                (0, 0, 0)
                cs
             )

fun sub (c1, c2) =
    case (toRGBi c1, toRGBi c2) of
      ((r1, g1, b1), (r2, g2, b2)) =>
      fromRGBi (r1 - r2, g1 - g2, b1 - b2)

fun mul (c1, c2) =
    case (toRGBi c1, toRGBi c2) of
      ((r1, g1, b1), (r2, g2, b2)) =>
      fromRGBi ((r1 * r2) div 255,
                (g1 * g2) div 255,
                (b1 * b2) div 255)

nonfix div
fun div (c1, c2) =
    let open Int infix div in
      case (toRGBi c1, toRGBi c2) of
        ((r1, g1, b1), (r2, g2, b2)) =>
        fromRGBi (r1 div r2, g1 div g2, b1 div b2)
    end

local val c = fromRGBi in
val Black = c (0, 0, 0)
val White = c (255, 255, 255)
val Gray = c (128, 128, 128)
val LightGray = c (192, 192, 192)
val DarkGray = c (64, 64, 64)
val Red = c (255, 0, 0)
val Green = c (0, 255, 0)
val Blue = c (0, 0, 255)
val Yellow = c (255, 255, 0)
val Magenta = c (255, 0, 255)
val Cyan = c (0, 255, 255)
end

end
