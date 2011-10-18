structure Math3D :> Math3D =
struct
type scalar = real
type angle = real
type vector = scalar * scalar * scalar
type vector2d = scalar * scalar
(* Basises must be orthogonal *)
type basis = vector * vector * vector

exception Null

val null = (0.0, 0.0, 0.0)
fun isNull (x, y, z) =
    (* Some implementations do not implement Real.= *)
    Real.compare (x, 0.0) = EQUAL andalso
    Real.compare (y, 0.0) = EQUAL andalso
    Real.compare (z, 0.0) = EQUAL

infix 6 ++ --
infix 7 ** // dot cross

fun s ** (x, y, z) =
    (s * x, s * y, s * z)

fun (x, y, z) // s =
    (x / s, y / s, z / s)

fun (x1, y1, z1) ++ (x2, y2, z2) : vector =
    (x1 + x2, y1 + y2, z1 + z2)

fun (x1, y1, z1) -- (x2, y2, z2) : vector =
    (x1 - x2, y1 - y2, z1 - z2)

fun neg (x, y, z) : vector =
    (~x, ~y, ~z)

fun (x1, y1, z1) dot (x2, y2, z2) : scalar =
    x1 * x2 + y1 * y2 + z1 * z2

fun (x1, y1, z1) cross (x2, y2, z2) : vector =
    (y1 * z2 - z1 * y2,
     z1 * x2 - x1 * z2,
     x1 * y2 - y1 * x2)

val sum = foldl op++ null

fun length v = Math.sqrt (v dot v)

fun normalize v = v // length v

fun toSpherical (v as (x, y, z)) =
    let
      val r = length v
      val t = Math.asin (y / r)
          handle Domain =>
                 if y > 0.0 then Math.pi / 2.0 else ~Math.pi / 2.0
      val p =
          let
            val p' = Math.pi / 2.0 - Math.atan (x / z)
          in
            if z < 0.0 then p' + Math.pi else p'
          end handle Domain => 0.0
    in
      (r, t, p)
    end

fun fromSpherical (r, t, p) =
    let
      val ct = Math.cos t
      val x = r * ct * Math.cos p
      val z = r * ct * Math.sin p
      val y = r * Math.sin t
    in
      (x, y, z)
    end

fun toBasis (x, y, z) v =
    (v dot x, v dot y, v dot z)

fun fromBasis ((x1, y1, z1), (x2, y2, z2), (x3, y3, z3)) v =
    (v dot (x1, x2, x3),
     v dot (y1, y2, y3),
     v dot (z1, z2, z3))

fun project (u, v) w =
    (w dot u, w dot v)

local
  val x = (1.0, 0.0, 0.0)
  val y = (0.0, 1.0, 0.0)
  val z = (0.0, 0.0, 1.0)
in
val projectXY = project (x, y)
val projectXZ = project (x, z)
val projectYZ = project (y, z)
end

val cos = Math.cos
val sin = Math.sin

fun rotate (x, y, z) theta (alpha, beta, gamma) =
    let
      val c = cos theta
      val s = sin theta
      val t = 1.0 - c
    in
      (alpha * (t * x * x + c) +
       beta  * (t * x * y - s * z) +
       gamma * (t * x * z + s * y),
       alpha * (t * x * y + s * z) +
       beta  * (t * y * y + c) +
       gamma * (t * y * z - s * x),
       alpha * (t * x * z - s * y) +
       beta  * (t * y * z + s * x) +
       gamma * (t * z * z + c)
      )
    end

local
  fun rot t (x, y) =
      let
        val c = cos t
        val s = sin t
      in
        (c * x - s * y, s * x + c * y)
      end
in
fun rotateX t (x, y, z) =
    let
      val (y', z') = rot t (y, z)
    in
      (x, y', z')
    end

fun rotateY t (x, y, z) =
    let
      val (z', x') = rot t (z, x)
    in
      (x', y, z')
    end

fun rotateZ t (x, y, z) =
    let
      val (x', y') = rot t (x, y)
    in
      (x', y', z)
    end
end

fun orto (x, y, z) =
    case (Real.compare (x, 0.0) = EQUAL,
          Real.compare (y, 0.0) = EQUAL,
          Real.compare (z, 0.0) = EQUAL) of
      (false, true, true) => (0.0, 1.0, 0.0)
    | (true, false, true) => (0.0, 0.0, 1.0)
    | (true, true, false) => (1.0, 0.0, 0.0)
    | (false, _, _)       => normalize (0.0, ~x * z, x * y)
    | (_, false, _)       => normalize (~y * z, 0.0, x * y)
    | _                   => raise Null

fun mirror n v =
    let
      val u = (v dot n) ** n
    in
      v -- 2.0 ** u
    end

val rgen = Random.rand (0, 0)
fun rand () = Random.randReal rgen

fun monteCarlo n (j, k) =
    let
      val rn = real n
      val t = Math.acos (Math.sqrt ((real j + rand ()) / rn))
      val p = Math.pi * ((real k + rand ()) / rn)
    in
      (t, p)
    end

fun fuzz a v =
    let
      val v' = orto v
      val (t, p) = monteCarlo 1 (0, 0)
    in
      rotate v (2.0 * p) (rotate v' t v)
    end

fun distribute 0 _ = nil
  | distribute n v =
    let
      val v' = orto v
      val js = 0 to n - 1
      val ks = 0 to 2 * n - 1
      val vs = map (monteCarlo n) (List.allPairs js ks)
    in
      map (fn (t, p) =>
              rotate v p (rotate v' t v)
          ) vs
    end

fun --> (v, u) = u -- v

end
