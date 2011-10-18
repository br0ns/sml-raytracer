structure Sphere : Primitive =
struct
open Math3D infix 5 --> infix 6 ++ -- infix 7 ** // dot cross
type t = {center : vector, radius : scalar}
fun distance ({origin, direction}, {center, radius}) =
    let
      val v = origin --> center
      val b = v dot direction
      val r = b * b - v dot v + radius * radius
    in
      (* miss *)
      if r < 0.0 then
        NONE
      else
        let
          val r = Math.sqrt r
          val t2 = b + r
        in
          (* sphere is behind ray *)
          if t2 < 0.0 then
            NONE
          else
            SOME $ (fn t1 => if t1 > 0.0 then t1 else t2) (b - r)
        end
    end

fun normal (point, {center, radius}) =
    (center --> point) // radius
end
