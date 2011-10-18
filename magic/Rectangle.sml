structure Rectangle : Primitive =
struct
type t = {origin : Math3D.vector, normal : Math3D.vector}
fun distance ({origin, direction}, {origin = p, normal}) =
    SOME 1.0

fun normal (_, {normal, ...} : t) =
    Math3D.null
end
