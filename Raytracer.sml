val epsilon = 0.000001

open Math3D
infix 6 ++ --
infix 7 ** // dot cross

type light = scalar * vector
type color = vector

(* farve, genskær, hårdhed, reflektion *)
type material = color * scalar * scalar * scalar

datatype shape =
         Sphere of {center : vector, radius : scalar}
       | Plane of {origin : vector, normal : vector}

datatype object =
         Group of object list
       | Primitive of shape * material

val black = (0.0, 0.0, 0.0)
val white = (1.0, 1.0, 1.0)

val green = (0.0, 1.0, 0.0)


fun distance (ray, Sphere s) = Sphere.distance (ray, s)
  | distance (ray, Plane p) = Plane.distance (ray, p)

fun normal (point, Sphere s) = Sphere.normal (point, s)
  | normal (point, Plane p) = Plane.normal (point, p)

fun shoot (ray, object) =
    let
      fun best (x as SOME _, NONE) = x
        | best (NONE, x as SOME _) = x
        | best (x as SOME (xd, _, _), y as SOME (yd, _, _)) =
                                      if xd < yd then
                                        x
                                      else
                                        y
        | best (x, _) = x
      fun loop (Group objs, hit) =
          foldl loop hit objs
        | loop (Primitive (s, m), hit) =
          case distance (ray, s) of
            SOME d =>
            best (SOME (d, s, m), hit)
          | NONE => hit
    in
      loop (object, NONE)
    end

fun raytracer {ray, scene, lights, ambience} =
    let
      fun loop _ 0 = black
        | loop (ray as {origin, direction}) n =
          case shoot (ray, scene) of
            (* NONE => white *)
            NONE => black
          | SOME (dist, shape, (color, spec, hardness, refl)) =>
            let
              val origin = origin ++ dist ** direction
              val normal = normal (origin, shape)
              val direction =
                  direction -- 2.0 * (direction dot normal) ** normal
              val origin = origin ++ epsilon ** normal
              val ray = {origin = origin, direction = direction}

              fun direct (i, d) =
                  case shoot ({origin = origin, direction = neg d}, scene) of
                    NONE =>
                    let
                      val diffuse = Real.max (~(d dot normal), 0.0) ** color
                      val specular =
                          Math.pow (Real.max (~(d dot direction), 0.0),
                                    hardness) * spec ** white
                    in
                      i ** (diffuse ++ black)
                    end
                  | SOME _ => black

              val refl =
                  let
                    val (r, g, b) = refl ** loop ray (n - 1)
                    val (r', g', b') = (1.0 - refl) ** color ++ refl ** white
                  in
                    (r * r', g * g', b * b')
                  end

              val amb = ambience ** color
              val direct = foldl op++ black (map direct lights)
            in
              amb ++ direct ++ refl
            end
    in
      Color.fromRGBr $ loop ray 6
    end

fun hue h =
    let
      val h' = 3.0 * h
    in
      (if h' < 1.0 then
         (1.0 - h', h', 0.0)
       else if h' < 2.0 then
         (0.0, 2.0 - h', h' - 1.0)
       else
         (h' - 2.0, 0.0, 3.0 - h')
      )
    end

fun sphere r c h =
    Primitive
    (Sphere {center = c,
             radius = r},
     (hue h, 0.3, 50.0, 0.5)
    )

fun create i h =
    let
      fun loop 0 r c _ = sphere r c h
        | loop i' r c (v, u) =
          Group
          (let
             val f = 2
             val p = 0.5

             val t = 2.0 * Math.pi / real f
             val r' = r / 1.5

             fun loop' t =
                 let
                   val u' = rotate v t u
                   val v' = rotate u' p v
                   val c = (r + r') ** v' ++ c
                 in
                   loop (i' - 1) r' c (v', u')
                 end
           in
             sphere r c (real i' / real i + h) ::
             List.tabulate (f, fn x => loop' (real x * t))
           end
          )
    in
      loop i
    end

fun mkScene n =
    create n 0.0 3.0 (~5.0, ~2.8, 20.0)
           (normalize (1.0, 1.0, ~1.0),
            normalize (1.0, 1.0, 1.0)
           )

val lights = [(1.0, normalize (0.0, ~0.5, 1.0))]

val res = 600

val disp = MosGame.Display.create_display (res, res)
val _ =
    RenderSDL.render
      disp
      {raytracer = raytracer,
       scene = mkScene 3,
       lights = lights,
       ambience = 0.0,
       resolution = res,
       subsampling = 1}
val _ = MosGame.Display.flip disp

(* val disp = Image.create (res, res) Color.Black *)
(* val disp = *)
(*     RenderBMP.render *)
(*       disp *)
(*       {raytracer = raytracer, *)
(*        scene = mkScene 3, *)
(*        lights = lights, *)
(*            ambience = 0.0, *)
(*        resolution = res, *)
(*        subsampling = 1} *)
(* val _ = Image.view "eog" disp *)
