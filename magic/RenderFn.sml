functor RenderFn (
type display
val setPixel : display -> Color.t -> int * int -> unit
) : Render =
struct
type display = display
fun render disp {raytracer, scene, lights, ambience, resolution, subsampling} =
    let
      val eye = (0.0, 0.0, ~6.0)
      val n = real resolution
      val d = real subsampling
      val progress = ref 0
      val onepercent = (resolution * resolution) div 100
      fun pixel (x, y) =
          let
            val x' = real x - n / 2.0
            val y' = (n - 1.0) / 2.0 - real y
            val r = 0 to (subsampling - 1)
            fun trace (dx, dy) =
                raytracer
                  {scene = scene,
                   lights = lights,
                   ambience = ambience,
                   ray = {origin = eye,
                          direction =
                          Math3D.normalize (x' + real dx / d,
                                            y' + real dy / d,
                                            n)
                         }
                  }
          in
            setPixel
              disp
              (Color.avgMany (map trace (List.allPairs r r)))
              (x, y)
          ; let
              val p = !progress div onepercent
              val _ = inc progress
              val p' = !progress div onepercent
            in
              if p' > p then
                let
                  val s = Show.int p ^ "%]"
                  val s' = Show.int p' ^ "%]"
                in
                  print (implode (List.tabulate (size s, fn _ => chr 8))) ;
                  print s'
                end
              else
                ()
            end
          end

      val r = 0 to (resolution - 1)
    in
      print "[Rendering 0%]" ;
      Benchmark.start () ;
      app (fn x => app (fn y => pixel (x, y)) r) r ;
      Benchmark.stop () ;
      let
        val {tot, ...} = Benchmark.time ()
      in
        println ("\n[Done: Took " ^
                 Time.toString tot ^
                 "s]")
      end ;
      disp
    end
end
