structure Image :> Image =
struct
type 'a array = 'a Array.array
type color = Color.t
type point = int * int
type t = point * color array ref

fun create (w, h) c =
    if w > 0 andalso h > 0 then
      ((w, h),
       ref (Array.tabulate (w * h, fn _ => c))
      )
    else
      raise Domain

fun setPixel ((w, _), data) c (x, y) =
    Array.update (!data, y * w + x, c)

fun getPixel ((w, h), data) (x, y) =
    Array.sub (!data, y * w + x)

fun width ((w, _), _) = w
fun height ((_, h), _) = h

(* fun resize ((w, h), data) (w', h') = *)
(*     let *)
(*       fun pixel  *)

fun resize _ = raise Fail ""
fun scale _ = raise Fail ""

fun toBMP file (img as ((w, h), _)) =
    let
      val padding =
          implode
            (List.tabulate
               ((~3 * w) mod 4, fn _ => chr 0)
            )
      val size = (3 * w + size padding) * h + 54

      fun dword x =
          implode
            [chr (x mod 256),
             chr ((x mod 65536) div 256),
             chr ((x mod 16777216) div 65536),
             chr (x div 16777216)]

      fun word x =
          implode
            [chr (x mod 256),
             chr (x div 256)]

      val header =
          "BM" ^
          dword size ^
          word 0 ^
          word 0 ^
          dword 54 ^
          dword 40 ^
          dword w ^
          dword h ^
          word 1 ^
          word 24 ^
          dword 0 ^
          dword (w * h) ^
          dword 2835 ^
          dword 2835 ^
          dword 0 ^
          dword 0

      val os = TextIO.openOut file

      fun loop ~1 = ()
        | loop y =
          let
            fun loop' x =
                if x < w then
                  case Color.toRGBi (getPixel img (x, y)) of
                    (r, g, b) =>
                    (TextIO.output1 (os, chr b) ;
                     TextIO.output1 (os, chr g) ;
                     TextIO.output1 (os, chr r) ;
                     loop' (x + 1)
                    )
                else
                  TextIO.output (os, padding)
          in
            loop' 0 ; loop (y - 1)
          end
    in
      TextIO.output (os, header) ;
      loop (h - 1) ;
      TextIO.closeOut os
    end

fun fromBMP file =
    let
      val is = TextIO.openIn file
      val data = TextIO.inputAll is

      fun at n = ord (String.sub (data, n))

      fun dword n =
          (at n +
           256 * (at (n + 1) +
                  256 * (at (n + 2) +
                         256 * at (n + 3)
                        )
                 )
          )

      fun word n =
          (at n + 256 * at (n + 1))

      val magic = word 0
      val offset = dword 10
      val w = dword 18
      val h = dword 22

      val padding = (~3 * w) mod 4

      fun pixel (x, y) =
          let
            val i = 3 * x + (h - y - 1) * (3 * w + padding) + offset
          in
            Color.fromRGBi (at (i + 2), at (i + 1), at i)
          end
      val img = create (w, h) Color.Black
    in
      app (fn p => setPixel img (pixel p) p)
          (List.allPairs (0 to (w - 1)) (0 to (h - 1))) ;
      img
    end

fun view viewer img =
    let
      val file = OS.FileSys.tmpName () ^ ".bmp"
    in
      toBMP file img ;
      OS.Process.system (viewer ^ " " ^ file) ;
      OS.FileSys.remove file handle _ => ()
    end
end
