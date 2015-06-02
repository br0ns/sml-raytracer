(*
structure RenderSDL =
RenderFn (
open MosGame
type display = Display.surface
fun setPixel disp c p = (
    Draw.draw_pixel disp p (RGB $ Color.toRGBi c)
  ; MosGame.Event.clear MosGame.Event.AllEvents
)
)
*)
