signature Render =
sig
  type display
  val render : display ->
               {raytracer   :
                {ray      : {origin : Math3D.vector, direction : Math3D.vector},
                 scene    : 'object,
                 lights   : 'light list,
                 ambience : real} -> Color.t,
                scene       : 'object,
                lights      : 'light list,
                ambience    : real,
                resolution  : int,
                subsampling : int} -> display
end
