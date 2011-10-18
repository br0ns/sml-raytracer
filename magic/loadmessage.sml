val _ =
    let open Layout infix \ in
      println (SOME 80)
              (txt "Magic loaded:" \
                   indent 2 $ itemize "o"
                   [txt "Color",
                    txt "Image",
                    txt "Math3D",
                    txt "RenderFn",
                    txt "RenderBMP",
                    txt "Primitives:" \
                        itemize "-"
                        [txt "Sphere",
                         txt "Plane",
                         txt "Disc",
                         txt "Rectangle"
                        ]
                   ]
              )
    end
