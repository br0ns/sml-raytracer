all:
	cat Color.sig Color.sml \
      Image.sig Image.sml \
      Math3D.sig Math3D.sml \
      Primitive.sig \
      Sphere.sml Plane.sml Disc.sml Rectangle.sml \
      Render.sig RenderFn.sml \
      RenderBMP.sml RenderSDL.sml \
      loadmessage.sml \
      > Magic.sml
	mosmlc -c -P full \
     ../lib/MyLib.ui ../lib/MyLib.uo \
     -toplevel Magic.sml
	rm Magic.sml

clean:
	rm Magic.ui Magic.uo
