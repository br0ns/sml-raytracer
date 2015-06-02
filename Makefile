all: magic
	mosmlc -c -P full \
     lib/MyLib.ui lib/MyLib.uo \
     magic/Magic.ui magic/Magic.uo \
     -toplevel Raytracer.sml

.PHONY: magic clean

magic:
	(cd magic ; make)

clean:
	rm Raytracer.ui Raytracer.uo
	(cd magic ; make clean)
