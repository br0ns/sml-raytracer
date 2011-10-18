all:
	mosmlc -c -P full \
     MyLib.ui MyLib.uo \
     MosGame.ui MosGame.uo \
     Magic.ui Magic.uo \
     -toplevel Raytracer.sml
