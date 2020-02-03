
all:
	gprbuild -p -j0
	
install:
	cp src/reporters/*.ad? ${DESTDIR}/reporters/
	cp src/framework/*.ad? ${DESTDIR}/framework/

