
all:
	gprbuild -p -j0
	
install:
	cp src/*.ad? ${DESTDIR}

