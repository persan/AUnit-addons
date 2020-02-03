
all:
	gprbuild -p -j0 -P aunit-addons.gpr

install:
	@if [ -z "${DESTDIR}" ]; then echo "DESDIR not set";exit 1; fi
	cd src; find -name "*.ad?"
	@(cd src; tar -c `find -name "*.ad?"`) | (cd ${DESTDIR}; tar -x)

clean:
	git clean -xdf
