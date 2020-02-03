
all:
	gprbuild -p -j0 -P aunit-addons.gpr

install:
	@if [ -z "${DESTDIR}" ]; then echo "DESDIR not set";exit 1; fi
	@cd src;     find -type f -name "*.ad?"
	@cd support; find -type f -name "*.py"
	@mkdir -p ${DESTDIR}/include/aunit
	@mkdir -p ${DESTDIR}/support
	@(cd src;     tar -c `find -name "*.ad?"`) | (cd ${DESTDIR}/include/aunit; tar -x)
	@(cd support; tar -c `find -name "*.py"`)  | (cd ${DESTDIR}/support; tar -x)
	sed "s-../../support/aunit.xml-../../support/aunit*-" ${DESTDIR}/lib/gnat/aunit.gpr -i

clean:
	git clean -xdf
