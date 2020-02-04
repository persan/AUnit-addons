
all:
	gprbuild -p -j0 -P aunit-addons.gpr

install:
# Target install requires DESTDIR
	@if [ -z "${DESTDIR}" ]; then echo "DESDIR not set";exit 1; fi
	
# Print all files thats going to be installed
	@cd src;     find -type f -name "*.ad?"
	@cd support; find -type f -name "*.py"

# In case of a "test" install prepare the target structure.
	@mkdir -p ${DESTDIR}/include/aunit  ${DESTDIR}/support ${DESTDIR}/lib/gnat/
	@touch ${DESTDIR}/lib/gnat/aunit.gpr

# Do the actual install
	@(cd src;     tar -c `find -name "*.ad?"`) | (cd ${DESTDIR}/include/aunit; tar -x)
	@(cd support; tar -c `find -name "*.py"`)  | (cd ${DESTDIR}/support; tar -x)
	sed "s-../../support/aunit.xml-../../support/aunit*-" ${DESTDIR}/lib/gnat/aunit.gpr -i

clean:
	git clean -xdf

