export PATH:=${CURDIR}/bin:${PATH}
all:
	gprbuild
	suits-main
scriptdir=$(dir ${CURDIR})support/

enable:
	@for i in ${scriptdir}*.py; do \
		(cd ${HOME}/.gnatstudio/plug-ins; ln -sf $$i);\
	done

disable:
	@for i in ${scriptdir}*.py; do \
		(cd ${HOME}/.gnatstudio/plug-ins; rm `basename $$i`);\
	done
