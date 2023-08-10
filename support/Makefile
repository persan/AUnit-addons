enable:
	@for i in ${CURDIR}/*.py; do \
		(cd ${HOME}/.gnatstudio/plug-ins/;ln -sf $$i);\
	done

disable:
	@for i in *.py; do \
		(cd ${HOME}/.gnatstudio/plug-ins/;rm  $$i);\
	done
	
