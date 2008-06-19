DOXYGROUPS = \
	h5part__attrib \
	h5part__errhandle \
	h5part__openclose \
	h5part__read \
	h5part__write \
	h5block__c__api 

EXTRA_DIST = \
	Doxyfile doxyfooter

all: doxydoc twiki

doxydoc:
	doxygen

twiki: doxydoc
	mkdir -p TWiki
	for g in ${DOXYGROUPS}; do for f in ReferencePages/group__$${g}_*.html; do ./html2twiki $$f  TWiki; done; done

clean:
	${RM} -rf ReferencePages TWiki
