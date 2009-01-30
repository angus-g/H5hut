DOXYGROUPS = \
	h5part__attrib \
	h5part__errhandle \
	h5part__openclose \
	h5part__read \
	h5part__write \
	h5block__c__api 

EXTRA_DIST = \
	Doxyfile doxyfooter

all:

doc:	doxydoc

doxydoc:
	doxygen

clean:
	${RM} -rf ReferencePages TWiki
