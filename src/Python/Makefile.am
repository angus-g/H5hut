
if ENABLE_PYTHON

BUILT_SOURCES = H5hut_wrap.c
SWIG_SOURCES = H5hut.i numpy.i

pkgpython_PYTHON = H5hut.py __init__.py
pkgpyexec_LTLIBRARIES = _H5hut.la

_H5hut_la_SOURCES = H5hut_wrap.c $(SWIG_SOURCES)
_H5hut_la_LDFLAGS = -module
_H5hut_la_LIBADD = $(top_builddir)/src/h5core/libH5hut.la 

if ENABLE_PARALLEL

#SWIG_SOURCES += $(MPI4PY_INCLUDE)/mpi4py/mpi4py.i

_H5hut_la_CPPFLAGS = -DPARALLEL_IO $(SWIG_PYTHON_CPPFLAGS) -I$(top_srcdir)/src/include -I$(PYTHON_PREFIX)/include/python$(PYTHON_VERSION)/ -I$(MPI4PY_INCLUDE) -I$(NUMPY_INCLUDE)

H5hut_wrap.c : $(SWIG_SOURCES)
	$(SWIG) $(SWIG_PYTHON_OPT) -python -DPARALLEL_IO -I$(top_srcdir)/src/include -I$(MPI4PY_INCLUDE) -o $@ $<

else


_H5hut_la_CPPFLAGS = $(SWIG_PYTHON_CPPFLAGS) -I$(top_srcdir)/src/include -I$(PYTHON_PREFIX)/include/python$(PYTHON_VERSION)/ -I$(NUMPY_INCLUDE)

H5hut_wrap.c : $(SWIG_SOURCES)
	$(SWIG) $(SWIG_PYTHON_OPT) -python -I$(top_srcdir)/src/include -o $@ $<


endif

#all-local: 
#	$(INSTALL) -d  $(DESTDIR)/site-packages/h5hut
#	$(INSTALL) -m644 H5hut.py $(DESTDIR)/site-packages/h5hut/
#	$(INSTALL) -m644 .libs/_H5hut.so $(DESTDIR)/site-packages/h5hut/
endif

clean: clean-am

clean-local:
	$(RM) -f *~ H5hut_wrap.c H5hut.py H5hut.pyc H5hut_mpi.pyc
