
AM_CPPFLAGS = -I$(top_srcdir)/src/include @AM_CPPFLAGS@

EXTRA_HEADERS =					\
	h5_attribs_private.h			\
	h5_debug_private.h			\
	h5_errorhandling_private.h		\
	h5_fcmp_private.h			\
	h5_hdf5_private.h			\
	h5_hsearch_private.h			\
	h5_lustre_private.h			\
	h5_maps_private.h			\
	h5_model_private.h			\
	h5_mpi_private.h			\
	h5_private.h				\
	h5_qsort_private.h			\
	h5_readwrite_private.h			\
	h5_syscall_private.h			\
	h5_types_private.h			\
	h5_va_macros.h				\
	h5b_model_private.h			\
	h5b_readwrite_private.h			\
	h5b_types_private.h			\
	h5t_access_private.h			\
	h5t_adjacencies_private.h		\
	h5t_core_private.h			\
	h5t_errorhandling_private.h		\
	h5t_map_private.h			\
	h5t_model_private.h			\
	h5t_octree_private.h			\
	h5t_readwrite_private.h			\
	h5t_ref_elements_private.h		\
	h5t_retrieve_private.h			\
	h5t_store_private.h			\
	h5t_tags_private.h			\
	h5t_types_private.h			\
	h5u_readwrite_private.h			\
	h5u_types_private.h

# Extra files that I wish to include in the dist tar ball.
EXTRA_DIST = $(EXTRA_HEADERS)

# What to build... Will be determined by configure script.
lib_LTLIBRARIES = libH5hut.la

# Listing of sources
libH5hut_la_SOURCES =				\
	h5_attach.c				\
	h5_attribs.c				\
	h5_errorhandling.c			\
	h5_fcmp.c				\
	h5_init.c				\
	h5_hdf5.c				\
	h5_hsearch.c				\
	h5_maps.c				\
	h5_model.c				\
	h5_openclose.c				\
	h5_qsort.c				\
	h5_qsort_r.c				\
	h5_readwrite.c				\
	h5_syscall.c				\
	h5_lustre.c				\
	h5u_readwrite.c                         \
	h5b_readwrite.c                         \
	h5u_model.c				\
        h5b_model.c				\
        h5b_attribs.c

if ENABLE_EXPERIMENTAL
libH5hut_la_SOURCES +=				\
	h5t_access_tetm.c			\
	h5t_access_trim.c			\
	h5t_adjacencies.c			\
	h5t_adjacencies_tetm.c			\
	h5t_adjacencies_trim.c			\
	h5t_core.c				\
	h5t_core_trim.c				\
	h5t_core_tetm.c				\
	h5t_map.c				\
	h5t_model.c				\
	h5t_model_tetm.c			\
	h5t_model_trim.c			\
	h5t_octree.c				\
	h5t_readwrite.c				\
	h5t_readwrite_trim.c			\
	h5t_readwrite_tetm.c			\
	h5t_ref_elements.c			\
	h5t_retrieve.c				\
	h5t_retrieve_tetm.c			\
	h5t_retrieve_trim.c			\
	h5t_store.c				\
	h5t_store_tetm.c			\
	h5t_store_trim.c			\
	h5t_tags.c
endif

libH5hut_la_DEPENDENCIES = $(EXTRA_HEADERS)

libH5hut_la_LDFLAGS = -version-info 2:0:0

#all-local:
#	$(INSTALL) -m0755 -d $(top_builddir)/src/lib
#	$(INSTALL) -m644 .libs/libH5hut.a $(top_builddir)/src/lib

install-exec-local:
	@$(INSTALL) -d $(DESTDIR)$(includedir)/h5core
	@$(INSTALL) -m644 $(top_srcdir)/src/include/h5core/*.h $(DESTDIR)$(includedir)/h5core/

clean-local:
	$(RM) -f $(top_srcdir)/src/lib/libH5hut.*
	$(RM) -f *~
