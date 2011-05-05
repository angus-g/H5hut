# src level Makefile.am

OBJEXT=o

LIBS = @LIBS@
INCLUDES = -I../include @INCLUDES@

EXTRA_HEADERS =					\
	../include/h5core/h5_attach.h		\
	../include/h5core/h5_attribs.h		\
	../include/h5core/h5_core.h		\
	../include/h5core/h5_errno.h		\
	../include/h5core/h5_errorhandling.h	\
	../include/h5core/h5_hdf5.h		\
	../include/h5core/h5_maps.h		\
	../include/h5core/h5_openclose.h	\
	../include/h5core/h5_readwrite.h	\
	../include/h5core/h5_syscall.h		\
	../include/h5core/h5_types.h		\
	../include/h5core/h5u_readwrite.h	\
	../include/h5core/h5b_readwrite.h	\
	../include/h5core/h5u_model.h		\
	../include/h5core/h5b_model.h		\
	../include/h5core/h5b_attribs.h		\
	../include/h5core/h5t_adjacencies.h	\
	../include/h5core/h5t_core.h		\
	../include/h5core/h5t_inquiry.h		\
	../include/h5core/h5t_map.h		\
	../include/h5core/h5t_openclose.h	\
	../include/h5core/h5t_readwrite.h	\
	../include/h5core/h5t_ref_elements.h	\
	../include/h5core/h5t_retrieve.h	\
	../include/h5core/h5t_storemesh.h	\
	../include/h5core/h5t_tags.h		\
	h5_attribs_private.h			\
	h5_core_private.h			\
	h5_errorhandling_private.h		\
	h5_fcmp_private.h			\
	h5_hdf5_private.h			\
	h5_hsearch_private.h			\
	h5_lustre_private.h			\
	h5_maps_private.h			\
	h5_mpi_private.h			\
	h5_qsort_private.h			\
	h5_readwrite_private.h			\
	h5_syscall_private.h			\
	h5_types_private.h			\
	h5b_errorhandling_private.h		\
	h5b_model_private.h			\
	h5b_types_private.h			\
	h5t_access_private.h			\
	h5t_adjacencies_private.h		\
	h5t_consts_private.h			\
	h5t_core_private.h			\
	h5t_errorhandling_private.h		\
	h5t_hsearch_private.h			\
	h5t_map_private.h			\
	h5t_openclose_private.h			\
	h5t_readwrite_private.h			\
	h5t_ref_elements_private.h		\
	h5t_retrieve_private.h			\
	h5t_store_private.h			\
	h5t_store_trim_private.h		\
	h5t_tags_private.h			\
	h5t_types_private.h			\
	h5u_errorhandling_private.h		\
	h5u_types_private.h

# Extra files that I wish to include in the dist tar ball.
EXTRA_DIST = $(EXTRA_HEADERS)

# What to build... Will be determined by configure script.
lib_LIBRARIES = ../lib/libH5hut.a

# Listing of all possible targets that I may build.
EXTRA_LIBRARIES = ../lib/libH5hut.a

# Listing of sources
___lib_libH5hut_a_SOURCES =			\
	h5_attach.c				\
	h5_attribs.c				\
	h5_errorhandling.c			\
	h5_fcmp.c				\
	h5_hdf5.c				\
	h5_hsearch.c				\
	h5_maps.c				\
	h5_mpi.c				\
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
        h5b_attribs.c                           \
	h5t_access_tetm.c			\
	h5t_access_trim.c			\
	h5t_adjacencies.c			\
	h5t_adjacencies_tetm.c			\
	h5t_adjacencies_trim.c			\
	h5t_consts.c				\
	h5t_errorhandling.c			\
	h5t_hsearch.c				\
	h5t_inquiry.c				\
	h5t_map.c				\
	h5t_openclose.c				\
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

___lib_libH5hut_a_DEPENDENCIES = $(EXTRA_HEADERS)

all: all-am

install-exec-local:
	@$(INSTALL) -d $(DESTDIR)$(includedir)/h5core
	@$(INSTALL) -m644 ../include/h5core/*.h $(DESTDIR)$(includedir)/h5core/

clean: clean-am

