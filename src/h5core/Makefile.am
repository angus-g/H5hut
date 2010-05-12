# src level Makefile.am

# COMPILERS
CC = @CC@

AM_CPPFLAGS = -I@HDF5ROOT@/include @MPIINC@ -I. -I../include -I../include/h5core
AM_CFLAGS = @CFLAGS@ 

EXTRA_HEADERS =					\
	../include/h5core/h5_attribs.h		\
	../include/h5core/h5_core.h		\
	../include/h5core/h5_errorhandling.h	\
	../include/h5core/h5_maps.h		\
	../include/h5core/h5_openclose.h	\
	../include/h5core/h5_readwrite.h	\
	../include/h5core/h5_types.h		\
	../include/h5core/h5t_adjacencies.h	\
	../include/h5core/h5t_inquiry.h		\
	../include/h5core/h5t_map.h		\
	../include/h5core/h5t_openclose.h	\
	../include/h5core/h5t_readwrite.h	\
	../include/h5core/h5t_ref_elements.h	\
	../include/h5core/h5t_retrieve.h	\
	../include/h5core/h5t_storemesh.h	\
	../include/h5core/h5t_tags.h		\
	../include/h5core/h5u_readwrite.h	\
	../include/h5core/hdf5_misc.h		\
	h5_errorhandling_private.h		\
	h5_fcmp_private.h			\
	h5_hdf5_private.h			\
	h5_hsearch_private.h			\
	h5_mpi_private.h			\
	h5_qsort_private.h			\
	h5_readwrite_private.h			\
	h5_syscall_private.h			\
	h5b_errorhandling_private.h		\
	h5t_consts_private.h			\
	h5t_errorhandling_private.h		\
	h5t_hsearch_private.h			\
	h5t_readwrite_private.h			\
	h5t_retrieve_private.h			\
	h5t_storemesh_private.h			\
	h5t_tags_private.h			\
	h5t_tetm_adjacencies_private.h		\
	h5t_tetm_retrieve_private.h		\
	h5t_tetm_store_private.h		\
	h5t_trim_adjacencies_private.h		\
	h5t_trim_retrieve_private.h		\
	h5t_trim_store_private.h		\
	h5u_errorhandling_private.h

# Extra files that I wish to include in the dist tar ball.
EXTRA_DIST = $(EXTRA_HEADERS)

# Files that I don't want to include in the dist tar ball
nodist_include_HEADERS =

OBJEXT = o
# What to build... Will be determined by configure script.
lib_LIBRARIES = libH5Core.a

# Listing of all possible targets that I may build.
EXTRA_LIBRARIES = libH5Core.a

# Header files that I wish to install in $(prefix)/include
include_HEADERS =

# Listing of sources
libH5Core_a_SOURCES =				\
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
	h5t_adjacencies.c			\
	h5t_consts.c				\
	h5t_errorhandling.c			\
	h5t_hsearch.c				\
	h5t_inquiry.c				\
	h5t_map.c				\
	h5t_openclose.c				\
	h5t_readwrite.c				\
	h5t_ref_elements.c			\
	h5t_retrieve.c				\
	h5t_storemesh.c				\
	h5t_tags.c				\
	h5t_tetm_adjacencies.c			\
	h5t_tetm_retrieve.c			\
	h5t_tetm_store.c			\
	h5t_trim_adjacencies.c			\
	h5t_trim_retrieve.c			\
	h5t_trim_store.c			\
	h5u_readwrite.c				\
	hdf5_misc.c

libH5Core_a_DEPENDENCIES = $(EXTRA_HEADERS)

all: ../lib/libH5Core.a

../lib/libH5Core.a: libH5Core.a
	-cp $^ $@

$(libH5Core_a_OBJECTS): $(libH5Core_a_DEPENDENCIES)

clean:
	$(RM) -f *~ *.o *.a *.so

distclean: clean
	$(RM) -f *.a
	$(RM) -rf .deps
	$(RM) -rf .libs
	$(RM) -f Makefile
