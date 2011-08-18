# src/C level Makefile.am

OBJEXT=o

INCLUDES = -I../include @INCLUDES@

# What to build... Will be determined by configure script.
lib_LTLIBRARIES = @LIB_C@

# Listing of all possible targets that I may build.
EXTRA_LTLIBRARIES = libH5hutC.la

# Header files that I wish to install in $(prefix)/include
include_HEADERS =				\
	../include/H5hut.h			\
	../include/H5.h				\
	../include/H5_attribs.h			\
	../include/H5_attachments.h		\
	../include/H5Part.h                     \
	../include/H5Block.h			\
	../include/H5Block_readwrite.h		\
	../include/H5Fed.h			\
	../include/H5Fed_adjacency.h		\
	../include/H5Fed_inquiry.h		\
	../include/H5Fed_retrieve.h		\
	../include/H5Fed_store.h		\
	../include/H5Fed_tags.h

# Listing of all possible headers that I may include
EXTRA_HEADERS =

# Listing of sources
libH5hutC_la_SOURCES =				\
	H5.c					\
	H5_attribs.c				\
	H5_attachments.c			\
	H5Part.c                                \
	H5Block.c				\
	H5Block_readwrite.c

libH5hutC_la_LDFLAGS = -version-info 2:0:0 -rpath '$(libdir)'

all-local:
	$(INSTALL) -m644 .libs/libH5hutC.a ../lib

clean-local:
	$(RM) ../lib/libH5hutC.a
