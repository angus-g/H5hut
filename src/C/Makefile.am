# src/C level Makefile.am

OBJEXT=o

LIBS = @LIBS@
INCLUDES = -I../include @INCLUDES@

# What to build... Will be determined by configure script.
lib_LIBRARIES = @LIB_C@

# Listing of all possible targets that I may build.
EXTRA_LIBRARIES = ../lib/libH5hutC.a

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
___lib_libH5hutC_a_SOURCES =			\
	H5.c					\
	H5_attribs.c				\
	H5_attachments.c			\
	H5Part.c                                \
	H5Block.c				\
	H5Block_readwrite.c			\
	H5Fed.c					\
	H5Fed_adjacency.c			\
	H5Fed_inquiry.c				\
	H5Fed_retrieve.c			\
	H5Fed_store.c				\
	H5Fed_tags.c

all: all-am

clean: clean-am

