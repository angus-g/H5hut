# COMPILERS
CC = @CC@
CXX = @CXX@

LIBS = @LIBS@
AM_CPPFLAGS = -I../include @AM_CPPFLAGS@

OBJEXT = o

lib_LIBRARIES = libH5XX.a
include_HEADERS = H5Fed.hh
libH5XX_a_SOURCES = H5Fed.cc

libH5XX.a: $(libH5XX_a_OBJECTS)
	${AR} rucs $@ $^

%.o : %.cc
	$(CXX) $(CFLAGS) $(INC) -c $<
