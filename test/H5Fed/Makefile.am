# test level Makefile.am

OBJEXT = o

# COMPILER SETTING
CXX = @CXX@
FC = @FC@
MPIFC = @MPIFC@
MPICXX = @MPICXX@
MPICC = @MPICC@

# COMPILER FLAG SETTING
AM_CFLAGS = @CFLAGS@
FFLAGS = @FFLAGS@

AM_LDFLAGS = -L../../src/lib @LDFLAGS@
LIBS = -lH5hutC -lH5hut @LIBS@
INCLUDES = -I../../src/include @INCLUDES@

noinst_PROGRAMS =				\
		tetmesh_write			\
		tetmesh_write2			\
		tetmesh_read			\
		tetmesh_adjacencies		\
		tetmesh_write_tags		\
		tetmesh_read_tags		\
		trimesh_read			\
		trimesh_write

#		map_tet2globalid \
#		map_triangle2globalid

# Extra files that I wish to include in the dist tar ball.
EXTRA_DIST =					\
		tetmesh_write.c			\
		tetmesh_write2.c		\
		tetmesh_read.c			\
		tetmesh_adjacencies.c		\
		tetmesh_write_tags.c		\
		tetmesh_read_tags.c		\
		trimesh_read.c			\
		trimesh_write.c

#		map_tet2globalid.c \
#		map_triangle2globalid.c \
#		$(bin_SCRIPTS)

all:	$(noinst_PROGRAMS)

clean: clean-am
	rm -f *.h5

