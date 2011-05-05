# test level Makefile.am

OBJEXT = o

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
		trimesh_write			\
		trimesh_adjacencies		\
		trimesh_write_dunetest

#		map_tet2globalid \
#		map_triangle2globalid

# Extra files that I wish to include in the dist tar ball.
EXTRA_DIST =					\
		tetmesh_adjacencies.c		\
		tetmesh_read.c			\
		tetmesh_read_tags.c		\
		tetmesh_write.c			\
		tetmesh_write2.c		\
		tetmesh_write_tags.c		\
		trimesh_adjacencies.c		\
		trimesh_read.c			\
		trimesh_write.c			\
		trimesh_write_dunetest.c

#		map_tet2globalid.c \
#		map_triangle2globalid.c \
#		$(bin_SCRIPTS)

clean: clean-am
	rm -f *.h5

