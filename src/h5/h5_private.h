#ifndef __H5_PRIVATE_H
#define __H5_PRIVATE_H

#include "errorhandling_private.h"

#define H5B_CONTAINER_GRPNAME		"Block"

#define H5T_CONTAINER_GRPNAME		"Topo"
#define H5T_COORD_GRPNAME		"COORD"
#define H5T_COORD3D_DSNAME		"COORD3D"
#define H5T_COORD3D_NUM_ELEMS_DSNAME	"COORD3D_NUM_ELEMS"
#define H5T_VMESH_GRPNAME		"VOLUME_MESH"
#define H5T_TETMESH_DSNAME		"TETMESH"
#define H5T_TETMESH_NUM_ELEMS_DSNAME	"TETMESH_NUM_ELEMS"

#define H5BLOCK_GROUPNAME_BLOCK	H5B_CONTAINER_GRPNAME

#define SET_FNAME( fname )	H5_set_funcname( fname );

#define CHECK_FILEHANDLE( f ) \
	if ( f == NULL ) \
		return HANDLE_H5_BADFD_ERR;

#define CHECK_WRITABLE_MODE( f )  \
	if ( f->mode==H5PART_READ ) \
		return (*H5_get_errorhandler()) (	\
			H5_get_funcname(), \
			H5_ERR_INVAL, \
			"Attempting to write to read-only file" );

#define CHECK_READONLY_MODE( f )  \
	if ( ! f->mode==H5PART_READ ) \
		return (*H5_get_errorhandler()) (	\
			H5_get_funcname(), \
			H5_ERR_INVAL, \
			"Operation is not allowed on writable files." );

#define CHECK_TIMEGROUP( f ) \
	if ( f->step_gid <= 0 ) \
		return (*H5_get_errorhandler()) (	\
			H5_get_funcname(), \
			H5_ERR_INVAL, \
			"Internal error: step_gid <= 0.");


#endif
