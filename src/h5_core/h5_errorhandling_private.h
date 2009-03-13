#ifndef __H5_ERRORHANDLING_PRIVATE_H
#define __H5_ERRORHANDLING_PRIVATE_H

#define h5_error_not_implemented( f, file, func, lino )		     \
	h5_error(						     \
		f,						     \
		H5_ERR_NOT_IMPLEMENTED,				     \
		"%s: Function \"%s\", line %d not yet implemented!", \
		file, func, lino );

#define h5_error_internal( f, file, func, lino )   \
	h5_error(				   \
		f,				   \
		H5_ERR_INTERNAL,		   \
		"%s: Internal error: %s line %d!", \
		file, func, lino )

#define HANDLE_H5_NOENT_ERR( f, name )				\
	h5_error(						\
		f,						\
		H5_ERR_NOENT,				\
		"Object \"%s\" doesn't exists.", name );

#define HANDLE_H5_DATASET_RANK_ERR( f, m, n )			  \
	h5_error(						  \
		f,						  \
		H5_ERR_INVAL,				  \
		"Wrong rank of dataset: Is %d, but should be %d", \
		m, n );

#define HANDLE_H5_GROUP_EXISTS_ERR( f, name )		\
	h5_error(					\
		f,					\
		H5_ERR_INVAL,			\
		"Group \"%s\" already exists", name )

/**************** H5 *********************/

#define HANDLE_H5_BADFD_ERR( f )			\
	h5_error(					\
		f,					\
		H5_ERR_BADFD,				\
		"Called with bad filehandle." );

#define HANDLE_H5_NOMEM_ERR( f )		\
	h5_error(				\
		f,				\
		H5_ERR_NOMEM,			\
		"Out of memory." );

#define HANDLE_H5_SETSTEP_ERR( f, rc, step )			\
	h5_error(						\
		f,						\
		h5_get_funcname(),				\
		rc,						\
		"Cannont set step to %lld.", (long long)step );

#define _h5_handle_file_mode_error( f, mode_id )		  \
	h5_error(						  \
		f,						  \
		H5_ERR_BADF,					  \
		"Operation not permitted in mode \"%s\"",	  \
		H5_O_MODES[mode_id] );

#define HANDLE_H5_FILE_ACCESS_TYPE_ERR( f, flags )		\
	h5_error(						\
		f,						\
		H5_ERR_INVAL,					\
		"Invalid file access mode \"%d\".", flags);

#define HANDLE_H5_STEP_EXISTS_ERR( f, step )				\
	h5_error(							\
		f,							\
		H5_ERR_INVAL,						\
		"Step #%lld already exists, step cannot be set "	\
		"to an existing step in write and append mode",		\
		(long long)step );

#define HANDLE_H5_NOENTRY_ERR( f, group_name, type, idx )	     \
	h5_error(						     \
		f,						     \
		H5_ERR_NOENTRY,					     \
		"No entry with index %lld and type %d in group %s!", \
		(long long)idx, type, group_name );


#define HANDLE_H5_OVERFLOW_ERR( f, otype, max )			\
	h5_error(						\
		f,						\
		H5_ERR_INVAL,					\
		"Cannot store more than %lld %s", max, otype );

#define HANDLE_H5_PARENT_ID_ERR( f, otype, parent_id  )			\
	h5_error(							\
		f,							\
		H5_ERR_INVAL,						\
		"Impossible parent_id %lld for %s.",	\
		parent_id, otype );

#define HANDLE_H5_OUT_OF_RANGE_ERR( f, otype, oid )	\
	h5_error(					\
		f,					\
		H5_ERR_INVAL,				\
		"%s id %lld out of range",		\
		otype, oid );

/**************** HDF5 *********************/
/* H5A: Attribute */
#define HANDLE_H5A_GET_NUM_ATTRS_ERR( f )		\
	h5_error(					\
		f,					\
		H5_ERR_HDF5,				\
		"Cannot get number of attributes." );

#define HANDLE_H5A_OPEN_IDX_ERR( f, n )				      \
	h5_error(						      \
		f,						      \
		H5_ERR_HDF5,					      \
		"Cannot open attribute specified by index \"%lld\".", \
		(long long)n );

/* H5G: group */
#define HANDLE_H5G_GET_OBJINFO_ERR( f, s )				\
	h5_error(							\
		f,							\
		H5_ERR_HDF5,						\
		"Cannot get information about object \"%s\".", s );

/* H5P: property */
#define HANDLE_H5P_SET_FAPL_MPIO_ERR( f )				\
	h5_error(							\
		f,							\
		H5_ERR_HDF5,						\
		"Cannot store IO communicator information to the "	\
		"file access property list.");

/* H5S: dataspace */
#define HANDLE_H5S_CREATE_SIMPLE_3D_ERR( f, dims )			\
	h5_error(							\
		f,							\
		H5_ERR_HDF5,						\
		"Cannot create 3d dataspace with dimension sizes "	\
		"\"(%lld,%lld,%lld)\".",				\
		(long long)dims[0], (long long)dims[1], (long long)dims[2] );

#define HANDLE_H5S_GET_SELECT_NPOINTS_ERR(f)				\
	h5_error(							\
		f,							\
		H5_ERR_HDF5,						\
		"Cannot determine the number of elements"		\
		"in dataspace selection." ); 

#define HANDLE_H5S_GET_SIMPLE_EXTENT_NPOINTS_ERR( f )			\
	h5_error(							\
		f,							\
		H5_ERR_HDF5,						\
		"Cannot determine number of elements in dataspace." ); 

#define HANDLE_H5S_SELECT_HYPERSLAB_ERR( f )				\
	h5_error(							\
		f,							\
		H5_ERR_HDF5,						\
		"Cannot set select hyperslap region or add the "	\
		"specified region" );

#define HANDLE_H5S_GET_SIMPLE_EXTENT_DIMS_ERR( f )		\
	h5_error(						\
		f,						\
		H5_ERR_HDF5,				\
		"Cannot get dimension sizes of dataset" );


/* MPI */
#define HANDLE_MPI_ALLGATHER_ERR( f )		\
	h5_error(				\
		f,				\
		 H5_ERR_MPI,			\
		"Cannot gather data." );

#define HANDLE_MPI_COMM_SIZE_ERR( f )					\
	h5_error(							\
		f,							\
		H5_ERR_MPI,						\
		"Cannot get number of processes in my group." );

#define HANDLE_MPI_COMM_RANK_ERR( f )					\
	h5_error(							\
		f,							\
		H5_ERR_MPI,						\
		"Cannot get rank of the calling process in my group." );

#define HANDLE_MPI_UNAVAILABLE_ERR( f )		\
	h5_error(				\
		f,				\
		H5_ERR_MPI,			\
		"MPI not available" );

#endif
