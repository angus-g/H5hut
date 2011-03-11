#ifndef __H5_ERRORHANDLING_PRIVATE_H
#define __H5_ERRORHANDLING_PRIVATE_H

#define HANDLE_H5_DATASET_RANK_ERR( m, n )			  \
	h5_error(						  \
		 H5_ERR_INVAL,					  \
		"Wrong rank of dataset: Is %d, but should be %d", \
		m, n );

#define HANDLE_H5_GROUP_EXISTS_ERR( name )		\
	h5_error(					\
		H5_ERR_INVAL,				\
		"Group \"%s\" already exists", name )

/**************** H5 *********************/

#define h5priv_handle_file_mode_error( mode_id )		  \
	h5_error(						  \
		H5_ERR_BADF,					  \
		"Operation not permitted in mode \"%s\"",	  \
		H5_O_MODES[mode_id] );

#define HANDLE_H5_STEP_EXISTS_ERR( step )				\
	h5_error(							\
		H5_ERR_INVAL,						\
		"Step #%lld already exists, step cannot be set "	\
		"to an existing step in write and append mode",		\
		(long long)step );

#define HANDLE_H5_NOENTRY_ERR( group_name, type, idx )	     \
	h5_error(						     \
		H5_ERR_NOENTRY,					     \
		"No entry with index %lld and type %d in group %s!", \
		(long long)idx, type, group_name );


#define HANDLE_H5_OVERFLOW_ERR( otype, max )			\
	h5_error(						\
		H5_ERR_INVAL,					\
		"Cannot store more than %lld %s", (long long)max, otype );

#define HANDLE_H5_PARENT_ID_ERR( otype, parent_id  )			\
	h5_error(							\
		H5_ERR_INVAL,						\
		"Impossible parent_id %lld for %s.",	\
		(long long)parent_id, otype );

#define HANDLE_H5_OUT_OF_RANGE_ERR( otype, oid )	\
	h5_error(					\
		H5_ERR_INVAL,				\
		"%s id %lld out of range",		\
		otype, (long long)oid );

#endif
