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

#define _h5_handle_file_mode_error( f, mode_id )		  \
	h5_error(						  \
		f,						  \
		H5_ERR_BADF,					  \
		"Operation not permitted in mode \"%s\"",	  \
		H5_O_MODES[mode_id] );

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

#endif
