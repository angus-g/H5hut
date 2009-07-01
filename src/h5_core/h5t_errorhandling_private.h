#ifndef __H5T_ERRORHANDLING_PRIVATE_H
#define __H5T_ERRORHANDLING_PRIVATE_H

h5_err_t
_h5t_error_local_elem_nexist (
	h5_file_t * const f,
	h5_id_t local_vids[]
	);

#define _h5t_error_undef_mesh( f )		\
	h5_error(				\
		f,				\
		H5_ERR_INVAL,			\
		"Mesh not yet defined." );

#define _h5t_error_undef_level( f )	\
	h5_error(				\
		f,				\
		H5_ERR_INVAL,			\
		"Level not defined." );


#define _h5t_error_nexist_level( f, level_id )	\
	h5_error(				\
		f,				\
		H5_ERR_INVAL,			\
		"Level %lld doesn't exist.", level_id );

#define _h5t_error_global_id_nexist( f, name, id )		\
	h5_error(						\
		f,						\
		H5_ERR_NOENTRY,					\
		"%s with global id %lld does not exist!",	\
		name, id );


#define _h5t_error_global_triangle_id_nexist( f, vids )			\
	h5_error(							\
		f,							\
		H5_ERR_NOENTRY,						\
		"Triangle with global vertex ids (%lld,%lld,%lld) doesn't exist!", \
		vids[0], vids[1], vids[2] );

#define _h5t_error_local_triangle_nexist( f, local_vids )			\
	h5_error(							\
		f,							\
		H5_ERR_NOENTRY,						\
		"Triangle with global vertex ids (%lld,%lld,%lld) doesn't exist!", \
		local_vids[0], local_vids[1], local_vids[2] );


#define _h5t_error_store_boundaryface_local_id( f, local_fid )	     \
	h5_error(						     \
		f,						     \
		H5_ERR_INVAL,					     \
		"Boundary face with local id %lld is not on level 0!", \
		local_fid );

#endif
