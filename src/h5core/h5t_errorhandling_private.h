#ifndef __H5T_ERRORHANDLING_PRIVATE_H
#define __H5T_ERRORHANDLING_PRIVATE_H

h5_err_t
h5tpriv_error_local_elem_nexist (
	h5_file_t * const f,
	h5_id_t local_vertex_indices[]
	);

#define h5tpriv_error_undef_mesh( f )		\
	h5_error(				\
		f,				\
		H5_ERR_INVAL,			\
		"Mesh not yet defined." );

#define h5tpriv_error_undef_level( f )	\
	h5_error(				\
		f,				\
		H5_ERR_INVAL,			\
		"Level not defined." );


#define h5tpriv_error_nexist_level( f, level_id )	\
	h5_error(				\
		f,				\
		H5_ERR_INVAL,			\
		"Level %lld doesn't exist.", (long long)level_id );

#define h5tpriv_error_global_id_nexist( f, name, id )		\
	h5_error(						\
		f,						\
		H5_ERR_NOENTRY,					\
		"%s with global id %lld does not exist!",	\
		name, (long long)id );


#define h5tpriv_error_global_triangle_id_nexist( f, vids )			\
	h5_error(							\
		f,							\
		H5_ERR_NOENTRY,						\
		"Triangle with global vertex ids (%lld,%lld,%lld) doesn't exist!", \
		(long long)vids[0], (long long)vids[1], (long long)vids[2] );

#define h5tpriv_error_local_triangle_nexist( f, indices )	\
	h5_error(							\
		f,							\
		H5_ERR_NOENTRY,						\
		"Triangle with global vertex ids (%lld,%lld,%lld) doesn't exist!", \
		(long long)indices[0], (long long)indices[1], (long long)indices[2] );


#define h5tpriv_error_store_boundaryface_local_id( f, local_fid )	     \
	h5_error(						     \
		f,						     \
		H5_ERR_INVAL,					     \
		"Boundary face with local id %lld is not on level 0!", \
		(long long)local_fid );

#endif
