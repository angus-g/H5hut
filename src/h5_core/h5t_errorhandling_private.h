#ifndef __H5T_ERRORHANDLING_PRIVATE_H
#define __H5T_ERRORHANDLING_PRIVATE_H


h5_err_t
_h5t_handle_get_global_entity_id_err (
	h5_file *f,
	const h5_id_t * const global_vids
	);

h5_err_t
_h5t_handle_get_local_entity_id_err (
	h5_file *f,
	const h5_id_t * const local_vids
	);

h5_err_t
_h5t_error_illegal_object_type (
	h5_file_t * const f,
	h5_oid_t oid );

#define _h5t_error_undef_mesh( f )		\
	h5_error(				\
		H5_ERR_INVAL,			\
		"Mesh not yet defined." );

#define _h5t_error_undef_level( f )		\
	h5_error(				\
		H5_ERR_INVAL,			\
		"Level not yet defined." );

#define _h5t_error_global_id_nexist( name, id )	\
	h5_error( \
		H5_ERR_NOENTRY, \
		"%s with global id %ld does not exist!", \
		name, (long)id );


#define _h5t_error_global_tet_id_nexist( vids ) \
	h5_error( \
		H5_ERR_NOENTRY,						\
		"Tetrahedron with global vertex ids (%d,%d,%d,%d) doesn't exist!", \
		vids[0], vids[1], vids[2], vids[3] );

#define _h5t_error_global_tri_id_nexist( vids ) \
	h5_error( \
		H5_ERR_NOENTRY,	\
		"Triangle with global vertex ids (%d,%d,%d) doesn't exist!", \
		vids[0], vids[1], vids[2] );

#define _h5t_error_local_tet_id_nexist( vids ) \
	h5_error( \
		H5_ERR_NOENTRY,	\
		"Tetrahedron with local vertex ids (%d,%d,%d,%d) doesn't exist!", \
		vids[0], vids[1], vids[2], vids[3] );

#define _h5t_error_local_triangle_id_nexist( vids ) \
	h5_error( \
		H5_ERR_NOENTRY,						\
		"Triangle with local vertex ids (%d,%d,%d) doesn't exist!", \
		vids[0], vids[1], vids[2] );

#define _h5t_error_global_triangle_id_nexist( vids ) \
	h5_error( \
		H5_ERR_NOENTRY,	\
		"Triangle with global vertex ids (%d,%d,%d) doesn't exist!", \
		vids[0], vids[1], vids[2] );

#define _h5t_error_store_boundaryface_local_id( local_fid ) \
	h5_error( \
		H5_ERR_INVAL, \
		"Boundary face with local id %d is not on level 0!", \
		local_fid );

#endif
