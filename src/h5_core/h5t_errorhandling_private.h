#ifndef __T_ERRORHANDLING_H
#define __T_ERRORHANDLING_H

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

#define H5T_HANDLE_GID_NOT_EXIST_ERR( name, id )	\
	(*h5_get_errorhandler()) (			\
		h5_get_funcname(),			    \
		H5_ERR_NOENTRY,				    \
		"Global %s id %ld does not exist!",	    \
		name, (long)id );

#define _h5t_handle_global_id_not_exist_err( name, id )	\
	(*h5_get_errorhandler()) (			\
		h5_get_funcname(),			    \
		H5_ERR_NOENTRY,				    \
		"%s with global id %ld does not exist!",	    \
		name, (long)id );

#define _h5t_handle_get_global_tet_id_err( vids ) \
	(*h5_get_errorhandler()) (		  \
		h5_get_funcname(),		  \
		H5_ERR_NOENTRY,						\
		"Tetrahedron with global vertex ids (%d,%d,%d,%d) doesn't exist!", \
		vids[0], vids[1], vids[2], vids[3] );

#define _h5t_handle_get_global_tri_id_err( vids ) \
	(*h5_get_errorhandler()) (		  \
		h5_get_funcname(),		  \
		H5_ERR_NOENTRY,						\
		"Triangle with global vertex ids (%d,%d,%d) doesn't exist!", \
		vids[0], vids[1], vids[2] );

#define _h5t_handle_get_local_tet_id_err( vids ) \
	(*h5_get_errorhandler()) (		 \
		h5_get_funcname(),		 \
		H5_ERR_NOENTRY,						\
		"Tetrahedron with local vertex ids (%d,%d,%d,%d) doesn't exist!", \
		vids[0], vids[1], vids[2], vids[3] );

#define _h5t_handle_get_local_triangle_id_err( vids ) \
	(*h5_get_errorhandler()) (		 \
		h5_get_funcname(),		 \
		H5_ERR_NOENTRY,						\
		"Triangle with local vertex ids (%d,%d,%d) doesn't exist!", \
		vids[0], vids[1], vids[2] );

#define _h5t_handle_get_global_triangle_id_err( vids ) \
	(*h5_get_errorhandler()) (		 \
		h5_get_funcname(),		 \
		H5_ERR_NOENTRY,						\
		"Triangle with global vertex ids (%d,%d,%d) doesn't exist!", \
		vids[0], vids[1], vids[2] );

#endif
