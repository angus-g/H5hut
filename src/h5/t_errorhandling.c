#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>	/* va_arg - System dependent ?! */
#include <string.h>
#include <errno.h>
#include <fcntl.h>
#include <hdf5.h>

#include "h5_core.h"
#include "h5_private.h"

h5_err_t
_h5t_handle_get_global_entity_id_err (
	h5_file *f,
	const h5_id_t * const global_vids
	) {
	struct h5t_fdata *t = &f->t;
	switch ( t->mesh_type ) {
	case TETRAHEDRAL_MESH:
		return _h5t_handle_get_global_tet_id_err ( global_vids );
	case TRIANGLE_MESH:
		return _h5t_handle_get_global_tri_id_err ( global_vids );
	}
	return -1;
}
	
h5_err_t
_h5t_handle_get_local_entity_id_err (
	h5_file *f,
	const h5_id_t * const local_vids
	) {
	struct h5t_fdata *t = &f->t;
	switch ( t->mesh_type ) {
	case TETRAHEDRAL_MESH:
		return _h5t_handle_get_local_tet_id_err ( local_vids );
	case TRIANGLE_MESH:
		return _h5t_handle_get_local_triangle_id_err ( local_vids );
	}
	return -1;
}
	
