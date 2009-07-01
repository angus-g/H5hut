#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>	/* va_arg - System dependent ?! */
#include <string.h>
#include <errno.h>
#include <fcntl.h>
#include <hdf5.h>

#include "h5_core.h"
#include "h5_core_private.h"


h5_err_t
_h5t_error_local_elem_nexist (
	h5_file_t * const f,
	h5_id_t local_vids[]
	) {
	switch ( f->t->mesh_type ) {
	case H5_OID_TETRAHEDRON:
		return h5_error (
			f,
			H5_ERR_NOENTRY,
			"Tetrahedron with local vertex IDs "
			"(%lld,%lld,%lld,%lld) doesn't exist!",
			local_vids[0], local_vids[1],
			local_vids[2], local_vids[3] );
	case H5_OID_TRIANGLE:
		return h5_error (
			f,
			H5_ERR_NOENTRY,
			"Triangle with local vertex IDs "
			"(%lld,%lld,%lld) doesn't exist!",
			local_vids[0], local_vids[1], local_vids[2] );
	default:
		return h5_error_internal( f, __FILE__, __func__, __LINE__ );
	}
}
	
