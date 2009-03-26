#include <stdlib.h>
#include <hdf5.h>
#include "h5_core.h"
#include "h5_core_private.h"

void *
_h5_alloc (
	h5_file_t * const f,
	void *ptr,
	const size_t size ) {
	h5_debug ( f, "Allocating %ld bytes.", size ); 
	ptr = realloc ( ptr, size );
	if ( ptr == NULL ) {
		h5_error (
			f,
			H5_ERR_NOMEM,
			"Out of memory." );
		return (void*)(-1);
	}
	return ptr;
}
