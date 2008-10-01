#include <stdlib.h>
#include <hdf5.h>
#include "h5_core.h"
#include "h5_core_private.h"

void *
_h5_alloc (
	void *ptr,
	const size_t size ) {
	ptr = realloc ( ptr, size );
	if ( ptr == NULL ) {
		return (void*)HANDLE_H5_NOMEM_ERR;
	}
	return ptr;
}
