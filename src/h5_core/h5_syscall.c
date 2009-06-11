#include <stdlib.h>
#include <hdf5.h>
#include <search.h>
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

h5_err_t
_h5_free (
	h5_file_t * const f,
	void *ptr
	) {
	if ( ptr ) free ( ptr );
	return H5_SUCCESS;
}
	
void *
_h5_tsearch (
	h5_file_t * const f,
	const void *key,
	void **rootp,
	int (*compar) (const void *key1, const void *key2) 
	) {
	void *ptr = tsearch ( key, rootp, compar );
	if ( ptr == NULL ) {
		h5_error (
			f,
			H5_ERR_NOMEM,
			"Out of memory." );
		return (void*)(-1);
	}
	return ptr;
}

void *
_h5_tfind (
	h5_file_t * const f,
	const void *key,
	void *const *rootp,
	int (*compar) (const void *key1, const void *key2) 
	) {
	void *ptr = tfind ( key, rootp, compar );
	if ( ptr == NULL ) {
		return (void*)(-1);
	}
	return ptr;
}
