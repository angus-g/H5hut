#include <stdlib.h>
#include <search.h>

#include "h5core/h5_core.h"
#include "h5_core_private.h"

void*
h5priv_alloc (
	h5_file_t* const f,
	void* ptr,
	const size_t size
	) {
	h5_debug (f, "Allocating %lu bytes.", size); 
	ptr = realloc (ptr, size);
	if (ptr == NULL) {
		h5_error (f, H5_ERR_NOMEM, "Out of memory.");
		return (void*)(H5_ERR);
	}
	return ptr;
}

void*
h5priv_calloc (
	h5_file_t* const f,
	const size_t count,
	const size_t size
	) {
	h5_debug (f, "Allocating %lu * %lu bytes.", count, size); 
	void* ptr = calloc (count, size);
	if (ptr == NULL) {
		h5_error (f, H5_ERR_NOMEM, "Out of memory.");
		return (void*)(H5_ERR);
	}
	return ptr;
}

h5_err_t
h5priv_free (
	h5_file_t* const f,
	void* ptr
	) {
	if (ptr) free (ptr);
	return H5_SUCCESS;
}

void*
h5priv_tsearch (
	h5_file_t* const f,
	const void* key,
	void** rootp,
	int (*compar) (const void* key1, const void* key2) 
	) {
	void* ptr = tsearch (key, rootp, compar);
	if (ptr == NULL) {
		h5_error (f, H5_ERR_NOMEM, "Out of memory.");
		return (void*)(H5_ERR);
	}
	return ptr;
}

void*
h5priv_tfind (
	h5_file_t* const f,
	const void* key,
	void* const* rootp,
	int (*compar) (const void* key1, const void* key2) 
	) {
	void* ptr = tfind (key, rootp, compar);
	if (ptr == NULL) {
		return (void*)(H5_ERR);
	}
	return ptr;
}
