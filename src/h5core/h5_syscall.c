#include <stdlib.h>
#include <search.h>
#include <strings.h>

#include "h5core/h5_core.h"
#include "h5_core_private.h"

void*
h5_alloc (
	h5_file_t* const f,
	void* ptr,
	const size_t size
	) {
	h5_debug (f, "%s (ptr=%p, size=%lu)", __func__, ptr, size); 
	ptr = realloc (ptr, size);
	if (ptr == NULL) {
		h5_error (f, H5_ERR_NOMEM, "Out of memory.");
		return (void*)(H5_ERR);
	}
	h5_debug (f, "%s (): return address: 0x%p", __func__, ptr);
	return ptr;
}

void*
h5_calloc (
	h5_file_t* const f,
	const size_t count,
	const size_t size
	) {
	h5_debug (f, "%s (count=%lu , size=%lu)", __func__, count, size);
	void* ptr = calloc (count, size);
	if (ptr == NULL) {
		h5_error (f, H5_ERR_NOMEM, "Out of memory.");
		return (void*)(H5_ERR);
	}
	h5_debug (f, "%s (): return address: 0x%p", __func__, ptr);
	return ptr;
}

char*
h5priv_strdup (
	h5_file_t* const f,
	const char* s1
	) {
	char* s2 = strdup (s1);
	if (s2 == NULL) {
		h5_error (f, H5_ERR_NOMEM, "Out of memory.");
		return (void*)(H5_ERR);
	}
	return s2;
}

h5_err_t
h5_free (
	h5_file_t* const f,
	void* ptr
	) {
	if (ptr) {
		h5_debug (f, "%s (%p)", __func__,  ptr); 
		free (ptr);
	}
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
	UNUSED_ARGUMENT (f);
	void* ptr = tfind (key, rootp, compar);
	if (ptr == NULL) {
		return (void*)(H5_ERR);
	}
	return ptr;
}


