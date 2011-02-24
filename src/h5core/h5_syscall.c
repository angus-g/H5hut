#include <stdlib.h>
#include <search.h>
#include <string.h>

#include "h5core/h5_core.h"
#include "h5_core_private.h"

void_p
h5_alloc (
	void* ptr,
	const size_t size
	) {
	MALLOC_WRAPPER_ENTER2 (void_p,
			       "ptr=%p, size=%lu", ptr, size); 
	ptr = realloc (ptr, size);
	if (ptr == NULL) {
		MALLOC_WRAPPER_LEAVE (
			(void_p)h5_error (H5_ERR_NOMEM, "Out of memory."));
	}
	MALLOC_WRAPPER_RETURN (ptr);
}

void_p
h5_calloc (
	const size_t count,
	const size_t size
	) {
	MALLOC_WRAPPER_ENTER2 (void_p,
			      "count=%lu , size=%lu", count, size);
	void* ptr = calloc (count, size);
	if (ptr == NULL) {
		MALLOC_WRAPPER_LEAVE (
			(void_p)h5_error (H5_ERR_NOMEM, "Out of memory."));
	}
	MALLOC_WRAPPER_RETURN (ptr);
}

char_p
h5priv_strdup (
	const char* s1
	) {
	MALLOC_WRAPPER_ENTER1 (char_p, "s=%s", s1);
	
	char_p s2 = h5_calloc (1, strlen (s1)+1 );
	if (s2 == NULL) {
		MALLOC_WRAPPER_LEAVE (
			(char_p)h5_error (H5_ERR_NOMEM, "Out of memory."));
	}
	MALLOC_WRAPPER_RETURN (strcpy (s2, s1));

}

h5_err_t
h5_free (
	void* ptr
	) {
	MALLOC_WRAPPER_ENTER1 (h5_err_t, "ptr=0x%p", ptr);
	if (ptr) {
		free (ptr);
	}
	MALLOC_WRAPPER_RETURN (H5_SUCCESS);
}

void*
h5priv_tsearch (
	const void* key,
	void** rootp,
	int (*compar) (const void* key1, const void* key2) 
	) {
	void* ptr = tsearch (key, rootp, compar);
	if (ptr == NULL) {
		h5_error (H5_ERR_NOMEM, "Out of memory.");
		return (void*)(H5_ERR);
	}
	return ptr;
}

void*
h5priv_tfind (
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


