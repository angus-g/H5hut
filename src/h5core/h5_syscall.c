#include <stdlib.h>
#include <search.h>
#include <string.h>

#include "h5core/h5_core.h"
#include "h5_core_private.h"


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


