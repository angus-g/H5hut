#ifndef __H5_SYSCALL_H
#define __H5_SYSCALL_H

#include <stdlib.h>
#include <string.h>
#include "h5_errorhandling.h"

#define MALLOC_WRAPPER_ENTER(type, fmt, ...)				\
	__FUNC_ENTER(type, H5_DEBUG_MALLOC, fmt, __VA_ARGS__)
#define MALLOC_WRAPPER_LEAVE(value)	__FUNC_LEAVE(value)
#define MALLOC_WRAPPER_RETURN(value)	__FUNC_RETURN(value, H5_DEBUG_MALLOC)

#ifdef __cplusplus
extern "C" {
#endif

static inline void_p
h5_alloc (
	void* ptr,
	const size_t size
	) {
	MALLOC_WRAPPER_ENTER (void_p, "ptr=%p, size=%lu", ptr, size); 
	ptr = realloc (ptr, size);
	if (ptr == NULL) {
		MALLOC_WRAPPER_LEAVE (
			(void_p)h5_error (H5_ERR_NOMEM, "Out of memory."));
	}
	MALLOC_WRAPPER_RETURN (ptr);
}

static inline void_p
h5_calloc (
	const size_t count,
	const size_t size
	) {
	MALLOC_WRAPPER_ENTER (void_p, "count=%zu , size=%zu", count, size);
	void* ptr = calloc (count, size);
	if (ptr == NULL) {
		MALLOC_WRAPPER_LEAVE (
			(void_p)h5_error (H5_ERR_NOMEM, "Out of memory."));
	}
	MALLOC_WRAPPER_RETURN (ptr);
}

static inline h5_err_t
h5_free (
	void* ptr
	) {
	MALLOC_WRAPPER_ENTER (h5_err_t, "ptr=%p", ptr);
	if (ptr) {
		free (ptr);
	}
	MALLOC_WRAPPER_RETURN (H5_SUCCESS);
}

static inline char_p
h5_strdup (
	const char* s1
	) {
	MALLOC_WRAPPER_ENTER (char_p, "s='%s'", s1);
	
	char_p s2 = h5_calloc (1, strlen (s1)+1 );
	if (s2 == NULL) {
		MALLOC_WRAPPER_LEAVE (
			(char_p)h5_error (H5_ERR_NOMEM, "Out of memory."));
	}
	MALLOC_WRAPPER_RETURN (strcpy (s2, s1));
}

#ifdef __cplusplus
}
#endif

#endif
