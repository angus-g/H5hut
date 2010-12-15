#ifndef __H5_SYSCALL_H
#define __H5_SYSCALL_H

void*
h5_alloc (
	h5_file_t* const f,
	void* ptr,
	const size_t size
	);

void*
h5_calloc (
	h5_file_t* const f,
	const size_t count,
	const size_t size
	);

h5_err_t
h5_free (
	h5_file_t* const f,
	void* ptr
	);

char*
h5priv_strdup (
	h5_file_t* const f,
	const char* s1
	);
void*
h5priv_tsearch (
	h5_file_t* const f,
	const void* key,
	void** rootp,
	int (*compar) (const void* key1, const void* key2) 
	);

void*
h5priv_tfind (
	h5_file_t* const f,
	const void* key,
	void *const* rootp,
	int (*compar) (const void* key1, const void* key2) 
	);
#endif
