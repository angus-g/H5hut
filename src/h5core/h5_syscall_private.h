#ifndef __H5_SYSCALL_H
#define __H5_SYSCALL_H

void*
h5priv_alloc (
	h5_file_t* const f,
	void* ptr,
	const size_t size
	);

void*
h5priv_calloc (
	h5_file_t* const f,
	const size_t count,
	const size_t size
	);

h5_err_t
h5priv_free (
	h5_file_t* const f,
	void* ptr
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
