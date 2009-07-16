#ifndef __H5_SYSCALL_H
#define __H5_SYSCALL_H

void *
_h5_alloc (
	h5_file_t * const f,
	void *ptr,
	const size_t size
	);

void *
_h5_calloc (
	h5_file_t * const f,
	const size_t count,
	const size_t size
	);

h5_err_t
_h5_free (
	h5_file_t * const f,
	void *ptr
	);

void *
_h5_tsearch (
	h5_file_t * const f,
	const void *key,
	void **rootp,
	int (*compar) (const void *key1, const void *key2) 
	);

void *
_h5_tfind (
	h5_file_t * const f,
	const void *key,
	void *const *rootp,
	int (*compar) (const void *key1, const void *key2) 
	);
#endif
