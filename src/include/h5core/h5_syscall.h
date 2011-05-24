#ifndef __H5_SYSCALL_H
#define __H5_SYSCALL_H

#ifdef __cplusplus
extern "C" {
#endif

void*
h5_alloc (
	void* ptr,
	const size_t size
	);

void*
h5_calloc (
	const size_t count,
	const size_t size
	);

h5_err_t
h5_free (
	void* ptr
	);

#ifdef __cplusplus
}
#endif

#endif
