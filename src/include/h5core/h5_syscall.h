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
#endif
