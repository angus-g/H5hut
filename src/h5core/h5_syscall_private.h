#ifndef __H5_SYSCALL_PRIVATE_H
#define __H5_SYSCALL_PRIVATE_H

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
