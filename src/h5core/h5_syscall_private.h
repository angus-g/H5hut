#ifndef __H5_SYSCALL_PRIVATE_H
#define __H5_SYSCALL_PRIVATE_H

void*
h5priv_tsearch (
	const void* key,
	void** rootp,
	int (*compar) (const void* key1, const void* key2) 
	);

void*
h5priv_tfind (
	const void* key,
	void *const* rootp,
	int (*compar) (const void* key1, const void* key2) 
	);
#endif
