#ifndef __H5_QSORT_PRIVATE_H
#define __H5_QSORT_PRIVATE_H

void
h5priv_qsort_r ( 
	void *a,
	size_t n,
	size_t es,
	void *thunk,
	int (*compar)(void *, const void *, const void *)
	);

void
h5priv_qsort (
	void *a,
	size_t n,
	size_t es,
	int (*compar)(const void *, const void *)
	);

#endif
