#ifndef __H5_HSEARCH_PRIVATE_H
#define __H5_HSEARCH_PRIVATE_H

/* Action which shall be performed in the call to hsearch.  */
typedef enum {
	H5_FIND,
	H5_ENTER
} h5_action_t;

typedef struct h5_entry {
	void *dta;
} h5_entry_t;

/* Reentrant versions which can handle multiple hashing tables at the
   same time.  */
extern h5_err_t
_h5_hsearch_r (
	h5_file_t * const f,
	void *item,
	h5_action_t action,
	void **retval,
	struct hsearch_data *htab
	);

extern h5_err_t
_h5_hcreate_r (
	h5_file_t* const f,
	size_t __nel,
	struct hsearch_data *__htab,
	int (*compare)(void*, void*),
	unsigned int (*compute_hash)(void*)
	);

extern h5_err_t
_h5_hresize_r (
	h5_file_t * const f,
	size_t nel,
	h5_hashtable_t *htab
	);

extern h5_err_t
_h5_hdestroy_r (
	h5_file_t* f,
	struct hsearch_data *__htab
	);

extern void
_h5_hwalk_r (
	h5_file_t* f,
	struct hsearch_data *__htab,
	void (*visit)(const void *__item)
	);
#endif
