#ifndef __H5_HSEARCH_PRIVATE_H
#define __H5_HSEARCH_PRIVATE_H

typedef struct hsearch_data {
	struct _ENTRY *table;
	unsigned int size;
	unsigned int filled;
	int (*compare)(const void*, const void*);
	unsigned int (*compute_hash)(const void*);
} h5_hashtable_t; 

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
_h5_hsearch (
	h5_file_t * const f,
	void *item,
	const h5_action_t action,
	void **retval,
	h5_hashtable_t *htab
	);

extern h5_err_t
_h5_hcreate (
	h5_file_t* const f,
	size_t __nel,
	h5_hashtable_t *__htab,
	int (*compare)(const void*, const void*),
	unsigned int (*compute_hash)(const void*)
	);

extern h5_err_t
_h5_hresize (
	h5_file_t * const f,
	size_t nel,
	h5_hashtable_t *htab
	);

extern h5_err_t
_h5_hdestroy (
	h5_file_t* f,
	h5_hashtable_t *__htab
	);

extern h5_err_t
_h5_hwalk (
	h5_file_t* f,
	h5_hashtable_t *__htab,
	h5_err_t (*visit)(h5_file_t*const f, const void *__item)
	);

extern h5_err_t
_h5_hcreate_string_keyed (
	h5_file_t * const f,
	size_t nel,
	h5_hashtable_t *htab 
	);

h5_err_t
_h5_hcreate_id_keyed (
	h5_file_t * const f,
	size_t nel,
	h5_hashtable_t *htab 
	);

#endif
