/* Copyright (C) 1993,1995-1997,2002,2005,2007,2008
   Free Software Foundation, Inc.
   This file is part of the GNU C Library.
   Contributed by Ulrich Drepper <drepper@gnu.ai.mit.edu>, 1993.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307 USA.  */

#include <errno.h>
#include <string.h>

#include "h5core/h5_core.h"
#include "h5_core_private.h"

/* [Aho,Sethi,Ullman] Compilers: Principles, Techniques and Tools, 1986
   [Knuth]            The Art of Computer Programming, part 3 (6.4)  */


/* The reentrant version has no static variables to maintain the state.
   Instead the interface of all functions is extended to take an argument
   which describes the current status.  */
typedef struct _ENTRY {
	unsigned int used;
	void* entry;
} _ENTRY;


/* For the used double hash method the table size has to be a prime. To
   correct the user given table size we need a prime test.  This trivial
   algorithm is adequate because
   a)  the code is (most probably) called a few times per program run and
   b)  the number is small because the table must fit in the core  */
static int
isprime (const unsigned int number) {
	/* no even number will be passed */
	unsigned int div = 3;

	while (div * div < number && number % div != 0)
		div += 2;

	return number % div != 0;
}

/* Before using the hash table we must allocate memory for it.
   Test for an existing table are done. We allocate one element
   more as the found prime number says. This is done for more effective
   indexing as explained in the comment for the hsearch function.
   The contents of the table is zeroed, especially the field used
   becomes zero.  */
h5_err_t
h5priv_hcreate (
	h5_file_t* const f,
	size_t nel,
	h5_hashtable_t* htab,
	int (*compare)(const void*, const void*),
	unsigned int (*compute_hash)(const void*),
	h5_err_t (*free_entry)(h5_file_t* const f, const void*)
	) {
	H5_CORE_API_ENTER;
	h5_err_t h5err = H5_SUCCESS;

	/* Test for correct arguments.  */
	if (htab == NULL || htab->table != NULL) {
		TRY2( h5_error_internal (f, __FILE__, __func__, __LINE__) );
	}
	/* Change nel to the first prime number not smaller as nel. */
	nel |= 1;      /* make odd */
	while (!isprime (nel))
		nel += 2;

	htab->size = nel;
	htab->filled = 0;
	htab->compare = compare;
	htab->compute_hash = compute_hash;
	htab->free_entry = free_entry;

	/* allocate memory and zero out */
	TRY2( (htab->table = (_ENTRY *) h5_calloc (
		       f, htab->size + 1, sizeof (_ENTRY))) );

	/* everything went alright */
	H5_CORE_API_RETURN (h5err);
}

/*
  Grow hash table.
 */
h5_err_t
h5priv_hresize (
	h5_file_t* const f,
	size_t nel,		// number of entries to grow
	h5_hashtable_t* htab	// hash table to resize
	) {
	H5_CORE_API_ENTER;
	h5_err_t h5err = H5_SUCCESS;
	if (htab == NULL || htab->table == NULL) {
		TRY2( h5_error_internal (f, __FILE__, __func__, __LINE__) );
	}
	// create new hash table
	h5_hashtable_t __htab;
	memset (&__htab, 0, sizeof (__htab));
	nel += htab->size;
	h5_debug (f, "Resize hash table from %u to %lu elements.",
		   htab->size, nel);
	TRY2( h5priv_hcreate (f, nel, &__htab, htab->compare,
			     htab->compute_hash, htab->free_entry) );

	// add all entries to new hash table
	unsigned int idx;
	for (idx = 1; idx <= htab->size; idx++) {
		if (htab->table[idx].used) {
			void* ventry;
			TRY2( h5priv_hsearch (
				      f,
				      htab->table[idx].entry,
				      H5_ENTER,
				      &ventry,
				      &__htab) );
		}
	}
	/* Free used memory.  */
	TRY2( h5_free (f, htab->table) );

	/* the sign for an existing table is an value != NULL in htable */
	htab->table = NULL;
	*htab = __htab;
	H5_CORE_API_RETURN (h5err);
}

static inline h5_err_t
hwalk (
	h5_file_t* const f,
	struct hsearch_data* htab,
	h5_err_t (*visit)(h5_file_t*const f, const void *item)
	) {
	unsigned int idx = 1;
	for (idx = 1; idx < htab->size; idx++) {
		if (htab->table[idx].used) {
			TRY( (*visit)(f, &htab->table[idx].entry) );
		}
	}
	return H5_SUCCESS;
}


/* After using the hash table it has to be destroyed. The used memory can
   be freed and the local static variable can be marked as not used.  */
h5_err_t
h5priv_hdestroy (
	h5_file_t* const f,
	struct hsearch_data* htab
	) {
	H5_CORE_API_ENTER;
	h5_err_t ret_value = H5_SUCCESS;
	/* Test for correct arguments.  */
	if (htab == NULL) {
		TRY2( h5_error_internal (f, __FILE__, __func__, __LINE__) );
	}

	TRY2( hwalk (f, htab, htab->free_entry) );

	/* Free used memory.  */
	TRY2( h5_free (f, htab->table) );

	/* the sign for an existing table is an value != NULL in htable */
	htab->table = NULL;
	H5_CORE_API_RETURN (ret_value);
}



/* This is the search function. It uses double hashing with open addressing.
   The argument item.key has to be a pointer to an zero terminated, most
   probably strings of chars. The function for generating a number of the
   strings is simple but fast. It can be replaced by a more complex function
   like ajw (see [Aho,Sethi,Ullman]) if the needs are shown.

   We use an trick to speed up the lookup. The table is created by hcreate
   with one more element available. This enables us to use the index zero
   special. This index will never be used because we store the first hash
   index in the field used where zero means not used. Every other value
   means used. The used field can be used as a first fast comparison for
   equality of the stored and the parameter value. This helps to prevent
   unnecessary expensive calls of strcmp.  */
h5_err_t
h5priv_hsearch (
	h5_file_t* const f,
	void* item,
	const h5_action_t action,
	void** retval,
	struct hsearch_data* htab
	) {
	H5_CORE_API_ENTER;
	h5_err_t ret_value = H5_SUCCESS;
	unsigned int hval;
	unsigned int idx;

	/* Compute an value for the given key. Perhaps use a better method. */
	hval = (*htab->compute_hash)(item);

	/* First hash function: simply take the modul but prevent zero. */
	idx = hval % htab->size + 1;
	
	if (htab->table[idx].used) {
		/* Further action might be required according to the action
		   value. */
		if (htab->table[idx].used == hval
		    && ((*htab->compare) (item, htab->table[idx].entry) == 0) ) {
			if (retval) {
				*retval = htab->table[idx].entry;
			}
			H5_GOTO_DONE (H5_SUCCESS);
		}

		/* Second hash function, as suggested in [Knuth] */
		unsigned int hval2 = 1 + hval % (htab->size - 2);
		unsigned int first_idx = idx;
		
		do {
			/* Because SIZE is prime this guarantees to step
			   through all available indices.  */
			if (idx <= hval2)
				idx = htab->size + idx - hval2;
			else
				idx -= hval2;

			/* If we visited all entries leave the loop
			   unsuccessfully.  */
			if (idx == first_idx)
				break;

			/* If entry is found use it. */
			if (htab->table[idx].used == hval
			    && ((*htab->compare) (
					item, htab->table[idx].entry) == 0) ) {
				if (retval) {
					*retval = htab->table[idx].entry;
				}
				H5_GOTO_DONE (H5_SUCCESS);
			}
		} while (htab->table[idx].used);
	}

	/* An empty bucket has been found. */
	if (action == H5_ENTER) {
		/* If table is full and another entry should be entered return
		   with error.  */
		if (htab->filled == htab->size)	{
			h5_error_internal (f, __FILE__, __func__, __LINE__);
			if (retval) {
				*retval = NULL;
			}
			H5_GOTO_DONE (H5_ERR);
		}

		htab->table[idx].used  = hval;
		htab->table[idx].entry = item;
		
		++htab->filled;

		if (retval) {
			*retval = htab->table[idx].entry;
		}
		H5_GOTO_DONE (H5_SUCCESS);
	} else if (action == H5_REMOVE) {
		htab->table[idx].used = 0;		/* mark as unused, but */
		*retval = htab->table[idx].entry;	/* return ptr to entry */
		H5_GOTO_DONE (H5_SUCCESS);
	}
	if (retval) *retval = NULL;
	h5_debug (f, "Key not found in hash table.");
	ret_value = H5_NOK;
	H5_CORE_API_RETURN (ret_value);
}

typedef struct {
	char* key;
} h5_hitem_string_keyed_t;

static int
cmp_string_keyed (
	const void* __a,
	const void* __b
	) {
	h5_hitem_string_keyed_t *a = (h5_hitem_string_keyed_t*) __a;
	h5_hitem_string_keyed_t *b = (h5_hitem_string_keyed_t*) __b;
	return strcmp (a->key, b->key);
}

static unsigned int
compute_string_keyed (
	const void* __entry
	) {
	h5_hitem_string_keyed_t* entry = (h5_hitem_string_keyed_t*) __entry;
	unsigned int len = strlen (entry->key);
	unsigned int hval = len;
	unsigned int count = len;
	while (count-- > 0)  {
		hval <<= 4;
		hval += entry->key[count];
	}
	return hval;
}

static h5_err_t
free_string_keyed (
	h5_file_t* const f,
	const void* __entry
	) {
	h5_hitem_string_keyed_t* entry = (h5_hitem_string_keyed_t*) __entry;
	TRY( h5_free (f, entry->key) );
	TRY( h5_free (f, entry) );
	return H5_SUCCESS;
}

h5_err_t
h5priv_hcreate_string_keyed (
	h5_file_t* const f,
	size_t nel,
	h5_hashtable_t* htab,
	h5_err_t (*free_entry)(h5_file_t* const f, const void*)
	) {
	H5_CORE_API_ENTER;
	h5_err_t ret_value = H5_SUCCESS;
	if (free_entry == NULL) {
		TRY2( ret_value = h5priv_hcreate (f, nel, htab,
						  cmp_string_keyed,
						  compute_string_keyed,
						  free_string_keyed) );
	} else {
		TRY2( ret_value = h5priv_hcreate (f, nel, htab,
						  cmp_string_keyed,
						  compute_string_keyed,
						  free_entry) );
	}
	H5_CORE_API_RETURN (ret_value);
}

#if 0
static unsigned int
hcompute_loc_id_keyed (
	const void*__item
	) {
	register uint16_t* key = (uint16_t*)__item;
	register int count = sizeof(h5_loc_id_t)/sizeof(uint16_t);
	register unsigned int hval = sizeof (h5_loc_id_t);
	while (count--) {
		if (*key) {
			hval <<= 6;
			hval += *key;
		}
		key++;
	}
	return hval;
}
#endif
