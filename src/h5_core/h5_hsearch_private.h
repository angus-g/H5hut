#ifndef __H5_HSEARCH_PRIVATE_H
#define __H5_HSEARCH_PRIVATE_H

/* Action which shall be performed in the call to hsearch.  */
typedef enum {
	H5_FIND,
	H5_ENTER
} h5_action_t;

typedef struct h5_entry {
	unsigned int len;
	char *key;
	void *data;
} h5_entry_t;

struct hsearch_data {
	struct _ENTRY *table;
	unsigned int size;
	unsigned int filled;
};

/* Reentrant versions which can handle multiple hashing tables at the
   same time.  */
extern int hsearch_r (h5_entry_t __item, h5_action_t __action, h5_entry_t **__retval,
                      struct hsearch_data *__htab);
extern int hcreate_r (size_t __nel, struct hsearch_data *__htab);
extern void hdestroy_r (struct hsearch_data *__htab);


#endif
