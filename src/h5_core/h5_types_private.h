#ifndef __H5_TYPES_PRIVATE_H
#define __H5_TYPES_PRIVATE_H

struct smap {
	h5_size_t	size;		/* allocated space in number of items */
	h5_size_t	num_items;	/* stored items	*/
	h5_id_t		*items;
};

struct idmap {
	h5_size_t	size;		/* allocated space in number of items */
	h5_size_t	num_items;	/* stored items	*/
	struct {
		h5_id_t	global_id;
		h5_id_t	local_id;
	}		*items;
};


#endif
