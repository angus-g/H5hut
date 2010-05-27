#ifndef __H5_TYPES_PRIVATE_H
#define __H5_TYPES_PRIVATE_H

struct h5_idmap_el {
	h5_id_t	global_id;
	h5_id_t	local_id;
};
typedef struct h5_idmap_el h5_idmap_el_t;

struct h5_idmap {
	h5_size_t	size;		/* allocated space in number of items */
	h5_size_t	num_items;	/* stored items	*/
	h5_idmap_el_t*  items;
};
#endif
