#ifndef __MAPS_H
#define __MAPS_H

h5_err_t
_h5_alloc_idmap (
	struct idmap	*map,
	h5_size_t	size
	);

h5_err_t
_h5_insert_idmap (
	struct idmap *map,
	h5_id_t global_id,
	h5_id_t local_id
	);

h5_id_t
_h5_search_idmap (
	struct idmap *map,
	h5_id_t value
	);

h5_id_t
h5t_map_vertex_id_global2local (
	h5_file *f,
	h5_id_t global_id
	);
#endif
