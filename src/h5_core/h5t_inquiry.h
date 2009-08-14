#ifndef __H5T_INQUIRY_H
#define __H5T_INQUIRY_H

h5_size_t
h5t_get_num_meshes (
	h5_file_t * const f,
	const enum h5_oid type
	);

h5_size_t
h5t_get_num_levels (
	h5_file_t * const f
	);

h5_size_t
h5t_get_num_elems (
	h5_file_t * const f,
	const h5_id_t cnode_id
	);

h5_size_t
h5t_get_num_vertices (
	h5_file_t * const f,
	const h5_id_t cnode_id
	);

h5_id_t
h5t_get_level (
	h5_file_t * const f
	);
#endif
