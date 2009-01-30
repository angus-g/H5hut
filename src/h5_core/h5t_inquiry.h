#ifndef __H5T_INQUIRY_H
#define __H5T_INQUIRY_H

h5_size_t
h5t_get_num_meshes (
	h5_file_t * f,
	const enum h5_oid type
	);

h5_size_t
h5t_get_num_levels (
	h5_file_t * f
	);

h5_size_t
h5t_get_num_elems (
	h5_file_t * f,
	hid_t cnode_id,
	hid_t level_id
	);

h5_size_t
h5t_get_num_elems_total (
	h5_file_t * f,
	hid_t cnode_id,
	hid_t level_id
	);

h5_size_t
h5t_get_num_vertices (
	h5_file_t * f,
	hid_t cnode_id,
	hid_t level_id
	);

h5_id_t
h5t_get_level (
	h5_file_t * f
	);


#endif
