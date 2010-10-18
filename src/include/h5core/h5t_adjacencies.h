#ifndef __H5T_ADJACENCIES_H
#define __H5T_ADJACENCIES_H

h5_err_t
h5t_get_adjacencies (
	h5_file_t * const f,
	const h5_loc_id_t entity_id,
	const h5_int32_t dim,
	h5_idlist_t **list
	);

h5_err_t
h5t_release_list_of_adjacencies (
	h5_file_t * const f,
	h5_idlist_t **list
	);
#endif
