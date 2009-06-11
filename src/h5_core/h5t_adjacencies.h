#ifndef __H5T_ADJACENCIES_H
#define __H5T_ADJACENCIES_H

h5_err_t
h5t_get_edges_upadjacent_to_vertex (
	h5_file_t * const f,
	const h5_id_t local_vid,
	h5_idlist_t **list
	);

h5_err_t
h5t_get_triangles_upadjacent_to_vertex (
	h5_file_t * const f,
	const h5_id_t local_vid,
	h5_idlist_t **list
	);

h5_err_t
h5t_get_tets_upadjacent_to_vertex (
	h5_file_t * const f,
	const h5_id_t local_vid,
	h5_idlist_t **list
	);

h5_err_t
h5t_release_list_of_adjacencies (
	h5_file_t * const f,
	h5_idlist_t **list
	);
#endif
