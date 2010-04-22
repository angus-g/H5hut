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
h5t_get_triangles_upadjacent_to_edge (
	h5_file_t * const f,
	const h5_id_t local_kid,
	h5_idlist_t **list
	);

h5_err_t
h5t_get_tets_upadjacent_to_edge (
	h5_file_t * const f,
	const h5_id_t local_kid,
	h5_idlist_t **list
	);

h5_err_t
h5t_get_tets_upadjacent_to_triangle (
	h5_file_t * const f,
	const h5_id_t local_did,
	h5_idlist_t **list
	);

h5_err_t
h5t_get_vertices_downadjacent_to_edge (
	h5_file_t * const f,
	const h5_id_t local_kid,
	h5_idlist_t **list
	);

h5_err_t
h5t_get_vertices_downadjacent_to_triangle (
	h5_file_t * const f,
	const h5_id_t local_did,
	h5_idlist_t **list
	);

h5_err_t
h5t_get_vertices_downadjacent_to_tet (
	h5_file_t * const f,
	const h5_id_t local_tid,
	h5_idlist_t **list
	);

h5_err_t
h5t_get_edges_downadjacent_to_triangle (
	h5_file_t * const f,
	const h5_id_t local_did,
	h5_idlist_t **list
	);

h5_err_t
h5t_get_edges_downadjacent_to_tet (
	h5_file_t * const f,
	const h5_id_t local_tid,
	h5_idlist_t **list
	);

h5_err_t
h5t_get_triangles_downadjacent_to_tet (
	h5_file_t * const f,
	const h5_id_t local_tid,
	h5_idlist_t **list
	);

h5_err_t
h5t_release_list_of_adjacencies (
	h5_file_t * const f,
	h5_idlist_t **list
	);
#endif
