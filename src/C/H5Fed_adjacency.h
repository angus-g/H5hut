#ifndef __H5FED_ADJACENCY_H
#define __H5FED_ADJACENCY_H

h5_err_t
H5FedGetEdgesUpAdjacentToVertex (
	h5_file_t * const f,
	const h5_id_t local_vid,
	h5_idlist_t **list
	);

h5_err_t
H5FedGetTrianglesUpAdjacentToVertex (
	h5_file_t * const f,
	const h5_id_t local_vid,
	h5_idlist_t **list
	);

h5_err_t
H5FedGetTetsUpAdjacentToVertex (
	h5_file_t * const f,
	const h5_id_t local_vid,
	h5_idlist_t **list
	);

h5_err_t
H5FedGetTrianglesUpAdjacentToEdge (
	h5_file_t * const f,
	const h5_id_t local_kid,
	h5_idlist_t **list
	);

h5_err_t
H5FedGetTetsUpAdjacentToEdge (
	h5_file_t * const f,
	const h5_id_t local_kid,
	h5_idlist_t **list
	);

h5_err_t
H5FedGetTetsUpAdjacentToTriangle (
	h5_file_t * const f,
	const h5_id_t local_did,
	h5_idlist_t **list
	);

h5_err_t
H5FedGetVerticesDownAdjacentToEdge (
	h5_file_t * const f,
	const h5_id_t local_kid,
	h5_idlist_t **list
	);

h5_err_t
H5FedGetVerticesDownAdjacentToTriangle (
	h5_file_t * const f,
	const h5_id_t local_did,
	h5_idlist_t **list
	);

h5_err_t
H5FedGetVerticesDownAdjacentToTet (
	h5_file_t * const f,
	const h5_id_t local_tid,
	h5_idlist_t **list
	);

h5_err_t
H5FedGetEdgesDownAdjacentToTriangle (
	h5_file_t * const f,
	const h5_id_t local_did,
	h5_idlist_t **list
	);

h5_err_t
H5FedGetEdgesDownAdjacentToTet (
	h5_file_t * const f,
	const h5_id_t local_tid,
	h5_idlist_t **list
	);

h5_err_t
H5FedGetTrianglesDownAdjacentToTet (
	h5_file_t * const f,
	const h5_id_t local_tid,
	h5_idlist_t **list
	);

h5_err_t
H5FedReleaseListOfAdjacencies (
	h5_file_t * const f,
	h5_idlist_t **list
	);

#endif
