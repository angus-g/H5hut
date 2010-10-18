#ifndef __H5FED_ADJACENCY_H
#define __H5FED_ADJACENCY_H

h5_err_t
H5FedGetAdjacencies (
	h5_file_t* const f,
	const h5_loc_id_t entity_id,
	const h5_int32_t dim,
	h5_idlist_t** list
	);

h5_err_t
H5FedReleaseListOfAdjacencies (
	h5_file_t * const f,
	h5_idlist_t **list
	);

#endif
