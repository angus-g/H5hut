#ifndef __H5FED_BOUNDARIES_H
#define __H5FED_BOUNDARIES_H

h5_err_t
H5FedAddBoundary (
	h5_file * const f
	);


h5_err_t
H5FedOpenBoundary (
	h5_file * const f,
	const h5_id_t boundary_id
	);

h5_err_t
H5FedCloseBoundary (
	h5_file *f
	);

h5_err_t
H5FedAddNumBoundaryfaces (
	h5_file * const f,
	const h5_id_t num_boundaryfaces
	);

h5_err_t
H5FedStoreBoundaryface (
	h5_file *f,
	h5_id_t *global_vids
	);

h5_err_t
H5FedStoreBoundaryfaceGlobalID (
	h5_file *f,
	h5_id_t global_fid
	);

h5_err_t
H5FedStoreBoundaryfaceLocalID (
	h5_file *f,
	h5_id_t local_fid
	);

h5_err_t
H5FedStartTraverseBoundaryfaces (
	h5_file * const f
	);

h5_id_t
H5FedTraverseBoundaryfaces (
	h5_file * const f,
	h5_id_t * const id,
	h5_id_t * const parent_id,
	h5_id_t vertex_ids[3]
	);

#endif
