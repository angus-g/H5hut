#include "h5core/h5_core.h"
#include "h5_core_private.h"

static hid_t
open_space_all (
	h5_file_t* const f,
	const hid_t dataset_id
	) {
	UNUSED_ARGUMENT (f);
	UNUSED_ARGUMENT (dataset_id);
	return H5S_ALL;
}

/*
  Write vertices:
  * either we write a new dataset
  * or we append data to this dataset
  * appending means, a new level has been added
  * existing vertices will never be changed!

 */
static h5_err_t
write_vertices (
	h5_file_t* const f
	) {
	H5_PRIV_FUNC_ENTER (h5_err_t);
	h5t_fdata_t *t = f->t;
	assert (t->num_leaf_levels > 0);
	
	t->dsinfo_vertices.dims[0] = t->num_vertices[t->num_leaf_levels-1];
	TRY( h5priv_write_dataset_by_name (
		     f,
		     t->mesh_gid,
		     &t->dsinfo_vertices,
		     open_space_all,
		     open_space_all,
		     t->vertices) );
	TRY( h5priv_write_dataset_by_name (
		     f,
		     t->mesh_gid,
		     &t->dsinfo_num_vertices,
		     open_space_all,
		     open_space_all,
		     t->num_vertices) );

	H5_PRIV_FUNC_RETURN (H5_SUCCESS);
}

static h5_err_t
write_elems (
	h5_file_t* const f
	) {
	H5_PRIV_FUNC_ENTER (h5_err_t);
	h5t_fdata_t* t = f->t;
	assert (t->num_leaf_levels > 0);

	h5_loc_idx_t num_elems = t->num_elems[t->num_leaf_levels-1];
	// alloc and inititalize data in memory
	TRY( h5tpriv_alloc_glb_elems_struct (f, num_elems) );
	TRY( h5tpriv_init_glb_elems_struct (f) );

	t->dsinfo_elems.dims[0] = num_elems;
	TRY( h5priv_write_dataset_by_name (
		     f,
		     t->mesh_gid,
		     &t->dsinfo_elems,
		     open_space_all,
		     open_space_all,
		     t->glb_elems.data) );

	TRY( h5priv_write_dataset_by_name (
		     f,
		     t->mesh_gid,
		     &t->dsinfo_num_elems,
		     open_space_all,
		     open_space_all,
		     t->num_elems) );

	TRY( h5priv_write_dataset_by_name (
		     f,
		     t->mesh_gid,
		     &t->dsinfo_num_elems_on_leaf_level,
		     open_space_all,
		     open_space_all,
		     t->num_elems_on_leaf_level) );
	// release mem
	TRY( h5_free (t->glb_elems.data) );
	t->glb_elems.tets = NULL;

	H5_PRIV_FUNC_RETURN (H5_SUCCESS);
}

h5_err_t
h5tpriv_write_mesh (
	h5_file_t* const f
	) {
	H5_PRIV_API_ENTER (h5_err_t);
	h5t_fdata_t* t = f->t;
	if (t->mesh_changed) {
		TRY (write_vertices (f));
		TRY (write_elems (f));
	}
	if (t->mtags.changed) { 
		TRY (h5tpriv_write_mtags (f));
	}

	H5_PRIV_API_RETURN (H5_SUCCESS);
}

static h5_ssize_t
read_num_leaf_levels (
	h5_file_t* const f
	) {
	h5t_fdata_t* t = f->t;
	hid_t dataset_id;
	hid_t diskspace_id;
	hssize_t size;

	H5_PRIV_FUNC_ENTER (h5_ssize_t);
	if (t->cur_mesh < 0) {
		return h5tpriv_error_undef_mesh ();
	}
	TRY (dataset_id = hdf5_open_dataset (t->mesh_gid, "NumVertices") );
	TRY (diskspace_id = hdf5_get_dataset_space (dataset_id) );
	TRY (size = hdf5_get_npoints_of_dataspace (diskspace_id) );
	TRY (hdf5_close_dataspace (diskspace_id) );

	t->num_leaf_levels = size;
	H5_PRIV_FUNC_RETURN (size);
}


static hid_t
open_mem_space_vertices (
	h5_file_t* const f,
	hid_t dataset_id
	) {
	UNUSED_ARGUMENT (f);
	UNUSED_ARGUMENT (dataset_id);
	return H5S_ALL;
}

static hid_t
open_file_space_vertices (
	h5_file_t* const f,
	hid_t dataset_id
	) {
	UNUSED_ARGUMENT (f);
	UNUSED_ARGUMENT (dataset_id);
	return H5S_ALL;
}

static inline h5_err_t
read_num_vertices (
	h5_file_t* const f
	) {
	H5_PRIV_FUNC_ENTER (h5_err_t);
	h5t_fdata_t* t = f->t;

 	if (t->mesh_gid < 0) {
		H5_PRIV_FUNC_LEAVE (
			h5_error_internal (__FILE__, __func__, __LINE__));
	}
	ssize_t num_bytes = t->num_leaf_levels*sizeof (t->num_vertices[0]);
	TRY( t->num_vertices = h5_alloc (t->num_vertices, num_bytes) );
	TRY( h5priv_read_dataset_by_name (
		     f,
		     t->mesh_gid,
		     &t->dsinfo_num_vertices,
		     open_space_all,
		     open_space_all,
		     t->num_vertices) );

	H5_PRIV_FUNC_RETURN (H5_SUCCESS);
}

static h5_err_t
read_vertices (
	h5_file_t * f
	) {
	H5_PRIV_FUNC_ENTER (h5_err_t);
	h5t_fdata_t* t = f->t;

	TRY( h5tpriv_alloc_num_vertices (f, t->num_vertices[t->num_leaf_levels-1]) );
	TRY( h5priv_read_dataset_by_name (
		     f,
		     t->mesh_gid,
		     &t->dsinfo_vertices,
		     open_mem_space_vertices,
		     open_file_space_vertices,
		     t->vertices) );

	H5_PRIV_FUNC_RETURN (H5_SUCCESS);
}

static hid_t
open_mem_space_elems (
	h5_file_t* const f,
	hid_t dataset_id
	) {
	UNUSED_ARGUMENT (f);
	UNUSED_ARGUMENT (dataset_id);
	return H5S_ALL;
}

static hid_t
open_file_space_elems (
	h5_file_t* const f,
	hid_t dataset_id
	) {
	UNUSED_ARGUMENT (f);
	UNUSED_ARGUMENT (dataset_id);
	return H5S_ALL;
}

static h5_err_t
read_elems (
	h5_file_t* const f
	) {
	H5_PRIV_FUNC_ENTER (h5_err_t);
	h5t_fdata_t* t = f->t;

	size_t size = t->num_leaf_levels * sizeof (t->num_elems[0]);
	TRY( t->num_elems = h5_calloc (1, size) );
	TRY( t->num_elems_on_leaf_level = h5_calloc (1, size) );
	TRY( h5priv_read_dataset_by_name (
		     f,
		     t->mesh_gid,
		     &t->dsinfo_num_elems,
		     open_space_all,
		     open_space_all,
		     t->num_elems) );

	TRY( h5priv_read_dataset_by_name (
		     f,
		     t->mesh_gid,
		     &t->dsinfo_num_elems_on_leaf_level,
		     open_space_all,
		     open_space_all,
		     t->num_elems_on_leaf_level) );

	h5_loc_idx_t num_elems = t->num_elems[t->num_leaf_levels-1];
	TRY( h5tpriv_alloc_elems (f, 0, num_elems) );
	TRY( h5tpriv_alloc_glb_elems_struct (f, num_elems) );
	TRY( h5priv_read_dataset_by_name (
		     f,
		     t->mesh_gid,
		     &t->dsinfo_elems,
		     open_mem_space_elems,
		     open_file_space_elems,
		     t->glb_elems.data) );

	H5_PRIV_FUNC_RETURN (H5_SUCCESS);
}

static inline h5_err_t
read_mtags (
	h5_file_t* const f
	) {
	H5_PRIV_FUNC_ENTER (h5_err_t);
	h5t_fdata_t* t = f->t;
	h5_err_t exists;
	TRY (exists = hdf5_link_exists (t->mesh_gid, "Tags"));
	if (exists) {
		TRY (t->mtags.group_id = h5priv_open_group (f, t->mesh_gid, "Tags") );
		TRY (h5tpriv_read_tag_container (f, &f->t->mtags) );
	}
	H5_PRIV_FUNC_RETURN (H5_SUCCESS);
}

h5_err_t
h5tpriv_read_mesh (
	h5_file_t* const f
	) {
	H5_PRIV_API_ENTER (h5_err_t);
	h5t_fdata_t* t = f->t;
 	if (t->mesh_gid < 0) {
		H5_PRIV_API_LEAVE (h5_error_internal (__FILE__, __func__, __LINE__));
	}
	TRY (read_num_leaf_levels (f));
	TRY (read_num_vertices (f));

	TRY (read_vertices (f));
	TRY (h5tpriv_rebuild_vertex_indices_mapping (f));

	TRY (read_elems (f));

	TRY (h5tpriv_init_glb2loc_elem_map (f));
	TRY (h5tpriv_init_loc_elems_struct (f, 0));
	TRY (h5_free (t->glb_elems.data));
	TRY (h5tpriv_update_adjacency_structs (f, 0));
	TRY (h5tpriv_init_geom_boundary_info (f, 0));

	TRY (read_mtags (f));
	t->num_loaded_levels = t->num_leaf_levels;
	H5_PRIV_API_RETURN (H5_SUCCESS);
}
