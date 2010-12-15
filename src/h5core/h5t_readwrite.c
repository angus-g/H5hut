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
	h5t_fdata_t *t = f->t;

	if (t->num_vertices <= 0) return H5_SUCCESS;  /* ???? */

	t->dsinfo_vertices.dims[0] = t->num_vertices[t->cur_level];
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

	return H5_SUCCESS;
}

static h5_err_t
write_elems (
	h5_file_t* const f
	) {
	h5t_fdata_t* t = f->t;
	
	if (t->num_elems <= 0) return H5_SUCCESS;

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
		     &t->dsinfo_num_elems_on_level,
		     open_space_all,
		     open_space_all,
		     t->num_elems_on_level) );

	return H5_SUCCESS;
}

h5_err_t
h5tpriv_write_mesh (
	h5_file_t* const f
	) {
	h5_debug (f, "%s ()", __func__);
	h5t_fdata_t* t = f->t;
	if (t->mesh_changed) {
		TRY( write_vertices (f) );
		TRY( write_elems (f) );
	}
	if (t->mtags.changed) { 
		TRY( h5tpriv_write_mtags (f) );
	}

	return H5_SUCCESS;
}

static h5_size_t
read_num_levels (
	h5_file_t* const f
	) {
	h5t_fdata_t* t = f->t;
	hid_t dataset_id;
	hid_t diskspace_id;
	hssize_t size;

	if (t->cur_mesh < 0) {
		return h5tpriv_error_undef_mesh (f);
	}
	TRY( dataset_id = h5priv_open_hdf5_dataset (f, t->mesh_gid, "NumVertices") );
	TRY( diskspace_id = h5priv_get_hdf5_dataset_space (f, dataset_id) );
	TRY( size = h5priv_get_npoints_of_hdf5_dataspace (f, diskspace_id) );
	TRY( h5priv_close_hdf5_dataspace (f, diskspace_id) );

	t->num_levels = size;
	return size;
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

static h5_err_t
read_num_vertices (
	h5_file_t* const f
	) {
	h5t_fdata_t* t = f->t;

 	if (t->mesh_gid < 0) {
		return h5_error_internal (f, __FILE__, __func__, __LINE__);
	}
	ssize_t num_bytes = t->num_levels*sizeof (t->num_vertices[0]);
	TRY( t->num_vertices = h5_alloc (f, t->num_vertices, num_bytes) );
	TRY( h5priv_read_dataset_by_name (
		     f,
		     t->mesh_gid,
		     &t->dsinfo_num_vertices,
		     open_space_all,
		     open_space_all,
		     t->num_vertices) );

	return H5_SUCCESS;
}

static h5_err_t
read_vertices (
	h5_file_t * f
	) {
	h5t_fdata_t* t = f->t;

	TRY( h5tpriv_alloc_num_vertices (f, t->num_vertices[t->num_levels-1]) );
	TRY( h5priv_read_dataset_by_name (
		     f,
		     t->mesh_gid,
		     &t->dsinfo_vertices,
		     open_mem_space_vertices,
		     open_file_space_vertices,
		     t->vertices) );

	return H5_SUCCESS;
}



static h5_err_t
read_num_elems (
	h5_file_t* const f
	) {
	h5t_fdata_t* t = f->t;

 	if (t->mesh_gid < 0) {
		return h5_error_internal (f, __FILE__, __func__, __LINE__);
	}
	size_t size = t->num_levels * sizeof (t->num_elems[0]);
	TRY( t->num_elems = h5_alloc (f, NULL, size) );
	TRY( t->num_elems_on_level = h5_alloc (f, NULL, size) );
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
		     &t->dsinfo_num_elems_on_level,
		     open_space_all,
		     open_space_all,
		     t->num_elems_on_level) );
	
	return H5_SUCCESS;
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
	h5t_fdata_t* t = f->t;

	TRY( h5tpriv_alloc_elems(f, 0, t->num_elems[t->num_levels-1]) );
	TRY( h5priv_read_dataset_by_name (
		     f,
		     t->mesh_gid,
		     &t->dsinfo_elems,
		     open_mem_space_elems,
		     open_file_space_elems,
		     t->glb_elems.data) );
	return H5_SUCCESS;
}

static h5_err_t
read_mtags (
	h5_file_t* const f
	) {
	h5t_fdata_t* t = f->t;
	h5_err_t exists;
	TRY( exists = h5priv_hdf5_link_exists (f, t->mesh_gid, "Tags") );
	if (exists) {
		TRY( t->mtags.group_id = h5priv_open_group (f, t->mesh_gid, "Tags") );
		TRY( h5tpriv_read_tag_container (f, &f->t->mtags) );
	}
	return H5_SUCCESS;
}

h5_err_t
h5tpriv_read_mesh (
	h5_file_t* const f
	) {
	h5t_fdata_t* t = f->t;
 	if (t->mesh_gid < 0) {
		return h5_error_internal (f, __FILE__, __func__, __LINE__);
	}
	TRY( read_num_levels (f) );
	TRY( read_num_vertices (f) );
	TRY( read_num_elems (f) );

	TRY( read_vertices (f) );
	TRY( h5tpriv_rebuild_vertex_indices_mapping (f) );

	TRY( read_elems (f) );
	TRY( h5tpriv_rebuild_elem_indices_mapping (f) );
	TRY( h5tpriv_init_loc_elems_struct (f, 0) );
	TRY( h5tpriv_update_adjacency_structs (f, 0) );
	TRY( h5tpriv_init_geom_boundary_info (f, 0) );

	TRY( read_mtags (f) );
	t->num_loaded_levels = t->num_levels;
	return H5_SUCCESS;
}
