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
	h5t_mesh_t* const m
	) {
	H5_PRIV_FUNC_ENTER (h5_err_t, "m=%p", m);
	assert (m->num_leaf_levels > 0);

	m->dsinfo_vertices.dims[0] = m->num_vertices[m->num_leaf_levels-1];
	TRY( h5priv_write_dataset_by_name (
		     m->f,
		     m->mesh_gid,
		     &m->dsinfo_vertices,
		     open_space_all,
		     open_space_all,
		     m->vertices) );

	TRY (h5priv_write_attrib (
		     m->mesh_gid,
		     "__num_vertices__",
		     H5T_NATIVE_INT64,
		     m->num_vertices,
		     m->num_leaf_levels));

	H5_PRIV_FUNC_RETURN (H5_SUCCESS);
}

static h5_err_t
write_elems (
	h5t_mesh_t* const m
	) {
	H5_PRIV_FUNC_ENTER (h5_err_t, "m=%p", m);
	assert (m->num_leaf_levels > 0);

	h5_loc_idx_t num_elems = m->num_elems[m->num_leaf_levels-1];
	// alloc and inititalize data in memory
	TRY (h5tpriv_alloc_glb_elems_struct (m, num_elems));
	TRY (h5tpriv_init_glb_elems_struct (m));

	m->dsinfo_elems.dims[0] = num_elems;
	TRY( h5priv_write_dataset_by_name (
		     m->f,
		     m->mesh_gid,
		     &m->dsinfo_elems,
		     open_space_all,
		     open_space_all,
		     m->glb_elems.data) );

	TRY (h5priv_write_attrib (
		     m->mesh_gid,
		     "__num_elems__",
		     H5T_NATIVE_INT64,
		     m->num_elems,
		     m->num_leaf_levels));

	TRY (h5priv_write_attrib (
		     m->mesh_gid,
		     "__num_leaf_elems__",
		     H5T_NATIVE_INT64,
		     m->num_leaf_elems,
		     m->num_leaf_levels));

	TRY (h5priv_write_attrib (
		     m->mesh_gid,
		     "__num_leaf_levels__",
		     H5T_NATIVE_INT16,
		     &m->num_leaf_levels,
		     1));

	// release mem
	TRY( h5_free (m->glb_elems.data) );
	m->glb_elems.tets = NULL;

	H5_PRIV_FUNC_RETURN (H5_SUCCESS);
}

h5_err_t
h5tpriv_write_mesh (
	h5t_mesh_t* const m
	) {
	H5_PRIV_API_ENTER (h5_err_t, "m=%p", m);
	if (m->mesh_changed) {
		TRY (write_vertices (m));
		TRY (write_elems (m));
	}
	H5_PRIV_API_RETURN (H5_SUCCESS);
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
	h5t_mesh_t* const m
	) {
	H5_PRIV_FUNC_ENTER (h5_err_t, "m=%p", m);

	ssize_t num_bytes = m->num_leaf_levels*sizeof (m->num_vertices[0]);
	TRY (m->num_vertices = h5_alloc (m->num_vertices, num_bytes));
	TRY (h5priv_read_attrib (
		     m->mesh_gid,
		     "__num_vertices__",
		     H5T_NATIVE_INT64,
		     m->num_vertices));

	H5_PRIV_FUNC_RETURN (H5_SUCCESS);
}

static h5_err_t
read_vertices (
	h5t_mesh_t* m
	) {
	H5_PRIV_FUNC_ENTER (h5_err_t, "m=%p", m);

	TRY( h5tpriv_alloc_num_vertices (m, m->num_vertices[m->num_leaf_levels-1]) );
	TRY( h5priv_read_dataset_by_name (
		     m->f,
		     m->mesh_gid,
		     &m->dsinfo_vertices,
		     open_mem_space_vertices,
		     open_file_space_vertices,
		     m->vertices) );

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
	h5t_mesh_t* const m
	) {
	H5_PRIV_FUNC_ENTER (h5_err_t, "m=%p", m);

	TRY (m->num_elems = h5_calloc (m->num_leaf_levels, sizeof(m->num_elems[0])));
	TRY( m->num_leaf_elems = h5_calloc (m->num_leaf_levels, sizeof(m->num_elems[0])));

	TRY (h5priv_read_attrib (
		     m->mesh_gid,
		     "__num_elems__",
		     H5T_NATIVE_INT64,
		     m->num_elems));

	TRY (h5priv_read_attrib (
		     m->mesh_gid,
		     "__num_leaf_elems__",
		     H5T_NATIVE_INT64,
		     m->num_leaf_elems));

	h5_loc_idx_t num_elems = m->num_elems[m->num_leaf_levels-1];
	TRY( h5tpriv_alloc_elems (m, 0, num_elems) );
	TRY( h5tpriv_alloc_glb_elems_struct (m, num_elems) );
	TRY( h5priv_read_dataset_by_name (
		     m->f,
		     m->mesh_gid,
		     &m->dsinfo_elems,
		     open_mem_space_elems,
		     open_file_space_elems,
		     m->glb_elems.data) );

	H5_PRIV_FUNC_RETURN (H5_SUCCESS);
}

h5_err_t
h5tpriv_read_mesh (
	h5t_mesh_t* const m
	) {
	H5_PRIV_API_ENTER (h5_err_t, "m=%p", m);

	TRY (h5priv_read_attrib (
		     m->mesh_gid,
		     "__num_leaf_levels__",
		     H5T_NATIVE_INT16,
		     &m->num_leaf_levels));

	TRY (read_num_vertices (m));

	TRY (read_vertices (m));
	TRY (h5tpriv_rebuild_vertex_indices_mapping (m));

	TRY (read_elems (m));

	TRY (h5tpriv_init_glb2loc_elem_map (m));
	TRY (h5tpriv_init_loc_elems_struct (m, 0));
	TRY (h5_free (m->glb_elems.data));
	TRY (h5tpriv_update_adjacency_structs (m, 0));
	TRY (h5tpriv_init_geom_boundary_info (m, 0));

	m->num_loaded_levels = m->num_leaf_levels;
	H5_PRIV_API_RETURN (H5_SUCCESS);
}
