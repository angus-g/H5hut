#include <string.h>

#include "h5core/h5_core.h"
#include "h5_core_private.h"

static struct h5t_methods tet_funcs = {
	&h5tpriv_tetm_store_methods,
	&h5tpriv_tetm_retrieve_methods,
	&h5tpriv_tetm_adjacency_methods
};

static struct h5t_methods tri_funcs = {
	&h5tpriv_trim_store_methods,
	&h5tpriv_trim_retrieve_methods,
	&h5tpriv_trim_adjacency_methods
};

/*
  create several HDF5 types
*/
static h5_err_t
create_array_types (
	h5_file_t* const f
	) {
	h5t_fdata_t* t = f->t;
	h5_dtypes_t* dtypes = &(t->dtypes);

	hsize_t dims[1] = { 3 };
	TRY(
		dtypes->h5_coord3d_t = h5priv_create_hdf5_array_type (
			f,
			H5_FLOAT64_T,
			1,
			dims)
		);
	TRY( 
		dtypes->h5_3id_t = h5priv_create_hdf5_array_type (
			f,
			H5_ID_T,
			1,
			dims)
		);
	dims[0] = 4;
	TRY(
		dtypes->h5_4id_t = h5priv_create_hdf5_array_type (
			f,
			H5_ID_T,
			1,
			dims)
		);

	return H5_SUCCESS;
}

static h5_err_t
create_vertex_type (
	h5_file_t* const f
	) {
	h5_dtypes_t* dtypes = &f->t->dtypes;

	TRY(
		dtypes->h5_vertex_t = h5priv_create_hdf5_type (
			f,
			H5_COMPOUND_T,
			sizeof (struct h5_vertex)) );
	TRY(
		h5priv_insert_hdf5_type (
			f,
			dtypes->h5_vertex_t,
			"global_idx",
			HOFFSET (struct h5_vertex, global_idx),
			H5_ID_T) );
	TRY(
		h5priv_insert_hdf5_type (
			f,
			dtypes->h5_vertex_t,
			"P",
			HOFFSET (struct h5_vertex, P),
			dtypes->h5_coord3d_t) );

	return H5_SUCCESS;
}

static h5_err_t
create_triangle_type (
	h5_file_t* const f
	) {
	h5_dtypes_t* dtypes = &f->t->dtypes;

	TRY(
		dtypes->h5_triangle_t = h5priv_create_hdf5_type (
			f,
			H5_COMPOUND_T,
			sizeof (struct h5_triangle)) );
	TRY(
		h5priv_insert_hdf5_type (
			f,
			dtypes->h5_triangle_t,
			"global_idx",
			HOFFSET (struct h5_triangle, global_idx),
			H5_ID_T) );
	TRY(
		h5priv_insert_hdf5_type (
			f,
			dtypes->h5_triangle_t,
			"global_parent_idx",
			HOFFSET (struct h5_triangle, global_parent_idx),
			H5_ID_T) );
	TRY(
		h5priv_insert_hdf5_type (
			f,
			dtypes->h5_triangle_t,
			"global_child_idx",
			HOFFSET(struct h5_triangle, global_child_idx),
			H5_ID_T) );
	TRY(
		h5priv_insert_hdf5_type (
			f,
			dtypes->h5_triangle_t,
			"global_vertex_indices",
			HOFFSET (struct h5_triangle, global_vertex_indices),
			dtypes->h5_3id_t) );

	return H5_SUCCESS;
}

static h5_err_t
create_tag_types (
	h5_file_t* const f
	) {
	h5_dtypes_t* dtypes = &f->t->dtypes;

	TRY (
		dtypes->h5t_tag_idx_t = h5priv_create_hdf5_type (
			f,
			H5_COMPOUND_T,
			sizeof (h5t_tag_idx_t)) );
	TRY (
		h5priv_insert_hdf5_type (
			f,
			dtypes->h5t_tag_idx_t,
			"eid",
			HOFFSET (h5t_tag_idx_t, eid),
			H5_ID_T) );
	TRY (
		h5priv_insert_hdf5_type (
			f,
			dtypes->h5t_tag_idx_t,
			"idx",
			HOFFSET (h5t_tag_idx_t, idx),
			H5_ID_T) );

	return H5_SUCCESS;
}

static h5_err_t
create_tet_type (
	h5_file_t* const f
	) {
	h5_dtypes_t* dtypes = &f->t->dtypes;

	TRY(
		dtypes->h5_tet_t = h5priv_create_hdf5_type (
			f,
			H5_COMPOUND_T,
			sizeof (struct h5_tetrahedron)) );
	TRY(
		h5priv_insert_hdf5_type (
			f,
			dtypes->h5_tet_t,
			"global_idx",
			HOFFSET (struct h5_tetrahedron, global_idx),
			H5_ID_T) );
	TRY(
		h5priv_insert_hdf5_type (
			f,
			dtypes->h5_tet_t,
			"global_parent_idx",
			HOFFSET (struct h5_tetrahedron, global_parent_idx),
			H5_ID_T) );
	TRY(
		h5priv_insert_hdf5_type (
			f,
			dtypes->h5_tet_t,
			"global_child_idx",
			HOFFSET (struct h5_tetrahedron, global_child_idx),
			H5T_NATIVE_INT32) );
	TRY(
		h5priv_insert_hdf5_type (
			f,
			dtypes->h5_tet_t,
			"global_vertex_indices",
			HOFFSET (struct h5_tetrahedron, global_vertex_indices),
			dtypes->h5_4id_t) );

	return H5_SUCCESS;
}


#if 0
h5_err_t
h5priv_set_dataset_properties (
	h5_dsinfo_t* dsinfo,
	const char* name,
	const hid_t type,
	const int rank,
	const hsize_t* dims,
	const hsize_t* maxdims,
	const hsize_t chunk_dims
	) {
	return H5_SUCCESS;
}
#endif

static h5_err_t
init_fdata (
	h5_file_t* const f
	) {
	h5t_fdata_t* t = f->t;

	memset (t->mesh_name, 0, sizeof (t->mesh_name));
	t->num_meshes = -1;
	t->cur_mesh = -1;
	t->num_levels = -1;
	t->cur_level = -1;
	t->last_stored_vid = -1;
	t->last_stored_eid = -1;
	t->topo_gid = -1;
	t->meshes_gid = -1;
	t->mesh_gid = -1;

	/* vertices */
	strcpy (t->dsinfo_vertices.name, "Vertices");
	t->dsinfo_vertices.rank = 1;
	t->dsinfo_vertices.dims[0] = 0;
	t->dsinfo_vertices.max_dims[0] = H5S_UNLIMITED;
	t->dsinfo_vertices.chunk_dims[0] = 4096;
	t->dsinfo_vertices.type_id = t->dtypes.h5_vertex_t;
	TRY( t->dsinfo_vertices.create_prop = h5priv_create_hdf5_property (
		     f,
		     H5P_DATASET_CREATE) );
	TRY( h5priv_set_hdf5_chunk_property (
		     f,
		     t->dsinfo_vertices.create_prop,
		     t->dsinfo_vertices.rank,
		     t->dsinfo_vertices.chunk_dims) );
	t->dsinfo_vertices.access_prop = H5P_DEFAULT;

	/* NumVertices */
	strcpy( t->dsinfo_num_vertices.name, "NumVertices" );
	t->dsinfo_num_vertices.rank = 1;
	t->dsinfo_num_vertices.dims[0] = 0;
	t->dsinfo_num_vertices.max_dims[0] = H5S_UNLIMITED;
	t->dsinfo_num_vertices.chunk_dims[0] = 4096;
	t->dsinfo_num_vertices.type_id = t->dtypes.h5_id_t;
	TRY( t->dsinfo_num_vertices.create_prop = h5priv_create_hdf5_property (
		     f,
		     H5P_DATASET_CREATE) );
	TRY( h5priv_set_hdf5_chunk_property (
		     f,
		     t->dsinfo_num_vertices.create_prop,
		     t->dsinfo_num_vertices.rank,
		     t->dsinfo_num_vertices.chunk_dims) );
	t->dsinfo_num_vertices.access_prop = H5P_DEFAULT;

	/* Elems */
	strcpy (t->dsinfo_elems.name, "Elems");
	t->dsinfo_elems.rank = 1;
	t->dsinfo_elems.dims[0] = 0;
	t->dsinfo_elems.max_dims[0] = H5S_UNLIMITED;
	t->dsinfo_elems.chunk_dims[0] = 4096;
	TRY( t->dsinfo_elems.create_prop = h5priv_create_hdf5_property (
		     f,
		     H5P_DATASET_CREATE) );
	TRY( h5priv_set_hdf5_chunk_property (
		     f,
		     t->dsinfo_elems.create_prop,
		     t->dsinfo_elems.rank,
		     t->dsinfo_elems.chunk_dims) );
	t->dsinfo_elems.access_prop = H5P_DEFAULT;

	/* NumElems */
	strcpy (t->dsinfo_num_elems.name, "NumElems");
	t->dsinfo_num_elems.rank = 1;
	t->dsinfo_num_elems.dims[0] = 0;
	t->dsinfo_num_elems.max_dims[0] = H5S_UNLIMITED;
	t->dsinfo_num_elems.chunk_dims[0] = 4096;
	t->dsinfo_num_elems.type_id = t->dtypes.h5_id_t;
	TRY( t->dsinfo_num_elems.create_prop = h5priv_create_hdf5_property (
		     f,
		     H5P_DATASET_CREATE) );
	TRY( h5priv_set_hdf5_chunk_property (
		     f,
		     t->dsinfo_num_elems.create_prop,
		     t->dsinfo_num_elems.rank,
		     t->dsinfo_num_elems.chunk_dims) );
	t->dsinfo_num_elems.access_prop = H5P_DEFAULT;

	/* NumElemsOnLevel */
	strcpy (t->dsinfo_num_elems_on_level.name, "NumElemsOnLevel");
	t->dsinfo_num_elems_on_level.rank = 1;
	t->dsinfo_num_elems_on_level.dims[0] = 0;
	t->dsinfo_num_elems_on_level.max_dims[0] = H5S_UNLIMITED;
	t->dsinfo_num_elems_on_level.chunk_dims[0] = 4096;
	t->dsinfo_num_elems_on_level.type_id = t->dtypes.h5_id_t;
	TRY( t->dsinfo_num_elems_on_level.create_prop = h5priv_create_hdf5_property (
		     f,
		     H5P_DATASET_CREATE) );
	TRY( h5priv_set_hdf5_chunk_property (
		     f,
		     t->dsinfo_num_elems_on_level.create_prop,
		     t->dsinfo_num_elems_on_level.rank,
		     t->dsinfo_num_elems_on_level.chunk_dims) );
	t->dsinfo_num_elems_on_level.access_prop = H5P_DEFAULT;

	return H5_SUCCESS;
}

/*!
  \ingroup h5_private

  \internal

  Initialize topo internal structure. The structure has already be initialized
  with zero's.

  \return	H5_SUCCESS or error code
*/
h5_err_t
h5tpriv_open_file (
	h5_file_t* const f			/*!< IN: file handle */
	) {

	TRY( (f->t = h5priv_alloc (f, NULL, sizeof (*f->t))) );
	h5t_fdata_t* t = f->t;

	t->dtypes.h5_id_t = H5_INT64_T;
	t->dtypes.h5_int64_t = H5_INT64_T;
	t->dtypes.h5_float64_t = H5_FLOAT64_T;

	TRY( create_array_types (f) );
	TRY( create_vertex_type (f) );
	TRY( create_triangle_type (f) );
	TRY( create_tet_type (f) );
	TRY( create_tag_types (f) );
	TRY( init_fdata (f) );

	return H5_SUCCESS;
}

/*!
  \ingroup h5_private

  \internal

  De-initialize topological internal structure.  Open HDF5 objects are 
  closed and allocated memory freed.

  \return	H5_SUCCESS or error code
*/
h5_err_t
h5tpriv_close_file (
	h5_file_t* const f		/*!< IN: file handle */
	) {
	TRY( h5t_close_mesh (f) );

	return H5_SUCCESS;
}

h5_err_t
h5tpriv_init_step (
	h5_file_t* const f
	) {
	return H5_SUCCESS;
}

/*
 - write data
 - close HDF5 objects we cannot reuse
 - free memory
*/
h5_err_t
h5tpriv_close_step (
	h5_file_t* const f
	) {

	return H5_SUCCESS;
}


h5_err_t
h5tpriv_open_topo_group (
	h5_file_t * const f
	) {
	h5t_fdata_t* t = f->t;

	t->topo_gid = h5priv_open_group (f, f->root_gid, H5T_CONTAINER_GRPNAME);
	return t->topo_gid;
}

h5_err_t
h5tpriv_open_meshes_group (
	h5_file_t* const f
	) {
	h5t_fdata_t* t = f->t;

	if (t->topo_gid < 0) {
		TRY( h5tpriv_open_topo_group (f) );
	}
	TRY( (t->meshes_gid = h5priv_open_group (
		      f,
		      t->topo_gid,
		      h5tpriv_meshes_grpnames[t->mesh_type])) );

	return H5_SUCCESS;
}

h5_err_t
h5tpriv_open_mesh_group (
	h5_file_t* const f
	) {
	h5t_fdata_t* t = f->t;

	if (t->meshes_gid < 0) {
		TRY( h5tpriv_open_meshes_group (f) );
	}
	TRY( (t->mesh_gid = h5priv_open_group (
		      f,
		      t->meshes_gid,
		      t->mesh_name)) );
	return H5_SUCCESS;
}

/*
  If the value of parameter \c id is \c -1, a new mesh will be appended.
*/
h5_err_t
h5t_open_mesh (
	h5_file_t* const f,
	h5_id_t id,
	const h5_oid_t type
	) {
	h5t_fdata_t* t = f->t;

	TRY( h5t_close_mesh (f) );

	if (t->num_meshes < 0) {
		h5_size_t result = h5t_get_num_meshes (f, type);
		t->num_meshes = (result > 0 ? result : 0);
	}
	if ((id < -1) || (id >= t->num_meshes)) {
		return HANDLE_H5_OUT_OF_RANGE_ERR (f, "mesh", id);
	}
	if (id == -1) {  /* append new mesh */
		id = t->num_meshes;
	}
	t->mesh_type = type;
	snprintf (t->mesh_name, sizeof (t->mesh_name), "%lld", (long long)id);

	switch (type) {
	case H5_OID_TETRAHEDRON:
		t->dsinfo_elems.type_id = t->dtypes.h5_tet_t;
		t->methods = tet_funcs;
		t->ref_element = &h5t_tet_ref_element;
		break;
	case H5_OID_TRIANGLE:
		t->dsinfo_elems.type_id = t->dtypes.h5_triangle_t;
		t->methods = tri_funcs;
		t->ref_element = &h5t_tri_ref_element;
		break;
	default:
		return h5_error_internal (f, __FILE__, __func__, __LINE__);
	}

	TRY( h5tpriv_open_mesh_group (f) );

	t->cur_mesh = id;

	if (id != t->num_meshes) {	/* open existing */
		TRY( h5tpriv_read_mesh (f) );

	} else {			/* append new */
		t->num_meshes++;
		t->mesh_changed = id;
		t->num_levels = 0;
	} 

	return H5_SUCCESS;
}

static h5_err_t
release_elems (
	h5_file_t* const f
	) {
	h5t_fdata_t* t = f->t;
	TRY( h5priv_free (f, t->elems.data) );
	TRY( h5priv_free (f, t->num_elems) );
	TRY( h5priv_free (f, t->elems_ldta) );
	TRY( h5priv_free (f, t->num_elems_on_level) );
	TRY( h5priv_free (f, t->map_elem_g2l.items) );

	return H5_SUCCESS;
}

static h5_err_t
release_vertices (
	h5_file_t* const f
	) {
	h5t_fdata_t* t = f->t;
	TRY( h5priv_free (f, t->vertices) ); 
	TRY( h5priv_free (f, t->vertices_data) );
	TRY( h5priv_free (f, t->num_vertices) );
	TRY( h5priv_free (f, t->map_vertex_g2l.items) );

	return H5_SUCCESS;
}

static h5_err_t
release_memory (
	h5_file_t* const f
	) {
	TRY( h5tpriv_release_tags (f) );
	if (f->t->methods.adjacency != NULL) {
		TRY( (*f->t->methods.adjacency->release_internal_structs) (f) );
	}
	TRY( release_elems (f) );
	TRY( release_vertices (f) );

	return H5_SUCCESS;
}

h5_err_t
h5t_close_mesh (
	h5_file_t* const f
	) {
	TRY( h5tpriv_write_mesh (f) );
	TRY( release_memory (f) );
	TRY( init_fdata (f) );

	return H5_SUCCESS;
}

h5_err_t
h5t_set_level (
	h5_file_t* const f,
	const h5_id_t level_id
	) {
	h5t_fdata_t* t = f->t;

	if ((level_id < 0) || (level_id >= t->num_levels))
		return HANDLE_H5_OUT_OF_RANGE_ERR (f, "Level", level_id);

	h5_id_t prev_level = t->cur_level;
	t->cur_level = level_id;

	if (level_id >= t->num_loaded_levels) {
		TRY( (t->methods.adjacency->update_internal_structs)(f, prev_level+1) );
	}
	return H5_SUCCESS;
}

h5_err_t
h5tpriv_alloc_num_vertices (
	h5_file_t* const f,
	const h5_size_t num_vertices
	) {
	h5t_fdata_t* t = f->t;

	ssize_t size = num_vertices * sizeof (t->vertices[0]);
	TRY( t->vertices = h5priv_alloc (f, t->vertices, size) );
	size = num_vertices * sizeof (t->vertices_data[0]);
	TRY( t->vertices_data = h5priv_alloc (f, t->vertices_data, size) );
	TRY( h5priv_alloc_idmap (f, &t->map_vertex_g2l, num_vertices) );
	TRY( h5priv_alloc_idlist_items (f, &t->sorted_lvertices, num_vertices) );

	return H5_SUCCESS;
}
