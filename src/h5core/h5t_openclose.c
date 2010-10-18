#include <string.h>

#include "h5core/h5_core.h"
#include "h5_core_private.h"

static struct h5t_methods tet_funcs = {
	&h5tpriv_read_tetm_methods,
	&h5tpriv_tetm_store_methods,
	&h5tpriv_tetm_retrieve_methods,
	&h5tpriv_access_tetm_methods,
	&h5tpriv_tetm_adjacency_methods
};

static struct h5t_methods tri_funcs = {
	&h5tpriv_read_trim_methods,
	&h5tpriv_trim_store_methods,
	&h5tpriv_trim_retrieve_methods,
	&h5tpriv_access_trim_methods,
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
		dtypes->h5_3glb_idx_t = h5priv_create_hdf5_array_type (
			f,
			H5_ID_T,
			1,
			dims)
		);
	dims[0] = 4;
	TRY(
		dtypes->h5_4glb_idx_t = h5priv_create_hdf5_array_type (
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
			sizeof (h5_glb_vertex_t)) );
	TRY(
		h5priv_insert_hdf5_type (
			f,
			dtypes->h5_vertex_t,
			"idx",
			HOFFSET (h5_glb_vertex_t, idx),
			H5_ID_T) );
	TRY(
		h5priv_insert_hdf5_type (
			f,
			dtypes->h5_vertex_t,
			"P",
			HOFFSET (h5_glb_vertex_t, P),
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
			sizeof (h5_glb_triangle_t)) );
	TRY(
		h5priv_insert_hdf5_type (
			f,
			dtypes->h5_triangle_t,
			"idx",
			HOFFSET (h5_glb_triangle_t, idx),
			H5_ID_T) );
	TRY(
		h5priv_insert_hdf5_type (
			f,
			dtypes->h5_triangle_t,
			"parent_idx",
			HOFFSET (h5_glb_triangle_t, parent_idx),
			H5_ID_T) );
	TRY(
		h5priv_insert_hdf5_type (
			f,
			dtypes->h5_triangle_t,
			"child_idx",
			HOFFSET(h5_glb_triangle_t, child_idx),
			H5_ID_T) );
	TRY(
		h5priv_insert_hdf5_type (
			f,
			dtypes->h5_triangle_t,
			"vertex_indices",
			HOFFSET (h5_glb_triangle_t, vertex_indices),
			dtypes->h5_3glb_idx_t) );
	TRY(
		h5priv_insert_hdf5_type (
			f,
			dtypes->h5_triangle_t,
			"neighbor_indices",
			HOFFSET(h5_glb_triangle_t, neighbor_indices),
			dtypes->h5_3glb_idx_t) );

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
			sizeof (h5_glb_tetrahedron_t)) );
	TRY(
		h5priv_insert_hdf5_type (
			f,
			dtypes->h5_tet_t,
			"idx",
			HOFFSET (h5_glb_tetrahedron_t, idx),
			H5_ID_T) );
	TRY(
		h5priv_insert_hdf5_type (
			f,
			dtypes->h5_tet_t,
			"parent_idx",
			HOFFSET (h5_glb_tetrahedron_t, parent_idx),
			H5_ID_T) );
	TRY(
		h5priv_insert_hdf5_type (
			f,
			dtypes->h5_tet_t,
			"child_idx",
			HOFFSET (h5_glb_tetrahedron_t, child_idx),
			H5T_NATIVE_INT32) );
	TRY(
		h5priv_insert_hdf5_type (
			f,
			dtypes->h5_tet_t,
			"vertex_indices",
			HOFFSET (h5_glb_tetrahedron_t, vertex_indices),
			dtypes->h5_4glb_idx_t) );
	TRY(
		h5priv_insert_hdf5_type (
			f,
			dtypes->h5_tet_t,
			"neighbor_indices",
			HOFFSET (h5_glb_tetrahedron_t, neighbor_indices),
			dtypes->h5_4glb_idx_t) );

	return H5_SUCCESS;
}

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

	/* initialize pointers */
	t->glb_elems.data =		NULL;
	t->loc_elems.data =		NULL;
	t->num_elems =			NULL;
	t->num_elems_on_level =		NULL;
	t->map_elem_g2l.items =		NULL;
	t->vertices =			NULL; 
	t->num_vertices =		NULL;
	t->map_vertex_g2l.items =	NULL;
	t->mtags.names =		NULL;

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

	TRY( (f->t = h5priv_calloc (f, 1, sizeof (*f->t))) );
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

h5_err_t
h5tpriv_init_step (
	h5_file_t* const f
	) {
#pragma unused f
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
#pragma unused f
	return H5_SUCCESS;
}


h5_err_t
h5tpriv_open_topo_group (
	h5_file_t * const f
	) {
	h5t_fdata_t* t = f->t;
	if (t->topo_gid == 0 || t->topo_gid == -1) {
		t->topo_gid = h5priv_open_group (f, f->root_gid, H5T_CONTAINER_GRPNAME);
	}
	return t->topo_gid;
}

h5_err_t
h5tpriv_open_meshes_group (
	h5_file_t* const f,
	const h5_oid_t type_id
	) {
	h5t_fdata_t* t = f->t;

	if (t->topo_gid < 0) {
		TRY( h5tpriv_open_topo_group (f) );
	}
	TRY( (t->meshes_gid = h5priv_open_group (
		      f,
		      t->topo_gid,
		      h5tpriv_meshes_grpnames[type_id])) );
	t->mesh_type = type_id;

	return H5_SUCCESS;
}

/*
  Open HDF5 group with specific mesh
*/

h5_err_t
h5tpriv_open_mesh_group (
	h5_file_t* const f,
	const h5_oid_t type_id,
	const h5_id_t id
	) {
	h5t_fdata_t* t = f->t;

	if (t->meshes_gid < 0) {
		TRY( h5tpriv_open_meshes_group (f, type_id) );
	}
	snprintf (t->mesh_name, sizeof (t->mesh_name), "%lld", (long long)id);

	TRY( (t->mesh_gid = h5priv_open_group (
		      f,
		      t->meshes_gid,
		      t->mesh_name)) );
	t->cur_mesh = id;
	return H5_SUCCESS;
}

/*
  If the value of parameter \c id is \c -1, a new mesh will be appended.
*/
h5_err_t
h5t_open_mesh (
	h5_file_t* const f,
	h5_id_t id,
	const h5_oid_t type_id
	) {
	h5_debug (f, "%s ()", __func__);
	h5t_fdata_t* t = f->t;

	TRY( h5t_close_mesh (f) );

	if (t->num_meshes < 0) {
		h5_size_t result = h5t_get_num_meshes (f, type_id);
		t->num_meshes = (result > 0 ? result : 0);
	}
	if ((id < -1) || (id >= t->num_meshes)) {
		return HANDLE_H5_OUT_OF_RANGE_ERR (f, "mesh", id);
	}
	if (id == -1) {  /* append new mesh */
		id = t->num_meshes;
	}
	switch (type_id) {
	case H5_OID_TETRAHEDRON:
		t->dsinfo_elems.type_id = t->dtypes.h5_tet_t;
		t->methods = tet_funcs;
		t->ref_elem = &h5t_tet_ref_elem;
		break;
	case H5_OID_TRIANGLE:
		t->dsinfo_elems.type_id = t->dtypes.h5_triangle_t;
		t->methods = tri_funcs;
		t->ref_elem = &h5t_tri_ref_elem;
		break;
	default:
		return h5_error_internal (f, __FILE__, __func__, __LINE__);
	}

	TRY( h5tpriv_open_mesh_group (f, type_id, id) );

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
	TRY( h5priv_free (f, t->glb_elems.data) );
	TRY( h5priv_free (f, t->loc_elems.data) );
	TRY( h5priv_free (f, t->num_elems) );
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
	TRY( h5priv_free (f, t->num_vertices) );
	TRY( h5priv_free (f, t->map_vertex_g2l.items) );

	return H5_SUCCESS;
}

static h5_err_t
release_memory (
	h5_file_t* const f
	) {
	TRY( h5tpriv_release_tags (f) );
	TRY( h5tpriv_release_adjacency_structs (f) );
	TRY( release_elems (f) );
	TRY( release_vertices (f) );

	return H5_SUCCESS;
}

h5_err_t
h5t_close_mesh (
	h5_file_t* const f
	) {
	h5_debug (f, "%s ()", __func__);
	if (!(f->mode & H5_O_RDONLY)) {
		TRY( h5tpriv_write_mesh (f) );
	}
	TRY( h5priv_close_hdf5_group (f, f->t->mesh_gid) );
	TRY( h5priv_close_hdf5_group (f, f->t->meshes_gid) );
	TRY( h5priv_close_hdf5_group (f, f->t->topo_gid) );

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
		TRY( (h5tpriv_update_adjacency_structs)(f, prev_level+1) );
	}
	return H5_SUCCESS;
}

/*
  Allocate \c num additional vertices. 
 */
h5_err_t
h5tpriv_alloc_num_vertices (
	h5_file_t* const f,
	const h5_size_t num
	) {
	h5t_fdata_t* t = f->t;

	ssize_t size = num * sizeof (t->vertices[0]);
	TRY( t->vertices = h5priv_alloc (f, t->vertices, size) );
	TRY( h5priv_alloc_idxmap (f, &t->map_vertex_g2l, num) );
	TRY( h5priv_alloc_idlist_items (f, &t->sorted_lvertices, num) );

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
	h5_debug (f, "%s ()", __func__);
	TRY( h5t_close_mesh (f) );
	TRY( h5priv_close_hdf5_group (f, f->t->meshes_gid) );

	return H5_SUCCESS;
}
