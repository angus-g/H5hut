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
static inline h5_err_t
create_array_types (
	h5_file_t* const f
	) {
	H5_PRIV_FUNC_ENTER (h5_err_t);
	h5t_fdata_t* t = f->t;
	h5_dtypes_t* dtypes = &(t->dtypes);

	hsize_t dims[1] = { 3 };
	TRY(
		dtypes->h5_coord3d_t = hdf5_create_array_type (
			H5_FLOAT64_T,
			1,
			dims)
		);
	TRY( 
		dtypes->h5_3glb_idx_t = hdf5_create_array_type (
			H5_ID_T,
			1,
			dims)
		);
	dims[0] = 4;
	TRY(
		dtypes->h5_4glb_idx_t = hdf5_create_array_type (
			H5_ID_T,
			1,
			dims)
		);

	H5_PRIV_FUNC_RETURN (H5_SUCCESS);
}

static inline h5_err_t
create_vertex_type (
	h5_file_t* const f
	) {
	H5_PRIV_FUNC_ENTER (h5_err_t);
	h5_dtypes_t* dtypes = &f->t->dtypes;

	TRY(
		dtypes->h5_vertex_t = hdf5_create_type (
			H5_COMPOUND_T,
			sizeof (h5_glb_vertex_t)) );
	TRY(
		hdf5_insert_type (
			dtypes->h5_vertex_t,
			"idx",
			HOFFSET (h5_glb_vertex_t, idx),
			H5_ID_T) );
	TRY(
		hdf5_insert_type (
			dtypes->h5_vertex_t,
			"P",
			HOFFSET (h5_glb_vertex_t, P),
			dtypes->h5_coord3d_t) );

	H5_PRIV_FUNC_RETURN (H5_SUCCESS);
}

static inline h5_err_t
create_triangle_type (
	h5_file_t* const f
	) {
	H5_PRIV_FUNC_ENTER (h5_err_t);
	h5_dtypes_t* dtypes = &f->t->dtypes;

	TRY(
		dtypes->h5_triangle_t = hdf5_create_type (
			H5_COMPOUND_T,
			sizeof (h5_glb_triangle_t)) );
	TRY(
		hdf5_insert_type (
			dtypes->h5_triangle_t,
			"idx",
			HOFFSET (h5_glb_triangle_t, idx),
			H5_ID_T) );
	TRY(
		hdf5_insert_type (
			dtypes->h5_triangle_t,
			"parent_idx",
			HOFFSET (h5_glb_triangle_t, parent_idx),
			H5_ID_T) );
	TRY(
		hdf5_insert_type (
			dtypes->h5_triangle_t,
			"child_idx",
			HOFFSET(h5_glb_triangle_t, child_idx),
			H5_ID_T) );
	TRY(
		hdf5_insert_type (
			dtypes->h5_triangle_t,
			"vertex_indices",
			HOFFSET (h5_glb_triangle_t, vertex_indices),
			dtypes->h5_3glb_idx_t) );
	TRY(
		hdf5_insert_type (
			dtypes->h5_triangle_t,
			"neighbor_indices",
			HOFFSET(h5_glb_triangle_t, neighbor_indices),
			dtypes->h5_3glb_idx_t) );

	H5_PRIV_FUNC_RETURN (H5_SUCCESS);
}

static inline h5_err_t
create_tag_types (
	h5_file_t* const f
	) {
	H5_PRIV_FUNC_ENTER (h5_err_t);
	h5_dtypes_t* dtypes = &f->t->dtypes;

	TRY (
		dtypes->h5t_glb_tag_idx_t = hdf5_create_type (
			H5_COMPOUND_T,
			sizeof (h5t_glb_tag_idx_t)) );
	TRY (
		hdf5_insert_type (
			dtypes->h5t_glb_tag_idx_t,
			"eid",
			HOFFSET (h5t_glb_tag_idx_t, eid),
			H5_ID_T) );
	TRY (
		hdf5_insert_type (
			dtypes->h5t_glb_tag_idx_t,
			"idx",
			HOFFSET (h5t_glb_tag_idx_t, idx),
			H5_ID_T) );

	H5_PRIV_FUNC_RETURN (H5_SUCCESS);
}

static inline h5_err_t
create_tet_type (
	h5_file_t* const f
	) {
	H5_PRIV_FUNC_ENTER (h5_err_t);
	h5_dtypes_t* dtypes = &f->t->dtypes;

	TRY(
		dtypes->h5_tet_t = hdf5_create_type (
			H5_COMPOUND_T,
			sizeof (h5_glb_tetrahedron_t)) );
	TRY(
		hdf5_insert_type (
			dtypes->h5_tet_t,
			"idx",
			HOFFSET (h5_glb_tetrahedron_t, idx),
			H5_ID_T) );
	TRY(
		hdf5_insert_type (
			dtypes->h5_tet_t,
			"parent_idx",
			HOFFSET (h5_glb_tetrahedron_t, parent_idx),
			H5_ID_T) );
	TRY(
		hdf5_insert_type (
			dtypes->h5_tet_t,
			"child_idx",
			HOFFSET (h5_glb_tetrahedron_t, child_idx),
			H5T_NATIVE_INT32) );
	TRY(
		hdf5_insert_type (
			dtypes->h5_tet_t,
			"vertex_indices",
			HOFFSET (h5_glb_tetrahedron_t, vertex_indices),
			dtypes->h5_4glb_idx_t) );
	TRY(
		hdf5_insert_type (
			dtypes->h5_tet_t,
			"neighbor_indices",
			HOFFSET (h5_glb_tetrahedron_t, neighbor_indices),
			dtypes->h5_4glb_idx_t) );

	H5_PRIV_FUNC_RETURN (H5_SUCCESS);
}

static h5_err_t
init_fdata (
	h5_file_t* const f
	) {
	H5_PRIV_FUNC_ENTER (h5_err_t);
	h5t_fdata_t* t = f->t;

	memset (t->mesh_name, 0, sizeof (t->mesh_name));
	memset (t->mesh_label, 0, sizeof (t->mesh_label));
	t->mesh_type = 0;
	t->ref_elem = NULL;
	t->cur_mesh = -1;
	t->mesh_changed = 0;
	t->num_meshes = -1;
	t->num_leaf_levels = -1;
	t->leaf_level = -1;
	t->last_stored_vid = -1;
	t->last_stored_eid = -1;
	t->topo_gid = -1;
	t->meshes_gid = -1;
	t->mesh_gid = -1;

	/* initialize pointers */
	t->glb_elems.data =		NULL;
	t->loc_elems.data =		NULL;
	t->num_elems =			NULL;
	t->num_elems_on_leaf_level =	NULL;
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
	TRY (t->dsinfo_vertices.create_prop = hdf5_create_property (
		     H5P_DATASET_CREATE) );
	TRY (hdf5_set_chunk_property (
		     t->dsinfo_vertices.create_prop,
		     t->dsinfo_vertices.rank,
		     t->dsinfo_vertices.chunk_dims) );
	t->dsinfo_vertices.access_prop = H5P_DEFAULT;

	/* NumVertices */
	strcpy (t->dsinfo_num_vertices.name, "NumVertices");
	t->dsinfo_num_vertices.rank = 1;
	t->dsinfo_num_vertices.dims[0] = 0;
	t->dsinfo_num_vertices.max_dims[0] = H5S_UNLIMITED;
	t->dsinfo_num_vertices.chunk_dims[0] = 4096;
	t->dsinfo_num_vertices.type_id = t->dtypes.h5_glb_idx_t;
	TRY (t->dsinfo_num_vertices.create_prop = hdf5_create_property (
		     H5P_DATASET_CREATE) );
	TRY (hdf5_set_chunk_property (
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
	TRY (t->dsinfo_elems.create_prop = hdf5_create_property (
		     H5P_DATASET_CREATE) );
	TRY (hdf5_set_chunk_property (
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
	t->dsinfo_num_elems.type_id = t->dtypes.h5_glb_idx_t;
	TRY (t->dsinfo_num_elems.create_prop = hdf5_create_property (
		     H5P_DATASET_CREATE) );
	TRY( hdf5_set_chunk_property (
		     t->dsinfo_num_elems.create_prop,
		     t->dsinfo_num_elems.rank,
		     t->dsinfo_num_elems.chunk_dims) );
	t->dsinfo_num_elems.access_prop = H5P_DEFAULT;

	/* NumElemsOnLevel */
	strcpy (t->dsinfo_num_elems_on_leaf_level.name, "NumElemsOnLeafLevel");
	t->dsinfo_num_elems_on_leaf_level.rank = 1;
	t->dsinfo_num_elems_on_leaf_level.dims[0] = 0;
	t->dsinfo_num_elems_on_leaf_level.max_dims[0] = H5S_UNLIMITED;
	t->dsinfo_num_elems_on_leaf_level.chunk_dims[0] = 4096;
	t->dsinfo_num_elems_on_leaf_level.type_id = t->dtypes.h5_glb_idx_t;
	TRY( t->dsinfo_num_elems_on_leaf_level.create_prop = hdf5_create_property (
		     H5P_DATASET_CREATE) );
	TRY( hdf5_set_chunk_property (
		     t->dsinfo_num_elems_on_leaf_level.create_prop,
		     t->dsinfo_num_elems_on_leaf_level.rank,
		     t->dsinfo_num_elems_on_leaf_level.chunk_dims) );
	t->dsinfo_num_elems_on_leaf_level.access_prop = H5P_DEFAULT;

	H5_PRIV_FUNC_RETURN (H5_SUCCESS);
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
	H5_PRIV_API_ENTER1 (h5_err_t, "f=0x%p", f);
	TRY (f->t = h5_calloc (1, sizeof (*f->t)));
	h5t_fdata_t* t = f->t;

	t->dtypes.h5_glb_idx_t = H5_INT64_T;
	t->dtypes.h5_int64_t = H5_INT64_T;
	t->dtypes.h5_float64_t = H5_FLOAT64_T;

	TRY (create_array_types (f));
	TRY (create_vertex_type (f));
	TRY (create_triangle_type (f));
	TRY (create_tet_type (f));
	TRY (create_tag_types (f));
	TRY (init_fdata (f));
	H5_PRIV_API_RETURN (H5_SUCCESS);
}

h5_err_t
h5tpriv_init_step (
	h5_file_t* const f
	) {
	H5_PRIV_API_ENTER1 (h5_err_t, "f=0x%p", f);
	UNUSED_ARGUMENT (f);
	H5_PRIV_API_RETURN (H5_SUCCESS);
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
	H5_PRIV_API_ENTER1 (h5_err_t, "f=0x%p", f);
	UNUSED_ARGUMENT (f);
	H5_PRIV_API_RETURN (H5_SUCCESS);
}


static inline h5_err_t
open_topo_group (
	h5_file_t * const f
	) {
	h5t_fdata_t* t = f->t;
	if (t->topo_gid == 0 || t->topo_gid == -1) {
		return (t->topo_gid = h5priv_open_group (f, f->root_gid, H5T_CONTAINER_GRPNAME));
	}
	return (t->topo_gid);
}

static inline h5_err_t
open_tetmeshes_group (
	h5_file_t* const f
	) {
	H5_PRIV_FUNC_ENTER (h5_err_t);
	h5t_fdata_t* t = f->t;

	if (t->topo_gid < 0) {
		TRY (open_topo_group (f));
	}
	TRY (t->meshes_gid = h5priv_open_group (
		      f,
		      t->topo_gid,
		      TETRAHEDRAL_MESHES_GRPNAME));
	t->mesh_type = H5_OID_TETRAHEDRON;
	H5_PRIV_FUNC_RETURN (H5_SUCCESS);
}

static inline h5_err_t
open_trimeshes_group (
	h5_file_t* const f
	) {
	H5_PRIV_FUNC_ENTER (h5_err_t);
	h5t_fdata_t* t = f->t;

	if (t->topo_gid < 0) {
		TRY (open_topo_group (f));
	}
	TRY (t->meshes_gid = h5priv_open_group (
		      f,
		      t->topo_gid,
		      TRIANGLE_MESHES_GRPNAME));
	t->mesh_type = H5_OID_TRIANGLE;
	H5_PRIV_FUNC_RETURN (H5_SUCCESS);
}

/*
  Open HDF5 group with specific mesh
*/

static inline h5_err_t
open_tetmesh_group (
	h5_file_t* const f,
	const h5_id_t id
	) {
	H5_PRIV_FUNC_ENTER (h5_err_t);
	h5t_fdata_t* t = f->t;

	if (t->meshes_gid < 0) {
		TRY (open_tetmeshes_group (f));
	}
	snprintf (t->mesh_name, sizeof (t->mesh_name), "%lld", (long long)id);

	TRY (t->mesh_gid = h5priv_open_group (
		      f,
		      t->meshes_gid,
		      t->mesh_name));
	t->cur_mesh = id;
	H5_PRIV_API_RETURN (H5_SUCCESS);
}

static inline h5_err_t
open_trimesh_group (
	h5_file_t* const f,
	const h5_id_t id
	) {
	H5_PRIV_FUNC_ENTER (h5_err_t);
	h5t_fdata_t* t = f->t;

	if (t->meshes_gid < 0) {
		TRY (open_trimeshes_group (f));
	}
	snprintf (t->mesh_name, sizeof (t->mesh_name), "%lld", (long long)id);

	TRY (t->mesh_gid = h5priv_open_group (
		      f,
		      t->meshes_gid,
		      t->mesh_name));
	t->cur_mesh = id;
	H5_PRIV_API_RETURN (H5_SUCCESS);
}

/*
  If the value of parameter \c id is \c -1, a new mesh will be appended.
*/
h5_err_t
h5t_open_tetrahedral_mesh (
	h5_file_t* const f,
	h5_id_t id
	) {
	H5_CORE_API_ENTER2 (h5_err_t, "f=0x%p, id=%lld", f, (long long)id);
	h5t_fdata_t* t = f->t;

	TRY (h5t_close_mesh (f));

	if (t->num_meshes < 0) {
		h5_size_t result = h5t_get_num_tetmeshes (f);
		t->num_meshes = (result > 0 ? result : 0);
	}
	if ((id < -1) || (id >= t->num_meshes)) {
		H5_CORE_API_LEAVE (HANDLE_H5_OUT_OF_RANGE_ERR ("mesh", id));
	}
	t->dsinfo_elems.type_id = t->dtypes.h5_tet_t;
	t->methods = tet_funcs;
	t->ref_elem = &h5t_tet_ref_elem;
	TRY (open_tetmesh_group (f, id));

	if (id == -1) {			// append new
		id = t->num_meshes;
		t->num_meshes++;
		t->mesh_changed = id;
		t->num_leaf_levels = 0;
	} else {			// read existing
		TRY (h5tpriv_read_mesh (f));
	} 
	H5_CORE_API_RETURN (H5_SUCCESS);
}

h5_err_t
h5t_open_triangle_mesh (
	h5_file_t* const f,
	h5_id_t id
	) {
	H5_CORE_API_ENTER2 (h5_err_t, "f=0x%p, id=%lld", f, (long long)id);
	h5t_fdata_t* t = f->t;

	TRY (h5t_close_mesh (f));

	if (t->num_meshes < 0) {
		h5_size_t result = h5t_get_num_trimeshes (f);
		t->num_meshes = (result > 0 ? result : 0);
	}
	if ((id < -1) || (id >= t->num_meshes)) {
		H5_CORE_API_LEAVE (HANDLE_H5_OUT_OF_RANGE_ERR ("mesh", id));
	}
	t->dsinfo_elems.type_id = t->dtypes.h5_triangle_t;
	t->methods = tri_funcs;
	t->ref_elem = &h5t_tri_ref_elem;
	TRY (open_trimesh_group (f, id));

	if (id == -1) {			// append new
		id = t->num_meshes;
		t->num_meshes++;
		t->mesh_changed = id;
		t->num_leaf_levels = 0;
	} else {			// read existing
		TRY (h5tpriv_read_mesh (f));
	} 
	H5_CORE_API_RETURN (H5_SUCCESS);
}

static h5_err_t
release_elems (
	h5_file_t* const f
	) {
	H5_PRIV_FUNC_ENTER1 (h5_err_t, "f=0x%p", f);
	h5t_fdata_t* t = f->t;
	TRY( h5_free (t->loc_elems.data) );
	t->loc_elems.data = NULL;
	TRY( h5_free (t->num_elems) );
	t->num_elems = NULL;
	TRY( h5_free (t->num_elems_on_leaf_level) );
	t->num_elems_on_leaf_level = NULL;
	TRY( h5_free (t->map_elem_g2l.items) );
	t->map_elem_g2l.items = NULL;

	H5_PRIV_FUNC_RETURN (H5_SUCCESS);
}

static h5_err_t
release_vertices (
	h5_file_t* const f
	) {
	H5_PRIV_FUNC_ENTER1 (h5_err_t, "f=0x%p", f);
	h5t_fdata_t* t = f->t;
	TRY( h5_free (t->vertices) );
	t->vertices = NULL;
	TRY( h5_free (t->num_vertices) );
	t->num_vertices = NULL;
	TRY( h5_free (t->map_vertex_g2l.items) );
	t->map_vertex_g2l.items = NULL;

	H5_PRIV_FUNC_RETURN (H5_SUCCESS);
}

static h5_err_t
release_memory (
	h5_file_t* const f
	) {
	H5_PRIV_FUNC_ENTER1 (h5_err_t, "f=0x%p", f);
	TRY( h5tpriv_release_tags (f) );
	TRY( h5tpriv_release_adjacency_structs (f) );
	TRY( release_elems (f) );
	TRY( release_vertices (f) );
	H5_PRIV_FUNC_RETURN (H5_SUCCESS);
}

h5_err_t
h5t_close_mesh (
	h5_file_t* const f
	) {
	H5_CORE_API_ENTER1 (h5_err_t, "f=0x%p", f);
	if (!(f->mode & H5_O_RDONLY)) {
		TRY (h5tpriv_write_mesh (f));
	}
	TRY (hdf5_close_group (f->t->mesh_gid));
	TRY (hdf5_close_group (f->t->meshes_gid));
	TRY (hdf5_close_group (f->t->topo_gid));

	TRY (release_memory (f));
	TRY (init_fdata (f));
	H5_CORE_API_RETURN (H5_SUCCESS);
}

h5_err_t
h5t_set_level (
	h5_file_t* const f,
	const h5t_lvl_idx_t level_id
	) {
	H5_CORE_API_ENTER2 (h5_err_t, "f=0x%p, level_id=%d", f, level_id);
	h5t_fdata_t* t = f->t;

	if ((level_id < 0) || (level_id >= t->num_leaf_levels))
		H5_CORE_API_LEAVE (HANDLE_H5_OUT_OF_RANGE_ERR ("Level", level_id));

	h5t_lvl_idx_t prev_level = t->leaf_level;
	t->leaf_level = level_id;

	if (level_id >= t->num_loaded_levels) {
		TRY (h5tpriv_update_adjacency_structs (f, ++prev_level));
	}
	H5_CORE_API_RETURN (H5_SUCCESS);
}

/*
  Allocate \c num additional vertices. 
 */
h5_err_t
h5tpriv_alloc_num_vertices (
	h5_file_t* const f,
	const h5_size_t num
	) {
	H5_PRIV_FUNC_ENTER2 (h5_err_t,
			     "f=0x%p, num=%llu",
			     f,
			     (long long unsigned)num);
	h5t_fdata_t* t = f->t;
	ssize_t size = num * sizeof (t->vertices[0]);
	TRY (t->vertices = h5_alloc (t->vertices, size));
	TRY (h5priv_alloc_idxmap (&t->map_vertex_g2l, num));
	H5_PRIV_API_RETURN (H5_SUCCESS);
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
	H5_PRIV_FUNC_ENTER1 (h5_err_t, "f=0x%p", f);
	TRY (h5t_close_mesh (f));
	TRY (hdf5_close_group (f->t->meshes_gid));
	H5_PRIV_API_RETURN (H5_SUCCESS);
}
