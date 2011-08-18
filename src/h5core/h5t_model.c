#include <string.h>

#include "h5core/h5_core.h"
#include "h5_core_private.h"

/*
  create several HDF5 types
*/
static inline h5_err_t
create_array_types (
	h5t_mesh_t* const m
	) {
	H5_PRIV_FUNC_ENTER (h5_err_t, "m=%p", m);
	h5_dtypes_t* dtypes = &(m->dtypes);

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
	h5t_mesh_t* const m
	) {
	H5_PRIV_FUNC_ENTER (h5_err_t, "m=%p", m);
	h5_dtypes_t* dtypes = &m->dtypes;

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
	h5t_mesh_t* const m
	) {
	H5_PRIV_FUNC_ENTER (h5_err_t, "m=%p", m);
	h5_dtypes_t* dtypes = &m->dtypes;

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
	h5t_mesh_t* const m
	) {
	H5_PRIV_FUNC_ENTER (h5_err_t, "m=%p", m);
	h5_dtypes_t* dtypes = &m->dtypes;

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
	h5t_mesh_t* const m
	) {
	H5_PRIV_FUNC_ENTER (h5_err_t, "m=%p", m);
	h5_dtypes_t* dtypes = &m->dtypes;

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

/*!
  \ingroup h5_private

  \internal

  Initialize topo internal structure. The structure has already be initialized
  with zero's.

  \return	H5_SUCCESS or error code
*/
h5_err_t
h5tpriv_init_mesh (
	h5t_mesh_t* const m,
	h5_file_t* const f,
	const char* name,
	const hid_t mesh_hid
	) {
	H5_PRIV_API_ENTER (h5_err_t, "m=%p", m);

	m->f = f;
	strncpy (m->mesh_name, name, sizeof (m->mesh_name)-1);
	m->mesh_name[sizeof(m->mesh_name)-1] = '\0';
	m->mesh_gid = mesh_hid;


	m->dtypes.h5_glb_idx_t = H5_INT64_T;
	m->dtypes.h5_int64_t = H5_INT64_T;
	m->dtypes.h5_float64_t = H5_FLOAT64_T;

	TRY (create_array_types (m));
	TRY (create_vertex_type (m));
	TRY (create_triangle_type (m));
	TRY (create_tet_type (m));
	TRY (create_tag_types (m));

	m->ref_elem = NULL;
	m->mesh_changed = 0;
	m->num_leaf_levels = -1;
	m->leaf_level = -1;
	m->last_stored_vid = -1;
	m->last_stored_eid = -1;


	/* initialize pointers */
	m->glb_elems.data =		NULL;
	m->loc_elems.data =		NULL;
	m->num_elems =			NULL;
	m->num_leaf_elems =	NULL;
	m->map_elem_g2l.items =		NULL;
	m->vertices =			NULL; 
	m->num_vertices =		NULL;
	m->map_vertex_g2l.items =	NULL;

	/* vertices */
	strcpy (m->dsinfo_vertices.name, "Vertices");
	m->dsinfo_vertices.rank = 1;
	m->dsinfo_vertices.dims[0] = 0;
	m->dsinfo_vertices.max_dims[0] = H5S_UNLIMITED;
	m->dsinfo_vertices.chunk_dims[0] = 4096;
	m->dsinfo_vertices.type_id = m->dtypes.h5_vertex_t;
	TRY (m->dsinfo_vertices.create_prop = hdf5_create_property (
		     H5P_DATASET_CREATE) );
	TRY (hdf5_set_chunk_property (
		     m->dsinfo_vertices.create_prop,
		     m->dsinfo_vertices.rank,
		     m->dsinfo_vertices.chunk_dims) );
	m->dsinfo_vertices.access_prop = H5P_DEFAULT;

	/* Elems */
	strcpy (m->dsinfo_elems.name, "Elems");
	m->dsinfo_elems.rank = 1;
	m->dsinfo_elems.dims[0] = 0;
	m->dsinfo_elems.max_dims[0] = H5S_UNLIMITED;
	m->dsinfo_elems.chunk_dims[0] = 4096;
	TRY (m->dsinfo_elems.create_prop = hdf5_create_property (
		     H5P_DATASET_CREATE) );
	TRY (hdf5_set_chunk_property (
		     m->dsinfo_elems.create_prop,
		     m->dsinfo_elems.rank,
		     m->dsinfo_elems.chunk_dims) );
	m->dsinfo_elems.access_prop = H5P_DEFAULT;

	H5_PRIV_FUNC_RETURN (H5_SUCCESS);
}

/*
  Open HDF5 group with specific mesh
*/
static h5_err_t
release_elems (
	h5t_mesh_t* const m
	) {
	H5_PRIV_FUNC_ENTER (h5_err_t, "m=%p", m);
	TRY( h5_free (m->loc_elems.data) );
	m->loc_elems.data = NULL;
	TRY( h5_free (m->num_elems) );
	m->num_elems = NULL;
	TRY( h5_free (m->num_leaf_elems) );
	m->num_leaf_elems = NULL;
	TRY( h5_free (m->map_elem_g2l.items) );
	m->map_elem_g2l.items = NULL;

	H5_PRIV_FUNC_RETURN (H5_SUCCESS);
}

static h5_err_t
release_vertices (
	h5t_mesh_t* const m
	) {
	H5_PRIV_FUNC_ENTER (h5_err_t, "m=%p", m);
	TRY( h5_free (m->vertices) );
	m->vertices = NULL;
	TRY( h5_free (m->num_vertices) );
	m->num_vertices = NULL;
	TRY( h5_free (m->map_vertex_g2l.items) );
	m->map_vertex_g2l.items = NULL;

	H5_PRIV_FUNC_RETURN (H5_SUCCESS);
}

static h5_err_t
release_memory (
	h5t_mesh_t* const m
	) {
	H5_PRIV_FUNC_ENTER (h5_err_t, "m=%p", m);
	TRY (h5tpriv_release_adjacency_structs (m));
	TRY (release_elems (m));
	TRY (release_vertices (m));
	TRY (h5_free (m));
	H5_PRIV_FUNC_RETURN (H5_SUCCESS);
}

h5_err_t
h5t_close_mesh (
	h5t_mesh_t* const m
	) {
	H5_CORE_API_ENTER (h5_err_t, "m=%p", m);

	// check if tagsets are still open
	if (m->mtagsets && m->mtagsets->num_items > 0)
		H5_CORE_API_LEAVE (
			h5_error (
				H5_ERR_H5FED,
				"Mesh cannot be closed: Mesh is referenced by open tagsets"));

	if (!(m->f->mode & H5_O_RDONLY)) {
		TRY (h5tpriv_write_mesh (m));
	}
	TRY (hdf5_close_group (m->mesh_gid));

	TRY (release_memory (m));
	H5_CORE_API_RETURN (H5_SUCCESS);
}

h5_err_t
h5t_set_level (
	h5t_mesh_t* const m,
	const h5t_lvl_idx_t level_id
	) {
	H5_CORE_API_ENTER (h5_err_t, "m=%p, level_id=%d", m, level_id);

	if ((level_id < 0) || (level_id >= m->num_leaf_levels))
		H5_CORE_API_LEAVE (HANDLE_H5_OUT_OF_RANGE_ERR ("Level", level_id));

	h5t_lvl_idx_t prev_level = m->leaf_level;
	m->leaf_level = level_id;

	if (level_id >= m->num_loaded_levels) {
		TRY (h5tpriv_update_adjacency_structs (m, ++prev_level));
	}
	H5_CORE_API_RETURN (H5_SUCCESS);
}

/*
  Allocate \c num additional vertices. 
 */
h5_err_t
h5tpriv_alloc_num_vertices (
	h5t_mesh_t* const m,
	const h5_size_t num
	) {
	H5_PRIV_FUNC_ENTER (h5_err_t,
			     "m=%p, num=%llu",
			     m,
			     (long long unsigned)num);
	ssize_t size = num * sizeof (m->vertices[0]);
	TRY (m->vertices = h5_alloc (m->vertices, size));
	TRY (h5priv_alloc_idxmap (&m->map_vertex_g2l, num));
	H5_PRIV_API_RETURN (H5_SUCCESS);
}
