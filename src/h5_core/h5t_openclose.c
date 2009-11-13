#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdarg.h>	/* va_arg - System dependent ?! */
#include <string.h>
#include <errno.h>
#include <fcntl.h>
#include <hdf5.h>

#include "h5_core/h5_core.h"
#include "h5_core/h5_core_private.h"

static struct h5t_methods tet_funcs = {
	_h5t_alloc_tets,
	_h5t_store_tet,
	_h5t_refine_tet
};

static struct h5t_methods tri_funcs = {
	_h5t_alloc_tris,
	_h5t_store_tri,
	_h5t_refine_tri
};

/*
  create several HDF5 types
*/
static h5_err_t
_create_array_types (
	h5_file_t * f
	) {
	h5t_fdata_t *t = f->t;
	h5_dtypes_t *dtypes = &(t->dtypes);

	hsize_t dims[1] = { 3 };
	TRY(
		dtypes->h5_coord3d_t = _hdf_create_array_type (
			f,
			H5_FLOAT64_T,
			1,
			dims )
		);
	TRY( 
		dtypes->h5_3id_t = _hdf_create_array_type (
			f,
			H5_ID_T,
			1,
			dims )
		);
	dims[0] = 4;
	TRY(
		dtypes->h5_4id_t = _hdf_create_array_type (
			f,
			H5_ID_T,
			1,
			dims )
		);

	return H5_SUCCESS;
}

static h5_err_t
_create_vertex_type (
	h5_file_t * f
	) {
	h5_dtypes_t *dtypes = &f->t->dtypes;

	TRY(
		dtypes->h5_vertex_t = _hdf_create_type (
			f,
			H5_COMPOUND_T,
			sizeof(struct h5_vertex) ) );
	TRY(
		_hdf_insert_type (
			f,
			dtypes->h5_vertex_t,
			"global_vid",
			HOFFSET(struct h5_vertex, global_vid),
			H5_ID_T ) );
	TRY(
		_hdf_insert_type (
			f,
			dtypes->h5_vertex_t,
			"P",
			HOFFSET(struct h5_vertex, P),
			dtypes->h5_coord3d_t ) );

	return H5_SUCCESS;
}

static h5_err_t
_create_triangle_type (
	h5_file_t * f
	) {
	h5_dtypes_t *dtypes = &f->t->dtypes;

	TRY(
		dtypes->h5_triangle_t = _hdf_create_type (
			f,
			H5_COMPOUND_T,
			sizeof(struct h5_triangle) ) );
	TRY(
		_hdf_insert_type (
			f,
			dtypes->h5_triangle_t,
			"global_eid",
			HOFFSET(struct h5_triangle, global_eid),
			H5_ID_T ) );
	TRY(
		_hdf_insert_type (
			f,
			dtypes->h5_triangle_t,
			"global_parent_eid",
			HOFFSET(struct h5_triangle, global_parent_eid),
			H5_ID_T ) );
	TRY(
		_hdf_insert_type (
			f,
			dtypes->h5_triangle_t,
			"global_child_eid",
			HOFFSET(struct h5_triangle, global_child_eid),
			H5_ID_T ) );
	TRY(
		_hdf_insert_type (
			f,
			dtypes->h5_triangle_t,
			"global_vids",
			HOFFSET(struct h5_triangle, global_vids),
			dtypes->h5_3id_t ) );

	return H5_SUCCESS;
}

static h5_err_t
_create_tag_types (
	h5_file_t * f
	) {
	h5_dtypes_t *dtypes = &f->t->dtypes;

	TRY (
		dtypes->h5t_tag_idx_t = _hdf_create_type (
			f,
			H5_COMPOUND_T,
			sizeof(h5t_tag_idx_t) ) );
	TRY (
		_hdf_insert_type (
			f,
			dtypes->h5t_tag_idx_t,
			"eid",
			HOFFSET(h5t_tag_idx_t, eid),
			H5_ID_T ) );
	TRY (
		_hdf_insert_type (
			f,
			dtypes->h5t_tag_idx_t,
			"idx",
			HOFFSET(h5t_tag_idx_t, idx),
			H5_ID_T ) );

	return H5_SUCCESS;
}

static h5_err_t
_create_tet_type (
	h5_file_t * f
	) {
	h5_dtypes_t *dtypes = &f->t->dtypes;

	TRY(
		dtypes->h5_tet_t = _hdf_create_type (
			f,
			H5_COMPOUND_T,
			sizeof(struct h5_tetrahedron) ) );
	TRY(
		_hdf_insert_type (
			f,
			dtypes->h5_tet_t,
			"global_eid",
			HOFFSET(struct h5_tetrahedron, global_eid),
			H5_ID_T ) );
	TRY(
		_hdf_insert_type (
			f,
			dtypes->h5_tet_t,
			"global_parent_eid",
			HOFFSET(struct h5_tetrahedron, global_parent_eid),
			H5_ID_T ) );
	TRY(
		_hdf_insert_type (
			f,
			dtypes->h5_tet_t,
			"global_child_eid",
			HOFFSET(struct h5_tetrahedron, global_child_eid),
			H5T_NATIVE_INT32 ) );
	TRY(
		_hdf_insert_type (
			f,
			dtypes->h5_tet_t,
			"global_vids",
			HOFFSET(struct h5_tetrahedron, global_vids),
			dtypes->h5_4id_t ) );

	return H5_SUCCESS;
}


#if 0
h5_err_t
_h5_set_dataset_properties (
	h5_dsinfo_t *dsinfo,
	const char *name,
	const hid_t type,
	const int rank,
	const hsize_t *dims,
	const hsize_t *maxdims,
	const hsize_t chunk_dims
	) {

	return H5_SUCCESS;
}
#endif

static h5_err_t
_init_fdata (
	h5_file_t * f
	) {
	struct h5t_fdata * t = f->t;

	memset ( t->mesh_name, 0, sizeof ( t->mesh_name ) );
	t->num_meshes = -1;
	t->cur_mesh = -1;
	t->num_levels = -1;
	t->new_level = -1;
	t->cur_level = -1;
	t->last_stored_vid = -1;
	t->last_stored_eid = -1;
	t->topo_gid = -1;
	t->meshes_gid = -1;
	t->mesh_gid = -1;
	t->num_boundaries = -1;

	/* vertices */
	strcpy( t->dsinfo_vertices.name, "Vertices" );
	t->dsinfo_vertices.rank = 1;
	t->dsinfo_vertices.dims[0] = 0;
	t->dsinfo_vertices.max_dims[0] = H5S_UNLIMITED;
	t->dsinfo_vertices.chunk_dims[0] = 4096;
	t->dsinfo_vertices.type_id = t->dtypes.h5_vertex_t;
	TRY( t->dsinfo_vertices.create_prop = _hdf_create_property (
		     f,
		     H5P_DATASET_CREATE ) );
	TRY( _hdf_set_chunk_property (
		     f,
		     t->dsinfo_vertices.create_prop,
		     t->dsinfo_vertices.rank,
		     t->dsinfo_vertices.chunk_dims ) );
	t->dsinfo_vertices.access_prop = H5P_DEFAULT;

	/* NumVertices */
	strcpy( t->dsinfo_num_vertices.name, "NumVertices" );
	t->dsinfo_num_vertices.rank = 1;
	t->dsinfo_num_vertices.dims[0] = 0;
	t->dsinfo_num_vertices.max_dims[0] = H5S_UNLIMITED;
	t->dsinfo_num_vertices.chunk_dims[0] = 4096;
	t->dsinfo_num_vertices.type_id = t->dtypes.h5_id_t;
	TRY( t->dsinfo_num_vertices.create_prop = _hdf_create_property (
		     f,
		     H5P_DATASET_CREATE ) );
	TRY( _hdf_set_chunk_property (
		     f,
		     t->dsinfo_num_vertices.create_prop,
		     t->dsinfo_num_vertices.rank,
		     t->dsinfo_num_vertices.chunk_dims ) );
	t->dsinfo_num_vertices.access_prop = H5P_DEFAULT;

	/* Elems */
	strcpy( t->dsinfo_elems.name, "Elems" );
	t->dsinfo_elems.rank = 1;
	t->dsinfo_elems.dims[0] = 0;
	t->dsinfo_elems.max_dims[0] = H5S_UNLIMITED;
	t->dsinfo_elems.chunk_dims[0] = 4096;
	TRY( t->dsinfo_elems.create_prop = _hdf_create_property (
		     f,
		     H5P_DATASET_CREATE ) );
	TRY( _hdf_set_chunk_property (
		     f,
		     t->dsinfo_elems.create_prop,
		     t->dsinfo_elems.rank,
		     t->dsinfo_elems.chunk_dims ) );
	t->dsinfo_elems.access_prop = H5P_DEFAULT;

	/* NumElems */
	strcpy( t->dsinfo_num_elems.name, "NumElems" );
	t->dsinfo_num_elems.rank = 1;
	t->dsinfo_num_elems.dims[0] = 0;
	t->dsinfo_num_elems.max_dims[0] = H5S_UNLIMITED;
	t->dsinfo_num_elems.chunk_dims[0] = 4096;
	t->dsinfo_num_elems.type_id = t->dtypes.h5_id_t;
	TRY( t->dsinfo_num_elems.create_prop = _hdf_create_property (
		     f,
		     H5P_DATASET_CREATE ) );
	TRY( _hdf_set_chunk_property (
		     f,
		     t->dsinfo_num_elems.create_prop,
		     t->dsinfo_num_elems.rank,
		     t->dsinfo_num_elems.chunk_dims ) );
	t->dsinfo_num_elems.access_prop = H5P_DEFAULT;

	/* NumElemsOnLevel */
	strcpy( t->dsinfo_num_elems_on_level.name, "NumElemsOnLevel" );
	t->dsinfo_num_elems_on_level.rank = 1;
	t->dsinfo_num_elems_on_level.dims[0] = 0;
	t->dsinfo_num_elems_on_level.max_dims[0] = H5S_UNLIMITED;
	t->dsinfo_num_elems_on_level.chunk_dims[0] = 4096;
	t->dsinfo_num_elems_on_level.type_id = t->dtypes.h5_id_t;
	TRY( t->dsinfo_num_elems_on_level.create_prop = _hdf_create_property (
		     f,
		     H5P_DATASET_CREATE ) );
	TRY( _hdf_set_chunk_property (
		     f,
		     t->dsinfo_num_elems_on_level.create_prop,
		     t->dsinfo_num_elems_on_level.rank,
		     t->dsinfo_num_elems_on_level.chunk_dims ) );
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
_h5t_open_file (
	h5_file_t * f			/*!< IN: file handle */
	) {

	TRY( (f->t = _h5_alloc ( f, NULL, sizeof(*f->t) ) ) );
	h5t_fdata_t *t = f->t;

	t->dtypes.h5_id_t = H5_INT64_T;
	t->dtypes.h5_int64_t = H5_INT64_T;
	t->dtypes.h5_float64_t = H5_FLOAT64_T;

	TRY( _create_array_types ( f ) );
	TRY( _create_vertex_type ( f ) );
	TRY( _create_triangle_type ( f ) );
	TRY( _create_tet_type ( f ) );
	TRY( _create_tag_types ( f ) );
	TRY( _init_fdata ( f ) );

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
_h5t_close_file (
	h5_file_t *f		/*!< IN: file handle */
	) {
	TRY ( _h5t_close_mesh ( f ) );

	return H5_SUCCESS;
}

h5_err_t
_h5t_init_step (
	h5_file_t * const f
	) {

	return H5_SUCCESS;
}

/*
 - write data
 - close HDF5 objects we cannot reuse
 - free memory
*/
h5_err_t
_h5t_close_step (
	h5_file_t * f
	) {

	return H5_SUCCESS;
}


h5_err_t
_h5t_open_topo_group (
	h5_file_t * const f
	) {
	struct h5t_fdata *t = f->t;

	t->topo_gid = _h5_open_group ( f, f->root_gid, H5T_CONTAINER_GRPNAME );
	return t->topo_gid;
}

h5_err_t
_h5t_open_meshes_group (
	h5_file_t * const f
	) {
	h5t_fdata_t *t = f->t;

	if ( t->topo_gid < 0 ) {
		TRY ( _h5t_open_topo_group ( f ) );
	}
	TRY ( (t->meshes_gid = _h5_open_group (
		       f,
		       t->topo_gid,
		       _h5t_meshes_grpnames[t->mesh_type] ) ) );

	return H5_SUCCESS;
}

h5_err_t
_h5t_open_mesh_group (
	h5_file_t * const f
	) {
	h5t_fdata_t *t = f->t;

	if ( t->meshes_gid < 0 ) {
		TRY ( _h5t_open_meshes_group ( f ) );
	}
	TRY ( ( t->mesh_gid = _h5_open_group (
			f,
			t->meshes_gid,
			t->mesh_name ) ) );
	return H5_SUCCESS;
}

/*
  If the value of parameter \c id is \c -1, a new mesh will be appended.
*/
h5_err_t
h5t_open_mesh (
	h5_file_t * const f,
	h5_id_t id,
	const h5_oid_t type
	) {
	h5t_fdata_t *t = f->t;

	TRY( _h5t_close_mesh ( f ) );

	if ( t->num_meshes < 0 ) {
		h5_size_t result = h5t_get_num_meshes ( f, type );
		t->num_meshes = ( result > 0 ? result : 0 );
	}
	if ( (id < -1) || (id >= t->num_meshes) ) {
		return HANDLE_H5_OUT_OF_RANGE_ERR( f, "mesh", id );
	}
	if ( id == -1 ) {  /* append new mesh */
		id = t->num_meshes;
	}
	t->mesh_type = type;
	snprintf ( t->mesh_name, sizeof (t->mesh_name), "%lld", id );

	switch( type ) {
	case H5_OID_TETRAHEDRON:
		t->dsinfo_elems.type_id = t->dtypes.h5_tet_t;
		t->methods = tet_funcs;
		break;
	case H5_OID_TRIANGLE:
		t->dsinfo_elems.type_id = t->dtypes.h5_triangle_t;
		t->methods = tri_funcs;
		break;
	default:
		return h5_error_internal ( f, __FILE__, __func__, __LINE__ );
	}

	TRY( _h5t_open_mesh_group ( f ) );

	t->cur_mesh = id;

	if ( id != t->num_meshes ) {	/* open existing */
		TRY ( _h5t_read_mesh ( f ) );

	} else {			/* append new */
		t->num_meshes++;
		t->mesh_changed = id;
		t->num_levels = 0;
	} 

	return H5_SUCCESS;
}


static h5_err_t
_release_elems (
	h5_file_t * const f
	) {
	h5t_fdata_t *t = f->t;
	TRY ( _h5_free ( f, t->elems.data ) );
	TRY( _h5_free ( f, t->num_elems ) );
	TRY ( _h5_free ( f, t->elems_ldta ) );
	TRY ( _h5_free ( f, t->num_elems_on_level ) );
	TRY ( _h5_free ( f, t->map_elem_g2l.items ) );

	return H5_SUCCESS;
}

static h5_err_t
_release_vertices (
	h5_file_t * const f
	) {
	h5t_fdata_t *t = f->t;
	TRY ( _h5_free ( f, t->vertices ) ); 
	TRY ( _h5_free ( f, t->vertices_data ) );
	TRY ( _h5_free ( f, t->num_vertices ) );
	TRY ( _h5_free ( f, t->map_vertex_g2l.items ) );

	return H5_SUCCESS;
}

static h5_err_t
_release_memory (
	h5_file_t * const f
	) {
	TRY ( _h5t_release_tags ( f ) );
	TRY ( _h5t_release_adjacencies ( f ) );
	TRY ( _release_elems ( f ) );
	TRY ( _release_vertices ( f ) );

	return H5_SUCCESS;
}

h5_err_t
_h5t_close_mesh (
	h5_file_t * const f
	) {
	TRY( _h5t_write_mesh ( f ) );
	TRY( _release_memory ( f ) );
	TRY( _init_fdata ( f ) );

	return H5_SUCCESS;
}

h5_err_t
h5t_open_level (
	h5_file_t * const f,
	const h5_id_t id
	) {
	struct h5t_fdata *t = f->t;

	if ( (id < 0) || (id >= t->num_levels) )
		return HANDLE_H5_OUT_OF_RANGE_ERR ( f, "Level", id );
	t->cur_level = id;

	return H5_SUCCESS;
}

herr_t
_hdf_set_chunk_property (
	h5_file_t * const f,
	hid_t plist,
	int ndims,
	const hsize_t * dim
	);


h5_err_t
_h5t_alloc_num_vertices (
	h5_file_t * const f,
	const h5_size_t num_vertices
	) {
	h5t_fdata_t *t = f->t;

	ssize_t size = num_vertices * sizeof ( t->vertices[0] );
	TRY ( t->vertices = _h5_alloc (	f, t->vertices, size ) );
	size = num_vertices * sizeof ( t->vertices_data[0] );
	TRY ( t->vertices_data = _h5_alloc (	f, t->vertices_data, size ) );
	TRY( _h5_alloc_idmap ( f, &t->map_vertex_g2l, num_vertices ) );
	TRY( _h5_alloc_idlist_items ( f, &t->sorted_lvertices, num_vertices ) );

	return H5_SUCCESS;
}

h5_err_t
_h5t_alloc_tris (
	h5_file_t * const f,
	const size_t cur,
	const size_t new
	) {
	h5t_fdata_t *t = f->t;
	const size_t nvertices = 3;

	/* alloc mem for elements */
	TRY ( t->elems.tris = _h5_alloc (
		      f,
		      t->elems.tris,
		      new * sizeof(t->elems.tris[0]) ) );
	memset (
		t->elems.tris + cur,
		-1,
		(new-cur) * sizeof(t->elems.tris[0]) );

	/* alloc mem for local data of elements */
	TRY ( t->elems_ldta = _h5_alloc (
		      f,
		      t->elems_ldta,
		      new * sizeof (t->elems_ldta[0]) ) );
	memset (
		t->elems_ldta + cur,
		-1,
		(new-cur) * sizeof (t->elems_ldta[0]) );

	/* alloc mem for local vertex IDs of elements */
	TRY ( t->elems_lvids = _h5_alloc (
		      f,
		      t->elems_lvids,
		      new * sizeof(t->elems_lvids[0]) * nvertices ) );
	memset (
		t->elems_lvids + cur * sizeof(t->elems_lvids[0]) * nvertices,
		-1,
		(new-cur) * sizeof(t->elems_lvids[0]) * nvertices );

	/* re-init pointer to local vertex id in local data structure */
	h5_id_t *p = t->elems_lvids;
	h5_id_t id;
	for ( id = 0; id < new; id++, p+=nvertices ) {
		t->elems_ldta[id].local_vids = p;
	}

	/* alloc mem for global to local ID mapping */
	TRY ( _h5_alloc_idmap ( f, &t->map_elem_g2l, new ) );

	return  H5_SUCCESS;
}

h5_err_t
_h5t_alloc_tets (
	h5_file_t * const f,
	const size_t cur,
	const size_t new
	) {
	h5t_fdata_t *t = f->t;
	const size_t nvertices = 4;

	/* alloc mem for elements */
	TRY ( t->elems.tets = _h5_alloc (
		      f,
		      t->elems.tets,
		      new * sizeof(t->elems.tets[0]) ) );
	memset (
		t->elems.tets + cur,
		-1,
		(new-cur) * sizeof(t->elems.tets[0]) );

	/* alloc mem for local data of elements */
	TRY ( t->elems_ldta = _h5_alloc (
		      f,
		      t->elems_ldta,
		      new * sizeof (t->elems_ldta[0]) ) );
	memset (
		t->elems_ldta + cur,
		-1,
		(new-cur) * sizeof (t->elems_ldta[0]) );

	/* alloc mem for local vertex IDs of elements */
	TRY ( t->elems_lvids = _h5_alloc (
		      f,
		      t->elems_lvids,
		      new * sizeof(t->elems_lvids[0]) * nvertices ) );
	memset (
		t->elems_lvids + cur * nvertices,
		-1,
		(new-cur) * sizeof(t->elems_lvids[0]) * nvertices );

	/* re-init pointer to local vertex id in local data structure */
	h5_id_t *p = t->elems_lvids;
	h5_id_t id;
	for ( id = 0; id < new; id++, p+=nvertices ) {
		t->elems_ldta[id].local_vids = p;
	}

	/* alloc mem for global to local ID mapping */
	TRY ( _h5_alloc_idmap ( f, &t->map_elem_g2l, new ) );

	return  H5_SUCCESS;
}

