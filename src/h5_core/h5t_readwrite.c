#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>	/* va_arg - System dependent ?! */
#include <string.h>
#include <errno.h>
#include <fcntl.h>
#include <hdf5.h>

#include "h5_core/h5_core.h"
#include "h5_core/h5_core_private.h"


h5_err_t
_h5t_write_obj (
	h5_file_t * f,
	const hid_t	group_id,
	const hsize_t  current_dims,
	const hsize_t  max_dims,
	const hid_t    type_id,
	const void * const object,
	const char * const dataset_name
	) {

	h5_err_t h5err = (h5_err_t)h5_write_dataset (
		f,
		group_id,
		dataset_name,
		type_id,
		H5S_ALL,
		H5S_ALL,
		object );
	if ( h5err < 0 ) return h5err;

	return H5_SUCCESS;
}

static h5_err_t
_write_vertices (
	h5_file_t * f
	) {
	struct h5t_fdata *t = f->t;
	h5_err_t h5err;

	if ( t->num_vertices <= 0 ) return H5_SUCCESS;  /* ???? */

 	if ( t->mesh_gid < 0 ) {
		h5err = _h5t_open_mesh_group ( f );
		if ( h5err < 0 ) return h5err;
	}

	hsize_t maxdim = H5S_UNLIMITED;
	h5err = _h5t_write_obj (
		f,
		t->mesh_gid,
		t->num_vertices[t->num_levels-1],
		maxdim,
		t->vertex_tid,
		(void*)t->vertices,
		"Vertices"
		);
	if ( h5err < 0 ) return h5err;
	return _h5t_write_obj (
		f,
		t->mesh_gid,
		t->num_levels,
		maxdim,
		H5T_NATIVE_INT32,
		(void*)t->num_vertices,
		"NumVertices"
		);
}

static h5_err_t
_write_entities (
	h5_file_t * f
	) {
	struct h5t_fdata *t = f->t;
	h5_err_t h5err;
	
	if ( t->num_entities <= 0 ) return H5_SUCCESS;

	if ( t->mesh_gid < 0 ) {
		h5err = _h5t_open_mesh_group ( f );
		if ( h5err < 0 ) return h5err;
	}

	hsize_t maxdim = H5S_UNLIMITED;
	h5err = _h5t_write_obj (
		f,
		t->mesh_gid,
		t->num_entities[t->num_levels-1],
		maxdim,
		t->entity_tid,
		(void*)t->entities.data,
		"Entities"
		);
	if ( h5err < 0 ) return h5err;

	h5err = _h5t_write_obj (
		f,
		t->mesh_gid,
		t->num_levels,
		maxdim,
		H5T_NATIVE_INT32,
		(void*)t->num_entities,
		"NumEntities"
		);
	if ( h5err < 0 ) return h5err;

	return _h5t_write_obj (
		f,
		t->mesh_gid,
		t->num_levels,
		maxdim,
		H5T_NATIVE_INT32,
		(void*)t->num_entities_on_level,
		"NumEntitiesOnLevel"
		);
}

h5_err_t
_h5t_write_mesh (
	h5_file_t * f
	) {
	struct h5t_fdata *t = f->t;
	h5_err_t h5err;

	if ( ! t->mesh_changed ) return 0;

	h5err = _write_vertices( f );
	if ( h5err < 0 ) return h5err;
	h5err = _write_entities( f );
	if ( h5err < 0 ) return h5err;

	return H5_SUCCESS;
}


h5_size_t
h5t_get_num_meshes (
	h5_file_t * f,
	const enum h5_oid type
	) {
	struct h5t_fdata *t = f->t;

	if ( t->topo_gid < 0 ) {
		h5_err_t h5err = _h5t_open_topo_group ( f );
		if ( h5err < 0 ) return h5err;
	}
	switch ( type ) {
	case H5_OID_TETRAHEDRON:
		return (h5_size_t)h5_get_num_objects (
			t->topo_gid,
			"TetMeshes",
			H5G_GROUP );
	case H5_OID_TRIANGLE:
		return (h5_size_t)h5_get_num_objects (
			t->topo_gid,
			"TriangleMeshes",
			H5G_GROUP );
	default:
		return -1;
	}
}

/*
 */
h5_size_t
h5t_get_num_levels (
	h5_file_t * f
	) {
	struct h5t_fdata *t = f->t;
	h5_err_t h5err;

	if ( t->num_levels >= 0 ) return t->num_levels;
	if ( t->cur_mesh < 0 ) {
		return _h5t_error_undef_mesh ( f );
	}
	if ( t->mesh_gid < 0 ) {
		h5err = _h5t_open_mesh_group ( f );
		if ( h5err < 0 ) return h5err;
	}
	hid_t dataset_id = H5Dopen ( t->mesh_gid, "NumVertices", H5P_DEFAULT );
	if ( dataset_id < 0 )
		return HANDLE_H5D_OPEN_ERR ( "NumVertices" );
	hid_t diskspace_id = H5Dget_space( dataset_id );
	if ( diskspace_id < 0 )
		return HANDLE_H5D_GET_SPACE_ERR;
	hssize_t size = H5Sget_simple_extent_npoints ( diskspace_id );
	if ( size < 0 )
		return HANDLE_H5S_GET_SIMPLE_EXTENT_NPOINTS_ERR;

	herr_t herr = H5Sclose ( diskspace_id );
	if ( herr < 0 )
		return HANDLE_H5S_CLOSE_ERR;
	t->num_levels = size;
	return size;
}


h5_id_t
h5t_get_level (
	h5_file_t * f
	) {
	struct h5t_fdata *t = f->t;
	return t->cur_level;
}

/*
  read everything with this function !?
*/
static h5_err_t
_read_dataset (
	h5_file_t * f,
	hid_t group_id,
	const char dataset_name[],
	hid_t type_id,
	hid_t (*open_mem_space)(h5_file_t*,hid_t),
	hid_t (*open_file_space)(h5_file_t*,hid_t),
	void * const data ) {

	hid_t dataset_id = H5Dopen ( group_id, dataset_name, H5P_DEFAULT );
	if ( dataset_id < 0 ) return HANDLE_H5D_OPEN_ERR ( dataset_name );

	hid_t mem_space_id = (*open_mem_space)( f, dataset_id );
	if ( mem_space_id < 0 ) return mem_space_id;
	hid_t file_space_id = (*open_file_space)( f, dataset_id );
	if ( file_space_id < 0 ) return file_space_id;

	herr_t herr = H5Dread (
		dataset_id,
		type_id,
		mem_space_id,
		file_space_id,
		f->xfer_prop,
		data );
	if ( herr < 0 )
		return HANDLE_H5D_READ_ERR ( h5_get_objname ( dataset_id ) );

	if ( file_space_id != H5S_ALL ) {
		herr = H5Sclose ( file_space_id );
		if ( herr < 0 ) return HANDLE_H5S_CLOSE_ERR;
	}

	if ( mem_space_id != H5S_ALL )
		herr = H5Sclose ( mem_space_id );
		if ( herr < 0 ) return HANDLE_H5S_CLOSE_ERR;

	herr = H5Dclose ( dataset_id );
	if ( herr < 0 ) return HANDLE_H5D_CLOSE_ERR;

	return H5_SUCCESS;
}

static hid_t
_open_mem_space_vertices (
	h5_file_t * f,
	hid_t dataset_id
	) {
	return H5S_ALL;
}

static hid_t
_open_file_space_vertices (
	h5_file_t * f,
	hid_t dataset_id
	) {
	return H5S_ALL;
}

static hid_t
_open_space_all (
	h5_file_t * f,
	hid_t dataset_id
	) {
	return H5S_ALL;
}

static h5_err_t
_read_num_vertices (
	h5_file_t * f
	) {
	h5_err_t h5err;
	struct h5t_fdata *t = f->t;

 	if ( t->mesh_gid < 0 ) {
		h5err = _h5t_open_mesh_group ( f );
		if ( h5err < 0 ) return h5err;
	}
	ssize_t num_bytes = t->num_levels*sizeof ( t->num_vertices[0] );
	h5_debug ( "Allocating %ld bytes.", num_bytes ); 
	t->num_vertices = realloc ( t->num_vertices, num_bytes );
	if ( t->num_vertices == NULL )
		return HANDLE_H5_NOMEM_ERR;
	h5err = _read_dataset (
		f,
		t->mesh_gid,
		"NumVertices",
		H5T_NATIVE_INT32,
		_open_space_all,
		_open_space_all,
		t->num_vertices );
	if ( h5err < 0 ) return h5err;

	return H5_SUCCESS;
}

h5_err_t
_h5t_read_vertices (
	h5_file_t * f
	) {
	h5_err_t h5err;
	struct h5t_fdata *t = f->t;

 	if ( t->mesh_gid < 0 ) {
		h5err = _h5t_open_mesh_group ( f );
		if ( h5err < 0 ) return h5err;
	}

	if ( t->num_vertices == NULL ) {
		h5err = _read_num_vertices ( f );
		if ( h5err < 0 ) return h5err;
	}

	h5err = _h5t_alloc_num_vertices ( f, t->num_vertices[t->num_levels-1] );
	if ( h5err < 0 ) return h5err;

	h5err = _read_dataset (
		f,
		t->mesh_gid,
		"Vertices",
		t->vertex_tid,
		_open_mem_space_vertices,
		_open_file_space_vertices,
		t->vertices );
	if ( h5err < 0 ) return h5err;

	h5_id_t local_vid = 0;
	for ( ; local_vid < t->num_vertices[t->num_levels-1]; local_vid++ ) {
		t->map_vertex_g2l.items[local_vid].global_id =
			t->vertices[local_vid].id; 
		t->map_vertex_g2l.items[local_vid].local_id = local_vid;
		t->map_vertex_g2l.num_items++;
	}
	_h5_sort_idmap ( &t->map_vertex_g2l );
	_h5t_sort_vertices ( f );

	return H5_SUCCESS;
}

h5_size_t
h5t_get_num_vertices_on_level (
	h5_file_t * f
	) {
	struct h5t_fdata *t = f->t;

	if ( t->cur_mesh < 0 ) {
		return _h5t_error_undef_mesh ( f );
	}
	if ( t->cur_level < 0 ) {
		return _h5t_error_undef_level( f );
	}
	if ( t->num_vertices == NULL ) {
		h5_err_t h5err = _read_num_vertices ( f );
		if ( h5err < 0 ) return h5err;
	}
	return t->num_vertices[t->cur_level];
}

h5_err_t
h5t_start_traverse_vertices (
	h5_file_t * f
	) {
	struct h5t_fdata *t = f->t;

	t->last_retrieved_vertex_id = -1;
	return H5_SUCCESS;
}

h5_id_t
h5t_traverse_vertices (
	h5_file_t * f,			/*!< file handle		*/
	h5_id_t * const id,		/*!< OUT: global vertex id	*/
	h5_float64_t P[3]		/*!< OUT: coordinates		*/
	) {
	struct h5t_fdata *t = f->t;

	if ( t->vertices == NULL ) {
		h5_err_t h5err = _h5t_read_vertices ( f );
		if ( h5err < 0 ) return h5err;
	}
	if ( t->last_retrieved_vertex_id+1 >= t->num_vertices[t->cur_level] ) {
		h5_debug ( "Traversing done!" );
		return 0;
	}
	h5_vertex_t *vertex = &t->vertices[++t->last_retrieved_vertex_id];
	*id = vertex->id;
	memcpy ( P, &vertex->P, sizeof ( vertex->P ) );

	return t->last_retrieved_vertex_id;
}


static h5_err_t
_read_num_entities (
	h5_file_t * f
	) {


	h5_err_t h5err;
	struct h5t_fdata *t = f->t;

 	if ( t->mesh_gid < 0 ) {
		h5err = _h5t_open_mesh_group ( f );
		if ( h5err < 0 ) return h5err;
	}
	ssize_t num_bytes = t->num_levels*sizeof ( t->num_entities[0] );
	h5_debug ( "Allocating %ld bytes.", num_bytes ); 
	t->num_entities = realloc ( t->num_entities, num_bytes );
	if ( t->num_entities == NULL )
		return HANDLE_H5_NOMEM_ERR;
	h5err = _read_dataset (
		f,
		t->mesh_gid,
		"NumEntities",
		H5T_NATIVE_INT32,
		_open_space_all,
		_open_space_all,
		t->num_entities );
	if ( h5err < 0 ) return h5err;

	return H5_SUCCESS;
}

static h5_err_t
_read_num_entities_on_level (
	h5_file_t * f
	) {

	h5_err_t h5err;
	struct h5t_fdata *t = f->t;

	if ( t->cur_level < 0 ) 
		return _h5t_error_undef_level( f );

 	if ( t->mesh_gid < 0 ) {
		h5err = _h5t_open_mesh_group ( f );
		if ( h5err < 0 ) return h5err;
	}
	ssize_t num_bytes = t->num_levels*sizeof ( t->num_entities_on_level[0] );
	h5_debug ( "Allocating %ld bytes.", num_bytes ); 
	t->num_entities_on_level = realloc ( t->num_entities_on_level, num_bytes );
	if ( t->num_entities_on_level == NULL )
		return HANDLE_H5_NOMEM_ERR;
	h5err = _read_dataset (
		f,
		t->mesh_gid,
		"NumEntitiesOnLevel",
		H5T_NATIVE_INT32,
		_open_space_all,
		_open_space_all,
		t->num_entities_on_level );
	if ( h5err < 0 ) return h5err;

	return H5_SUCCESS;
}

static hid_t
_open_mem_space_entities (
	h5_file_t * f,
	hid_t dataset_id
	) {
	return H5S_ALL;
}

static hid_t
_open_file_space_entities (
	h5_file_t * f,
	hid_t dataset_id
	) {
	return H5S_ALL;
}

h5_err_t
_h5t_read_entities (
	h5_file_t * f
	) {
	h5_err_t h5err;
	struct h5t_fdata *t = f->t;

 	if ( t->mesh_gid < 0 ) {
		h5err = _h5t_open_mesh_group ( f );
		if ( h5err < 0 ) return h5err;
	}

	if ( t->num_entities == NULL ) {
		h5err = _read_num_entities ( f );
		if ( h5err < 0 ) return h5err;
	}

	h5err = _h5t_alloc_num_entities ( f, 0, t->num_entities[t->num_levels-1] );

	h5err = _read_dataset (
		f,
		t->mesh_gid,
		"Entities",
		t->entity_tid,
		_open_mem_space_entities,
		_open_file_space_entities,
		t->entities.data );
	if ( h5err < 0 ) return h5err;

	/*
	  setup structure with local vertex ids
	 */
	h5_id_t local_eid = 0;
	h5_id_t num_entities = t->num_entities[t->num_levels-1];
	switch ( t->mesh_type ) {
	case H5_OID_TETRAHEDRON:
		for ( local_eid = 0; local_eid < num_entities; local_eid++ ) {
			h5err = h5t_map_global_vertex_ids2local (
				f,
				t->entities.tets[local_eid].vertex_ids,
				t->mesh_type,
				t->lentities.tets[local_eid].vertex_ids
				);
			if ( h5err < 0 ) return h5err;
		}
		break;
	case H5_OID_TRIANGLE:
		for ( local_eid = 0; local_eid < num_entities; local_eid++ ) {
			h5err = h5t_map_global_vertex_ids2local (
				f,
				t->entities.tris[local_eid].vertex_ids,
				t->mesh_type,
				t->lentities.tris[local_eid].vertex_ids
				);
			if ( h5err < 0 ) return h5err;
		}
		break;
	default:
		return -1;
	}
	
	return H5_SUCCESS;
}

h5_size_t
h5t_get_num_entities_on_level (
	h5_file_t * f
	) {
	struct h5t_fdata *t = f->t;

	if ( t->cur_mesh < 0 ) {
		return _h5t_error_undef_mesh ( f );
	}
	if ( t->cur_level < 0 ) {
		return _h5t_error_undef_level( f );
	}
	if ( t->num_entities_on_level == NULL ) {
		h5_err_t h5err = _read_num_entities_on_level ( f );
		if ( h5err < 0 ) return h5err;
	}
	return t->num_entities_on_level[t->cur_level];
}

h5_size_t
h5t_get_num_entities (
	h5_file_t * f
	) {
	struct h5t_fdata *t = f->t;

	if ( t->cur_mesh < 0 ) {
		return _h5t_error_undef_mesh ( f );
	}
	if ( t->num_entities == NULL ) {
		h5_err_t h5err = _read_num_entities ( f );
		if ( h5err < 0 ) return h5err;
	}
	return t->num_entities[t->num_levels-1];
}

h5_err_t
h5t_start_traverse_tets (
	h5_file_t * f
	) {
	struct h5t_fdata *t = f->t;

	switch ( t->mesh_type ) {
	case H5_OID_TRIANGLE: {
		return H5_ERR_INVAL;
	}
	case H5_OID_TETRAHEDRON: {
		t->last_retrieved_entity_id = -1;
		return H5_SUCCESS;
	}
	default:
		return H5_ERR_INTERNAL;
	}
}

h5_id_t
_traverse_tets (
	h5_file_t * f,
	h5_id_t * const id,		/*!< OUT: global tetrahedron id	*/
	h5_id_t * const parent_id,	/*!< OUT: global parent id
					     if level \c >0 else \c -1	*/
	h5_id_t ids[4]			/*!< OUT: tuple with vertex id's */
	) {
	struct h5t_fdata *t = f->t;

	if ( t->entities.data == NULL ) {
		h5_err_t h5err = _h5t_read_entities ( f );
		if ( h5err < 0 ) return h5err;
	}
	if ( t->last_retrieved_entity_id+1 >= t->num_entities[t->cur_level] ) {
		h5_debug ( "Traversing done!" );
		return 0;
	}
	h5_tetrahedron_t *tet = &t->entities.tets[++t->last_retrieved_entity_id];

	while ( (tet->refined_on_level != -1) &&
		(tet->refined_on_level <= t->cur_level) ){
		tet++;
		t->last_retrieved_entity_id++;
		if ( t->last_retrieved_entity_id >= t->num_entities[t->cur_level] ) {
			return h5_error_internal( __FILE__, __func__, __LINE__ );
		}
	}

	*id = tet->id;
	*parent_id = tet->parent_id;
	memcpy ( ids, &tet->vertex_ids, sizeof ( tet->vertex_ids ) );

	return t->last_retrieved_entity_id;
}

h5_id_t
h5t_traverse_tets (
	h5_file_t * f,
	h5_id_t * const id,		/*!< OUT: global tetrahedron id	*/
	h5_id_t * const parent_id,	/*!< OUT: global parent id
					     if level \c >0 else \c -1	*/
	h5_id_t ids[4]			/*!< OUT: tuple with vertex id's */
	) {
	struct h5t_fdata *t = f->t;

	switch ( t->mesh_type ) {
	case H5_OID_TRIANGLE: {
		return H5_ERR_INVAL;
	}
	case H5_OID_TETRAHEDRON: {
		return _traverse_tets ( f, id, parent_id, ids );
	}
	default:
		return h5_error_internal( __FILE__, __func__, __LINE__ );
	}
}


h5_err_t
h5t_start_traverse_triangles (
	h5_file_t * f
	) {
	struct h5t_fdata *t = f->t;

	switch ( t->mesh_type ) {
	case H5_OID_TRIANGLE: {
		t->last_retrieved_entity_id = -1;
		return H5_SUCCESS;
	}
	case H5_OID_TETRAHEDRON: {
		return h5_error_not_implemented( __FILE__, __func__, __LINE__ );
	}
	default:
		return h5_error_internal( __FILE__, __func__, __LINE__ );
	}
}


static h5_id_t
_traverse_triangles (
	h5_file_t * f,
	h5_id_t * const id,		/*!< OUT: global triangle id	*/
	h5_id_t * const parent_id,	/*!< OUT: global parent id
					     if level \c >0 else \c -1	*/
	h5_id_t ids[3]			/*!< OUT: tuple with vertex id's */
	) {
	struct h5t_fdata *t = f->t;

	if ( t->entities.data == NULL ) {
		h5_err_t h5err = _h5t_read_entities ( f );
		if ( h5err < 0 ) return h5err;
	}
	if ( t->last_retrieved_entity_id+1 >= t->num_entities[t->cur_level] ) {
		h5_debug ( "Traversing done!" );
		return 0;
	}
	h5_triangle_t *tri = &t->entities.tris[++t->last_retrieved_entity_id];

	while ( (tri->refined_on_level != -1) &&
		(tri->refined_on_level <= t->cur_level) ){
		tri++;
		t->last_retrieved_entity_id++;
		if ( t->last_retrieved_entity_id >= t->num_entities[t->cur_level] ) {
			return h5_error_internal( __FILE__, __func__, __LINE__ );
		}
	}

	*id = tri->id;
	*parent_id = tri->parent_id;
	memcpy ( ids, &tri->vertex_ids, sizeof ( tri->vertex_ids ) );

	return t->last_retrieved_entity_id;
}

h5_id_t
h5t_traverse_triangles (
	h5_file_t * f,
	h5_id_t * const id,		/*!< OUT: global triangle id	*/
	h5_id_t * const parent_id,	/*!< OUT: global parent id
					     if level \c >0 else \c -1	*/
	h5_id_t ids[3]			/*!< OUT: tuple with vertex id's */
	) {

	struct h5t_fdata *t = f->t;

	switch ( t->mesh_type ) {
	case H5_OID_TRIANGLE: {
		return _traverse_triangles ( f, id, parent_id, ids );
	}
	case H5_OID_TETRAHEDRON: {
		return h5_error_not_implemented( __FILE__, __func__, __LINE__ );
	}
	default:
		return h5_error_internal( __FILE__, __func__, __LINE__ );
	}
}

h5_err_t
_h5t_read_mesh (
	h5_file_t *f
	) {
	struct h5t_fdata *t = f->t;

	if ( t->vertices == NULL ) {
		h5_err_t h5err = _h5t_read_vertices ( f );
		if ( h5err < 0 ) return h5err;
	}

	if ( t->entities.data == NULL ) {
		h5_err_t h5err = _h5t_read_entities ( f );
		if ( h5err < 0 ) return h5err;
	}

	if ( t->sorted_lentities[0].items == NULL ) {
		_h5t_sort_entities ( f );
	}
	return H5_SUCCESS;
}

