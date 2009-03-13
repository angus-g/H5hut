#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>	/* va_arg - System dependent ?! */
#include <string.h>
#include <errno.h>
#include <fcntl.h>
#include <hdf5.h>

#include "h5_core/h5_core.h"
#include "h5_core/h5_core_private.h"

static hid_t
_open_space_all (
	h5_file_t * const f,
	hid_t dataset_id
	) {
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
_write_vertices (
	h5_file_t * f
	) {
	struct h5t_fdata *t = f->t;

	if ( t->num_vertices <= 0 ) return H5_SUCCESS;  /* ???? */

 	if ( t->mesh_gid < 0 ) {
		TRY( _h5t_open_mesh_group ( f ) );
	}

	TRY( _h5_write (
		     f,
		     t->mesh_gid,
		     &t->dsinfo_vertices,
		     _open_space_all,
		     _open_space_all,
		     t->vertices ) );
	TRY( _h5_write (
		     f,
		     t->mesh_gid,
		     &t->dsinfo_num_vertices,
		     _open_space_all,
		     _open_space_all,
		     t->num_vertices ) );

	return H5_SUCCESS;
}

static h5_err_t
_write_elems (
	h5_file_t * f
	) {
	struct h5t_fdata *t = f->t;
	
	if ( t->num_elems <= 0 ) return H5_SUCCESS;

	if ( t->mesh_gid < 0 ) {
		TRY( _h5t_open_mesh_group ( f ) );
	}

	TRY( _h5_write (
		     f,
		     t->mesh_gid,
		     &t->dsinfo_elems,
		     _open_space_all,
		     _open_space_all,
		     t->elems.data ) );

	TRY( _h5_write (
		     f,
		     t->mesh_gid,
		     &t->dsinfo_num_elems,
		     _open_space_all,
		     _open_space_all,
		     t->num_elems ) );

	TRY( _h5_write (
		     f,
		     t->mesh_gid,
		     &t->dsinfo_num_elems_on_level,
		     _open_space_all,
		     _open_space_all,
		     t->num_elems_on_level ) );

	return H5_SUCCESS;
}

h5_err_t
_h5t_write_mesh (
	h5_file_t * f
	) {
	if ( ! f->t->mesh_changed ) return 0;

	TRY( _write_vertices( f ) );
	TRY( _write_elems( f ) );

	return H5_SUCCESS;
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

	hid_t dataset_id;
	hid_t mem_space_id;
	hid_t file_space_id;

	TRY( dataset_id = _h5_open_dataset ( f, group_id, dataset_name ) );
	TRY( mem_space_id = (*open_mem_space)( f, dataset_id ) );
	TRY( file_space_id = (*open_file_space)( f, dataset_id ) );
	TRY( _h5_read_dataset (
		     f,
		     dataset_id,
		     type_id,
		     mem_space_id,
		     file_space_id,
		     f->xfer_prop,
		     data ) );

	TRY( _h5_close_dataspace( f, file_space_id ) );
	TRY( _h5_close_dataspace( f, mem_space_id ) );
	TRY( _h5_close_dataset( f, dataset_id ) );

	return H5_SUCCESS;
}

static hid_t
_open_mem_space_vertices (
	h5_file_t * const f,
	hid_t dataset_id
	) {
	return H5S_ALL;
}

static hid_t
_open_file_space_vertices (
	h5_file_t * const f,
	hid_t dataset_id
	) {
	return H5S_ALL;
}

h5_err_t
_h5t_read_num_vertices (
	h5_file_t * const f
	) {
	h5_err_t h5err;
	struct h5t_fdata *t = f->t;

 	if ( t->mesh_gid < 0 ) {
		h5err = _h5t_open_mesh_group ( f );
		if ( h5err < 0 ) return h5err;
	}
	ssize_t num_bytes = t->num_levels*sizeof ( t->num_vertices[0] );
	h5_debug ( f, "Allocating %ld bytes.", num_bytes ); 
	t->num_vertices = realloc ( t->num_vertices, num_bytes );
	if ( t->num_vertices == NULL )
		return HANDLE_H5_NOMEM_ERR ( f );
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
	struct h5t_fdata *t = f->t;

 	if ( t->mesh_gid < 0 ) {
		TRY( _h5t_open_mesh_group ( f ) );
	}
	if ( t->num_vertices == NULL ) {
		TRY( _h5t_read_num_vertices ( f ) );
	}

	TRY( _h5t_alloc_num_vertices ( f, t->num_vertices[t->num_levels-1] ) );
	TRY( _read_dataset (
		     f,
		     t->mesh_gid,
		     "Vertices",
		     t->dtypes.h5_vertex_t,
		     _open_mem_space_vertices,
		     _open_file_space_vertices,
		     t->vertices ) );

	h5_id_t local_vid = 0;
	for ( ; local_vid < t->num_vertices[t->num_levels-1]; local_vid++ ) {
		t->map_vertex_g2l.items[local_vid].global_id =
			t->vertices[local_vid].vid; 
		t->map_vertex_g2l.items[local_vid].local_id = local_vid;
		t->map_vertex_g2l.num_items++;
	}
	_h5_sort_idmap ( &t->map_vertex_g2l );
	_h5t_sort_vertices ( f );

	return H5_SUCCESS;
}


h5_err_t
h5t_start_traverse_vertices (
	h5_file_t * f
	) {
	struct h5t_fdata *t = f->t;

	t->last_retrieved_vid = -1;
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
	if ( t->last_retrieved_vid+1 >= t->num_vertices[t->cur_level] ) {
		h5_debug ( f, "Traversing done!" );
		return 0;
	}
	h5_vertex_t *vertex = &t->vertices[++t->last_retrieved_vid];
	*id = vertex->vid;
	memcpy ( P, &vertex->P, sizeof ( vertex->P ) );

	return t->last_retrieved_vid;
}


h5_err_t
_h5t_read_num_elems (
	h5_file_t * const f
	) {
	struct h5t_fdata *t = f->t;

 	if ( t->mesh_gid < 0 ) {
		TRY( _h5t_open_mesh_group ( f ) );
	}
	size_t size = t->num_levels * sizeof ( t->num_elems[0] );
	TRY( t->num_elems = _h5_alloc ( f, NULL, size ) );
	TRY( t->num_elems_on_level = _h5_alloc ( f, NULL, size ) );
	TRY( _read_dataset (
		     f,
		     t->mesh_gid,
		     "NumElems",
		     H5T_NATIVE_INT32,
		     _open_space_all,
		     _open_space_all,
		     t->num_elems ) );

	TRY( _read_dataset (
		     f,
		     t->mesh_gid,
		     "NumElemsOnLevel",
		     H5T_NATIVE_INT32,
		     _open_space_all,
		     _open_space_all,
		     t->num_elems_on_level ) );
	
	return H5_SUCCESS;
}

static hid_t
_open_mem_space_elems (
	h5_file_t * f,
	hid_t dataset_id
	) {
	return H5S_ALL;
}

static hid_t
_open_file_space_elems (
	h5_file_t * f,
	hid_t dataset_id
	) {
	return H5S_ALL;
}

h5_err_t
_h5t_read_elems (
	h5_file_t * f
	) {
	h5_err_t h5err;
	struct h5t_fdata *t = f->t;

 	if ( t->mesh_gid < 0 ) {
		h5err = _h5t_open_mesh_group ( f );
		if ( h5err < 0 ) return h5err;
	}

	if ( t->num_elems == NULL ) {
		TRY( _h5t_read_num_elems ( f ) );
	}

	TRY( _h5t_alloc_num_elems ( f, 0, t->num_elems[t->num_levels-1] ) );
	TRY( _read_dataset (
		     f,
		     t->mesh_gid,
		     "Elems",
		     t->elem_tid,
		     _open_mem_space_elems,
		     _open_file_space_elems,
		     t->elems.data ) );

	/*
	  setup structure with local vertex ids
	 */
	h5_id_t local_eid = 0;
	h5_id_t num_elems = t->num_elems[t->num_levels-1];
	switch ( t->mesh_type ) {
	case H5_OID_TETRAHEDRON:
		for ( local_eid = 0; local_eid < num_elems; local_eid++ ) {
			TRY( h5t_map_global_vids2local (
				     f,
				     t->elems.tets[local_eid].vids,
				     t->mesh_type,
				     t->elems_data.tets[local_eid].vids
				     ) );
		}
		break;
	case H5_OID_TRIANGLE:
		for ( local_eid = 0; local_eid < num_elems; local_eid++ ) {
			TRY( h5t_map_global_vids2local (
				     f,
				     t->elems.tris[local_eid].vids,
				     t->mesh_type,
				     t->elems_data.tris[local_eid].vids
				     ) );
		}
		break;
	default:
		return -1;
	}
	
	return H5_SUCCESS;
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
		t->last_retrieved_eid = -1;
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

	if ( t->elems.data == NULL ) {
		h5_err_t h5err = _h5t_read_elems ( f );
		if ( h5err < 0 ) return h5err;
	}
	if ( t->last_retrieved_eid+1 >= t->num_elems[t->cur_level] ) {
		h5_debug ( f, "Traversing done!" );
		return 0;
	}
	h5_tetrahedron_t *tet = &t->elems.tets[++t->last_retrieved_eid];

	while ( (tet->refined_on_level != -1) &&
		(tet->refined_on_level <= t->cur_level) ){
		tet++;
		t->last_retrieved_eid++;
		if ( t->last_retrieved_eid >= t->num_elems[t->cur_level] ) {
			return h5_error_internal( f, __FILE__, __func__, __LINE__ );
		}
	}

	*id = tet->eid;
	*parent_id = tet->parent_eid;
	memcpy ( ids, &tet->vids, sizeof ( tet->vids ) );

	return t->last_retrieved_eid;
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
		return h5_error_internal( f, __FILE__, __func__, __LINE__ );
	}
}


h5_err_t
h5t_start_traverse_triangles (
	h5_file_t * f
	) {
	struct h5t_fdata *t = f->t;

	switch ( t->mesh_type ) {
	case H5_OID_TRIANGLE: {
		t->last_retrieved_eid = -1;
		return H5_SUCCESS;
	}
	case H5_OID_TETRAHEDRON: {
		return h5_error_not_implemented( f, __FILE__, __func__, __LINE__ );
	}
	default:
		return h5_error_internal( f, __FILE__, __func__, __LINE__ );
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

	if ( t->elems.data == NULL ) {
		h5_err_t h5err = _h5t_read_elems ( f );
		if ( h5err < 0 ) return h5err;
	}
	if ( t->last_retrieved_eid+1 >= t->num_elems[t->cur_level] ) {
		h5_debug ( f, "Traversing done!" );
		return 0;
	}
	h5_triangle_t *tri = &t->elems.tris[++t->last_retrieved_eid];

	while ( (tri->refined_on_level != -1) &&
		(tri->refined_on_level <= t->cur_level) ){
		tri++;
		t->last_retrieved_eid++;
		if ( t->last_retrieved_eid >= t->num_elems[t->cur_level] ) {
			return h5_error_internal( f, __FILE__, __func__, __LINE__ );
		}
	}

	*id = tri->eid;
	*parent_id = tri->parent_eid;
	memcpy ( ids, &tri->vids, sizeof ( tri->vids ) );

	return t->last_retrieved_eid;
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
		return h5_error_not_implemented( f, __FILE__, __func__, __LINE__ );
	}
	default:
		return h5_error_internal( f, __FILE__, __func__, __LINE__ );
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

	if ( t->elems.data == NULL ) {
		h5_err_t h5err = _h5t_read_elems ( f );
		if ( h5err < 0 ) return h5err;
	}

	if ( t->sorted_elems[0].items == NULL ) {
		_h5t_sort_elems ( f );
	}
	return H5_SUCCESS;
}

