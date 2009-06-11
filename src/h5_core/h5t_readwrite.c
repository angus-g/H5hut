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
	t->dsinfo_vertices.dims[0] = t->num_vertices[t->cur_level];
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

	TRY ( _h5_write (
		     f,
		     t->mesh_gid,
		     &t->dsinfo_elems,
		     _open_space_all,
		     _open_space_all,
		     t->elems.data ) );

	TRY ( _h5_write (
		     f,
		     t->mesh_gid,
		     &t->dsinfo_num_elems,
		     _open_space_all,
		     _open_space_all,
		     t->num_elems ) );

	TRY ( _h5_write (
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
	h5t_fdata_t *t = f->t;
	if ( ! t->mesh_changed ) return 0;

	TRY( _write_vertices( f ) );
	TRY( _write_elems( f ) );

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
	struct h5t_fdata *t = f->t;

 	if ( t->mesh_gid < 0 ) {
		TRY ( _h5t_open_mesh_group ( f ) );
	}
	ssize_t num_bytes = t->num_levels*sizeof ( t->num_vertices[0] );
	TRY ( t->num_vertices = _h5_alloc ( f, t->num_vertices, num_bytes ) );
	TRY ( _h5_read (
		f,
		t->mesh_gid,
		&t->dsinfo_num_vertices,
		_open_space_all,
		_open_space_all,
		t->num_vertices ) );

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
	TRY( _h5_read (
		     f,
		     t->mesh_gid,
		     &t->dsinfo_vertices,
		     _open_mem_space_vertices,
		     _open_file_space_vertices,
		     t->vertices ) );
	TRY ( _h5t_sort_vertices ( f ) );
	TRY ( _h5t_rebuild_global_2_local_map_of_vertices ( f ) );

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
		TRY ( _h5t_read_mesh ( f ) );
	}
	if ( t->last_retrieved_vid+1 >= t->num_vertices[t->cur_level] ) {
		h5_debug ( f, "Traversing done!" );
		return 0;
	}
	h5_vertex_t *vertex = &t->vertices[++t->last_retrieved_vid];
	*id = vertex->global_vid;
	memcpy ( P, &vertex->P, sizeof ( vertex->P ) );

	return t->last_retrieved_vid;
}

h5_err_t
h5t_end_traverse_vertices (
	h5_file_t * f
	) {
	struct h5t_fdata *t = f->t;

	t->last_retrieved_vid = -1;
	return H5_SUCCESS;
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
	TRY( _h5_read (
		     f,
		     t->mesh_gid,
		     &t->dsinfo_num_elems,
		     _open_space_all,
		     _open_space_all,
		     t->num_elems ) );

	TRY( _h5_read (
		     f,
		     t->mesh_gid,
		     &t->dsinfo_num_elems_on_level,
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
	struct h5t_fdata *t = f->t;

 	if ( t->mesh_gid < 0 ) {
		TRY( _h5t_open_mesh_group ( f ) );
	}

	if ( t->num_elems == NULL ) {
		TRY( _h5t_read_num_elems ( f ) );
	}

	TRY( _h5t_alloc_num_elems ( f, 0, t->num_elems[t->num_levels-1] ) );
	TRY( _h5_read (
		     f,
		     t->mesh_gid,
		     &t->dsinfo_elems,
		     _open_mem_space_elems,
		     _open_file_space_elems,
		     t->elems.data ) );

	TRY ( _h5t_sort_elems ( f ) );
	TRY ( _h5t_rebuild_global_2_local_map_of_elems ( f ) );
	TRY ( _h5t_rebuild_elems_data ( f ) );
	TRY ( _h5t_rebuild_adj_data ( f ) );
	return H5_SUCCESS;
}

h5_err_t
h5t_start_traverse_elems (
	h5_file_t * f
	) {
	h5t_fdata_t *t = f->t;
	t->last_retrieved_eid = -1;
	return H5_SUCCESS;
}

/*!
  \param[in]	f		file handle
  \param[out]	global_eid	Global element id
  \param[out]	local_parent_id	Local parent id
  \param[out]	vids		Local vertex id
*/
h5_id_t
h5t_traverse_elems (
	h5_file_t * const f,
	h5_id_t * const global_eid,
	h5_id_t * const local_parent_id,
	h5_id_t *local_vids
	) {
	h5t_fdata_t *t = f->t;
	h5_element_t *elem;
	h5_element_data_t *elem_data;
	h5_id_t local_child_eid;
	h5_id_t refined_on_level = -1;

	if ( t->elems.data == NULL ) {
		TRY ( _h5t_read_elems ( f ) );
	}
	if ( t->last_retrieved_eid+1 >= t->num_elems[t->cur_level] ) {
		h5_debug ( f, "Traversing done!" );
		return 0;
	}

	/*
	  Skip elements which have been refined on a level less than the current one.
	*/
	do {
		switch ( t->mesh_type ) {
		case H5_OID_TETRAHEDRON:
			elem = (h5_element_t*)
				&t->elems.tets[++t->last_retrieved_eid];
			elem_data = (h5_element_data_t*)
				&t->elems_data.tets[t->last_retrieved_eid];
			local_child_eid = elem_data->local_child_eid;
			refined_on_level = ( local_child_eid >= 0 ) ?
				t->elems_data.tets[local_child_eid].level_id :
				t->cur_level+1;   /* this means "not refined" */
			break;
		case H5_OID_TRIANGLE:
			elem = (h5_element_t*)
				&t->elems.tris[++t->last_retrieved_eid];
			elem_data = (h5_element_data_t*)
				&t->elems_data.tris[t->last_retrieved_eid];
			local_child_eid = elem_data->local_child_eid;
			refined_on_level = ( elem_data->local_child_eid >= 0 ) ?
				t->elems_data.tris[local_child_eid].level_id :
				t->cur_level+1;   /* this means "not refined" */
			break;
		default:
			return h5_error_internal (
				f, __FILE__, __func__, __LINE__ );
		}
		if ( t->last_retrieved_eid >= t->num_elems[t->cur_level] ) {
			return h5_error_internal (
				f, __FILE__, __func__, __LINE__ );
		}
	}
	while ( refined_on_level <= t->cur_level );

	*global_eid = elem->global_eid;
	*local_parent_id = elem_data->local_parent_eid;
	memcpy (
		local_vids,
		&elem_data->local_vids,
		sizeof ( elem_data->local_vids[0] ) * t->mesh_type );

	return t->last_retrieved_eid;
}

h5_err_t
h5t_end_traverse_elems (
	h5_file_t * f
	) {
	h5t_fdata_t *t = f->t;
	t->last_retrieved_eid = -1;
	return H5_SUCCESS;
}

h5_err_t
_h5t_read_mesh (
	h5_file_t *f
	) {

	TRY ( _h5t_read_vertices ( f ) );
	TRY ( _h5t_read_elems ( f ) );

	return H5_SUCCESS;
}

