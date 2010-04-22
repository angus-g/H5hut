#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>	/* va_arg - System dependent ?! */
#include <string.h>
#include <errno.h>
#include <fcntl.h>
#include <hdf5.h>

#include "h5core/h5_core.h"
#include "h5_core_private.h"

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
		TRY( h5tpriv_open_mesh_group ( f ) );
	}
	t->dsinfo_vertices.dims[0] = t->num_vertices[t->cur_level];
	TRY( h5priv_write_dataset_by_name (
		     f,
		     t->mesh_gid,
		     &t->dsinfo_vertices,
		     _open_space_all,
		     _open_space_all,
		     t->vertices ) );
	TRY( h5priv_write_dataset_by_name (
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
		TRY( h5tpriv_open_mesh_group ( f ) );
	}

	TRY ( h5priv_write_dataset_by_name (
		     f,
		     t->mesh_gid,
		     &t->dsinfo_elems,
		     _open_space_all,
		     _open_space_all,
		     t->elems.data ) );

	TRY ( h5priv_write_dataset_by_name (
		     f,
		     t->mesh_gid,
		     &t->dsinfo_num_elems,
		     _open_space_all,
		     _open_space_all,
		     t->num_elems ) );

	TRY ( h5priv_write_dataset_by_name (
		     f,
		     t->mesh_gid,
		     &t->dsinfo_num_elems_on_level,
		     _open_space_all,
		     _open_space_all,
		     t->num_elems_on_level ) );

	return H5_SUCCESS;
}

h5_err_t
h5tpriv_write_mesh (
	h5_file_t * f
	) {
	h5t_fdata_t *t = f->t;
	if ( t->mesh_changed ) {
		TRY( _write_vertices( f ) );
		TRY( _write_elems( f ) );
	}
	if ( t->mtags.changed ) { 
		TRY ( h5tpriv_write_mtags ( f ) );
	}

	return H5_SUCCESS;
}

static h5_size_t
_read_num_levels (
	h5_file_t * f
	) {
	h5t_fdata_t *t = f->t;
	hid_t dataset_id;
	hid_t diskspace_id;
	hssize_t size;

	if ( t->cur_mesh < 0 ) {
		return h5tpriv_error_undef_mesh ( f );
	}
	TRY ( dataset_id = h5priv_open_hdf5_dataset ( f, t->mesh_gid, "NumVertices" ) );
	TRY ( diskspace_id = h5priv_get_hdf5_dataset_space ( f, dataset_id ) );
	TRY ( size = h5priv_get_npoints_of_hdf5_dataspace ( f, diskspace_id ) );
	TRY ( h5priv_close_hdf5_dataspace( f, diskspace_id ) );

	t->num_levels = size;
	return size;
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

static h5_err_t
_read_num_vertices (
	h5_file_t * const f
	) {
	struct h5t_fdata *t = f->t;

 	if ( t->mesh_gid < 0 ) {
		TRY ( h5tpriv_open_mesh_group ( f ) );
	}
	ssize_t num_bytes = t->num_levels*sizeof ( t->num_vertices[0] );
	TRY ( t->num_vertices = h5priv_alloc ( f, t->num_vertices, num_bytes ) );
	TRY ( h5priv_read_dataset_by_name (
		f,
		t->mesh_gid,
		&t->dsinfo_num_vertices,
		_open_space_all,
		_open_space_all,
		t->num_vertices ) );

	return H5_SUCCESS;
}

static h5_err_t
_read_vertices (
	h5_file_t * f
	) {
	struct h5t_fdata *t = f->t;


	TRY( h5tpriv_alloc_num_vertices ( f, t->num_vertices[t->num_levels-1] ) );
	TRY( h5priv_read_dataset_by_name (
		     f,
		     t->mesh_gid,
		     &t->dsinfo_vertices,
		     _open_mem_space_vertices,
		     _open_file_space_vertices,
		     t->vertices ) );
	TRY ( h5tpriv_sort_vertices ( f ) );
	TRY ( h5tpriv_rebuild_global_2_local_map_of_vertices ( f ) );

	return H5_SUCCESS;
}



static h5_err_t
_read_num_elems (
	h5_file_t * const f
	) {
	struct h5t_fdata *t = f->t;

 	if ( t->mesh_gid < 0 ) {
		TRY( h5tpriv_open_mesh_group ( f ) );
	}
	size_t size = t->num_levels * sizeof ( t->num_elems[0] );
	TRY( t->num_elems = h5priv_alloc ( f, NULL, size ) );
	TRY( t->num_elems_on_level = h5priv_alloc ( f, NULL, size ) );
	TRY( h5priv_read_dataset_by_name (
		     f,
		     t->mesh_gid,
		     &t->dsinfo_num_elems,
		     _open_space_all,
		     _open_space_all,
		     t->num_elems ) );

	TRY( h5priv_read_dataset_by_name (
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

/*
  setup structure "elems_data" with local ids for each element:
  - translate the global vertex id's of each element to their
    local id's
  - translate the global parent id of each element to the
    corresponding local id.
*/
static h5_err_t
_build_elems_ldta (
	h5_file_t * const f
	) {
	h5t_fdata_t *t = f->t;
	h5_id_t local_eid = 0;
	h5_id_t num_elems = t->num_elems[t->num_levels-1];
	h5_id_t level_id = 0;

	void *elp = t->elems.data;
	h5_elem_t *el;
	h5_elem_ldta_t *el_data = t->elems_ldta;
	
	for ( local_eid=0;
	      local_eid < num_elems;
	      local_eid++, elp+=h5tpriv_sizeof_elem[t->mesh_type], el_data++ ) {
		el = (h5_elem_t*)elp;
		TRY( h5t_map_global_vids2local (
			     f,
			     el->global_vids,
			     t->mesh_type,
			     el_data->local_vids
			     ) );
		if ( el->global_parent_eid >= 0 )
			TRY ( el_data->local_parent_eid =
			      h5t_map_global_eid2local (
				      f,
				      el->global_parent_eid ) );
		
		if ( el->global_child_eid >= 0 )
			TRY ( el_data->local_child_eid =
			      h5t_map_global_eid2local (
				      f,
				      el->global_child_eid ) );
		
		if ( local_eid >= t->num_elems[level_id] ) {
			level_id++;
		}
		el_data->level_id = level_id;
	}
	return H5_SUCCESS;
}

static h5_err_t
_read_elems (
	h5_file_t * f
	) {
	h5t_fdata_t *t = f->t;

	TRY( (*t->methods._alloc_elems)( f, 0, t->num_elems[t->num_levels-1] ) );
	TRY( h5priv_read_dataset_by_name (
		     f,
		     t->mesh_gid,
		     &t->dsinfo_elems,
		     _open_mem_space_elems,
		     _open_file_space_elems,
		     t->elems.data ) );

	TRY ( h5tpriv_sort_elems ( f ) );
	TRY ( h5tpriv_rebuild_global_2_local_map_of_elems ( f ) );

	TRY ( _build_elems_ldta ( f ) );
	TRY ( (*t->methods.adjacency->rebuild_internal_structs)( f ) );
	return H5_SUCCESS;
}

static h5_err_t
_read_mtags (
	h5_file_t *const f
	) {
	h5t_fdata_t *t = f->t;
	TRY ( t->mtags.group_id = h5priv_open_group ( f, t->mesh_gid, "Tags" ) );
	return h5tpriv_read_tag_container ( f, &f->t->mtags );
}

h5_err_t
h5tpriv_read_mesh (
	h5_file_t *f
	) {
 	if ( f->t->mesh_gid < 0 ) {
		TRY( h5tpriv_open_mesh_group ( f ) );
	}
	TRY ( _read_num_levels ( f ) );
	TRY ( _read_num_vertices ( f ) );
	TRY ( _read_num_elems ( f ) );
	TRY ( _read_vertices ( f ) );
	TRY ( _read_elems ( f ) );
	TRY ( _read_mtags ( f ) );
	return H5_SUCCESS;
}
