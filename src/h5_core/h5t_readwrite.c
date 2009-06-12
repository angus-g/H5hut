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
_h5t_read_mesh (
	h5_file_t *f
	) {

	TRY ( _h5t_read_vertices ( f ) );
	TRY ( _h5t_read_elems ( f ) );

	return H5_SUCCESS;
}

