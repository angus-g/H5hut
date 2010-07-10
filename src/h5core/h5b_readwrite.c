#include "h5core/h5_core.h"
#include "h5_core_private.h"

static h5_err_t
_create_block_group (
	h5_file_t *const f		/*!< IN: file handle */
	) {

	h5_err_t exists;
	TRY( exists = h5priv_hdf5_link_exists(f, f->step_gid, H5_BLOCKNAME) );

	if (exists > 0) {
		TRY( h5bpriv_open_block_group(f) );
	} else {
		TRY( f->b->block_gid = h5priv_create_hdf5_group(f,
					f->step_gid, H5_BLOCKNAME) );
	}

	return H5_SUCCESS;
}

static h5_err_t
_create_field_group (
	h5_file_t *const f,		/*!< IN: file handle */
	const char *name		/*!< IN: name of field group to create */
	) {

	struct h5b_fdata *b = f->b;

	TRY( _create_block_group(f) );

	char name2[H5_DATANAME_LEN];
	h5_normalize_dataset_name(f, name, name2);

	h5_err_t exists;
	TRY( exists = h5priv_hdf5_link_exists(f, b->block_gid, name2) );

	if (exists > 0) {
		TRY( h5bpriv_open_field_group(f, name2) );
	} else {
		TRY( b->field_gid = h5priv_create_hdf5_group(f,
						b->block_gid, name2) );
	}

	return H5_SUCCESS;
}	

static h5_err_t
_select_hyperslab_for_writing (
	h5_file_t *const f		/*!< IN: file handle */
	) {

	/*
	  re-use existing hyperslab
	*/
	if ( f->b->shape >= 0 ) return H5_SUCCESS;

	struct h5b_fdata *b = f->b;
	struct h5b_partition *p = &b->write_layout[f->myproc];
	struct h5b_partition *q = &b->user_layout[f->myproc];

	int rank = 3;
	
	hsize_t field_dims[3] = {
		b->k_max+1,
		b->j_max+1,
		b->i_max+1
	};

	hsize_t start[3] = {
		p->k_start,
		p->j_start,
		p->i_start
	};
	hsize_t stride[3] = { 1, 1, 1 };
	hsize_t part_dims[3] = {
		p->k_end - p->k_start + 1,
		p->j_end - p->j_start + 1,
		p->i_end - p->i_start + 1
	};


	TRY( b->shape = h5priv_create_hdf5_dataspace(f,
					rank, field_dims, NULL) );
	TRY( b->diskshape = h5priv_create_hdf5_dataspace(f,
					rank, field_dims, NULL) );
	h5_debug (f,
		"PROC[%d]: Select hyperslab on diskshape: \n"
		"\tstart:  (%lld,%lld,%lld)\n"
		"\tstride: (%lld,%lld,%lld)\n"
		"\tdims:   (%lld,%lld,%lld)",
		f->myproc,
		(long long)start[2],
		(long long)start[1],
		(long long)start[0],
		(long long)stride[2],
		(long long)stride[1],
		(long long)stride[0],
		(long long)part_dims[2],
		(long long)part_dims[1],
		(long long)part_dims[0]  );

	TRY( h5priv_select_hyperslab_of_hdf5_dataspace(f,
		b->diskshape,
		H5S_SELECT_SET,
		start,
		stride,
		part_dims,
		NULL) );

	field_dims[0] = q->k_end - q->k_start + 1;
	field_dims[1] = q->j_end - q->j_start + 1;
	field_dims[2] = q->i_end - q->i_start + 1;

	TRY( b->memshape = h5priv_create_hdf5_dataspace(f,
					rank, field_dims, NULL) );

	start[0] = p->k_start - q->k_start;
	start[1] = p->j_start - q->j_start;
	start[2] = p->i_start - q->i_start;

	h5_debug (f,
		"PROC[%d]: Select hyperslab on memshape: \n"
		"\tstart:  (%lld,%lld,%lld)\n"
		"\tstride: (%lld,%lld,%lld)\n"
		"\tdims:   (%lld,%lld,%lld)",
		f->myproc,
		(long long)start[2],
		(long long)start[1],
		(long long)start[0],
		(long long)stride[2],
		(long long)stride[1],
		(long long)stride[0],
		(long long)part_dims[2],
		(long long)part_dims[1],
		(long long)part_dims[0]  );

	TRY( h5priv_select_hyperslab_of_hdf5_dataspace(f,
		b->memshape,
		H5S_SELECT_SET,
		start,
		stride,
		part_dims,
		NULL) );

	return H5_SUCCESS;
}

static h5_err_t
_write_data (
	h5_file_t *const f,		/*!< IN: file handle */
	const char *field_name,		/*!< IN: name of field */
	const char *data_name,		/*!< IN: name of dataset */
	const void *data,		/*!< IN: data to write */
	const hid_t type		/*!< IN: data type */
	) {

	hid_t dataset;
	struct h5b_fdata *b = f->b;

	h5_err_t exists;
	TRY( exists = h5priv_hdf5_link_exists (f, b->field_gid, data_name) );
	if ( exists > 0 ) {
		TRY( dataset = h5priv_open_hdf5_dataset(f,
						b->field_gid, data_name) );
		hid_t type_file;
		TRY( type_file = h5priv_get_hdf5_dataset_type(f, dataset) );
		if ( type != type_file ) {
			return h5_error(f,
				H5_ERR_HDF5,
				"Field '%s' already has type '%s' "
				"but was written as '%s'.",
				field_name,
				h5priv_get_base_type_name(f, type_file),
				h5priv_get_base_type_name(f, type) );
		}
	} else {
		TRY( dataset = h5priv_create_hdf5_dataset(f,
					b->field_gid,
					data_name,
					type,
					b->shape,
					b->dcreate_prop) );
	}

	TRY( h5priv_write_hdf5_dataset(f,
		dataset,
		type,
		b->memshape,
		b->diskshape,
		f->xfer_prop,
		data) );

	TRY( h5priv_close_hdf5_dataset(f, dataset) );

	return H5_SUCCESS;
}

h5_err_t
h5b_write_scalar_data (
	h5_file_t *const f,		/*!< IN: file handle */
	const char *field_name,		/*!< IN: name of field */
	const void *data,		/*!< IN: data to write */
	const hid_t type		/*!< IN: data type */
	) {
	TRY( _create_field_group(f, field_name) );
	TRY( _select_hyperslab_for_writing(f) );
	TRY( _write_data(f, field_name, H5_BLOCKNAME_X, data, type) );
	return H5_SUCCESS;
}

h5_err_t
h5b_write_vector3d_data (
	h5_file_t *const f,		/*!< IN: file handle */
	const char *field_name,		/*!< IN: name of field */
	const void *xdata,		/*!< IN: x data to write */
	const void *ydata,		/*!< IN: y data to write */
	const void *zdata,		/*!< IN: z data to write */
	const hid_t type		/*!< IN: data type */
	) {
	TRY( _create_field_group(f, field_name) );
	TRY( _select_hyperslab_for_writing(f) );
	TRY( _write_data(f, field_name, H5_BLOCKNAME_X, xdata, type) );
	TRY( _write_data(f, field_name, H5_BLOCKNAME_Y, ydata, type) );
	TRY( _write_data(f, field_name, H5_BLOCKNAME_Z, zdata, type) );
	return H5_SUCCESS;
}

static h5_err_t
_select_hyperslab_for_reading (
	h5_file_t *const f,			/*!< IN: file handle */
	const hid_t dataset
	) {

	struct h5b_fdata *b = f->b;
	struct h5b_partition *p = &b->user_layout[f->myproc];
	int rank;
	hsize_t field_dims[3];
	hsize_t start[3] = {
		p->k_start,
		p->j_start,
		p->i_start };
	hsize_t stride[3] = { 1, 1, 1 };
	hsize_t part_dims[3] = {
		p->k_end - p->k_start + 1,
		p->j_end - p->j_start + 1,
		p->i_end - p->i_start + 1 };

	TRY( h5bpriv_release_hyperslab(f) );

 	TRY( b->diskshape = h5priv_get_hdf5_dataset_space(f, dataset) );

	TRY( rank = h5priv_get_dims_of_hdf5_dataspace(f,
		    			b->diskshape, field_dims, NULL) );
	if ( rank != 3 )
		return h5_error(f,
			H5_ERR_INVAL,
			"H5Block dataset has bad rank '%d' instead of rank 3! "
			"Is the file corrupt?",
			rank);
	
	if ( (field_dims[0] < (hsize_t)b->k_max) ||
	     (field_dims[1] < (hsize_t)b->j_max) ||
	     (field_dims[2] < (hsize_t)b->i_max) )
		return h5_error(f,
			H5_ERR_LAYOUT,
			"H5Block dataset has invalid layout. "
			"Is the file corrupt?");

	h5_debug (f,
		"PROC[%d]: field_dims: (%lld,%lld,%lld)",
		f->myproc,
		(long long)field_dims[2],
		(long long)field_dims[1],
		(long long)field_dims[0] );

	TRY( b->memshape = h5priv_create_hdf5_dataspace(f,
						rank, part_dims, NULL) );

	TRY( h5priv_select_hyperslab_of_hdf5_dataspace(f,
		b->diskshape,
		H5S_SELECT_SET,
		start,
		stride,
		part_dims,
		NULL) );

	h5_debug (f,
		"PROC[%d]: Select hyperslab: \n"
		"\tstart:  (%lld,%lld,%lld)\n"
		"\tstride: (%lld,%lld,%lld)\n"
		"\tdims:   (%lld,%lld,%lld)",
		f->myproc,
		(long long)start[2],
		(long long)start[1],
		(long long)start[0],
		(long long)stride[2],
		(long long)stride[1],
		(long long)stride[0],
		(long long)part_dims[2],
		(long long)part_dims[1],
		(long long)part_dims[0]  );

	return H5_SUCCESS;
}

static h5_err_t
_read_data (
	h5_file_t *const f,		/*!< IN: file handle */
	const char *field_name,		/*!< IN: name of field */
	const char *data_name,		/*!< IN: name of dataset */
	void *data,			/*!< OUT: ptr to read buffer */
	const hid_t type      		/*!< IN: data type */
	) {

	hid_t dataset;
	struct h5b_fdata *b = f->b;

	TRY( dataset = h5priv_open_hdf5_dataset(f, b->field_gid, data_name) );
	TRY( _select_hyperslab_for_reading(f, dataset) );
	TRY( h5priv_read_hdf5_dataset(f,
		dataset,
		type,
		f->b->memshape,
		f->b->diskshape,
		f->xfer_prop,
		data) );
	TRY( h5priv_close_hdf5_dataset(f, dataset) );

	return H5_SUCCESS;
}

h5_err_t
h5b_read_scalar_data (
	h5_file_t *const f,		/*!< IN: file handle */
	const char *field_name,		/*!< IN: name of field */
	void *data,			/*!< OUT: read bufer */
	const hid_t type		/*!< IN: data type */
	) {
	TRY( h5bpriv_open_field_group(f, field_name) );
	TRY( _read_data(f, field_name, H5_BLOCKNAME_X, data, type) );
	return H5_SUCCESS;
}

h5_err_t
h5b_read_vector3d_data (
	h5_file_t *const f,		/*!< IN: file handle */
	const char *field_name,		/*!< IN: name of field */
	void *xdata,			/*!< IN: x data to write */
	void *ydata,			/*!< IN: y data to write */
	void *zdata,			/*!< IN: z data to write */
	const hid_t type		/*!< IN: data type */
	) {
	TRY( h5bpriv_open_field_group(f, field_name) );
	TRY( _read_data(f, field_name, H5_BLOCKNAME_X, xdata, type) );
	TRY( _read_data(f, field_name, H5_BLOCKNAME_Y, ydata, type) );
	TRY( _read_data(f, field_name, H5_BLOCKNAME_Z, zdata, type) );
	return H5_SUCCESS;
}

