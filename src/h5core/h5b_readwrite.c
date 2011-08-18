#include "h5core/h5_core.h"
#include "h5_core_private.h"

static h5_err_t
_select_hyperslab_for_writing (
	h5_file_t *const f		/*!< IN: file handle */
	) {
	H5_PRIV_FUNC_ENTER (h5_err_t, "f=%p", f);
	/*
	  re-use existing hyperslab
	*/
	if ( f->b->shape >= 0 ) return H5_SUCCESS;

	h5b_fdata_t *b = f->b;
	h5b_partition_t *p = b->write_layout;
	h5b_partition_t *q = b->user_layout;

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


	TRY (b->shape = hdf5_create_dataspace(rank, field_dims, NULL));
	TRY (b->diskshape = hdf5_create_dataspace(rank,field_dims,NULL));
	h5_debug (
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

	TRY( hdf5_select_hyperslab_of_dataspace(
		     b->diskshape,
		     H5S_SELECT_SET,
		     start,
		     stride,
		     part_dims,
		     NULL) );

	field_dims[0] = q->k_end - q->k_start + 1;
	field_dims[1] = q->j_end - q->j_start + 1;
	field_dims[2] = q->i_end - q->i_start + 1;

	TRY (b->memshape = hdf5_create_dataspace(rank,field_dims,NULL));

	start[0] = p->k_start - q->k_start;
	start[1] = p->j_start - q->j_start;
	start[2] = p->i_start - q->i_start;

	h5_debug (
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

	TRY (hdf5_select_hyperslab_of_dataspace(
		     b->memshape,
		     H5S_SELECT_SET,
		     start,
		     stride,
		     part_dims,
		     NULL));

	H5_PRIV_FUNC_RETURN (H5_SUCCESS);
}

static h5_err_t
_write_data (
	h5_file_t *const f,		/*!< IN: file handle */
	const char *field_name,		/*!< IN: name of field */
	const char *data_name,		/*!< IN: name of dataset */
	const void *data,		/*!< IN: data to write */
	const hid_t type		/*!< IN: data type */
	) {
	H5_PRIV_FUNC_ENTER (h5_err_t,
			    "f=%p, field_name=%s, data_name=%s, data=%p type=%d",
			    f, field_name, data_name, data, type);
	hid_t dataset;
	h5b_fdata_t *b = f->b;

	h5_err_t exists;
	TRY (exists = hdf5_link_exists (b->field_gid, data_name));
	if ( exists > 0 ) {
		TRY (dataset = hdf5_open_dataset (b->field_gid, data_name));
		hid_t type_file;
		TRY( type_file = hdf5_get_dataset_type (dataset) );
		if ( type != type_file ) {
			H5_PRIV_FUNC_LEAVE (h5_error(
				H5_ERR_HDF5,
				"Field '%s' already has type '%s' "
				"but was written as '%s'.",
				field_name,
				hdf5_get_type_name (type_file),
				hdf5_get_type_name (type)));
		}
	} else {
		TRY (dataset = hdf5_create_dataset(
			     b->field_gid,
			     data_name,
			     type,
			     b->shape,
			     b->dcreate_prop));
	}
#ifdef PARALLEL_IO
	TRY (h5_start_throttle (f));
#endif
	TRY (hdf5_write_dataset(
		     dataset,
		     type,
		     b->memshape,
		     b->diskshape,
		     f->xfer_prop,
		     data));
#ifdef PARALLEL_IO
	TRY (h5_end_throttle (f));
#endif
	TRY (hdf5_close_dataset (dataset));

	H5_PRIV_FUNC_RETURN (H5_SUCCESS);
}

h5_err_t
h5b_write_scalar_data (
	h5_file_t *const f,		/*!< IN: file handle */
	const char *field_name,		/*!< IN: name of field */
	const void *data,		/*!< IN: data to write */
	const hid_t type		/*!< IN: data type */
	) {
	H5_CORE_API_ENTER (h5_err_t,
			   "f=%p, field_name='%s', data=%p, type=%d",
			   f, field_name, data, type);
	CHECK_TIMEGROUP( f );
	CHECK_WRITABLE_MODE( f );
	CHECK_LAYOUT( f );
	TRY( h5bpriv_create_field_group(f, field_name) );
	TRY( _select_hyperslab_for_writing(f) );
	TRY( _write_data(f, field_name, H5_BLOCKNAME_X, data, type) );
	H5_CORE_API_RETURN (H5_SUCCESS);
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
	H5_CORE_API_ENTER (h5_err_t,
			   "f=%p, field_name='%s', "
			   "xdata=%p, "
			   "ydata=%p, "
			   "zdata=%p, "
			   "type=%d",
			    f, field_name, xdata, ydata, zdata, type);
	CHECK_TIMEGROUP( f );
	CHECK_WRITABLE_MODE( f );
	CHECK_LAYOUT( f );
	TRY( h5bpriv_create_field_group(f, field_name) );
	TRY( _select_hyperslab_for_writing(f) );
	TRY( _write_data(f, field_name, H5_BLOCKNAME_X, xdata, type) );
	TRY( _write_data(f, field_name, H5_BLOCKNAME_Y, ydata, type) );
	TRY( _write_data(f, field_name, H5_BLOCKNAME_Z, zdata, type) );
	H5_CORE_API_RETURN (H5_SUCCESS);
}

static h5_err_t
_select_hyperslab_for_reading (
	h5_file_t *const f,			/*!< IN: file handle */
	const hid_t dataset
	) {
	H5_PRIV_FUNC_ENTER (h5_err_t, "f=%p, dataset=%d", f, dataset);
	h5b_fdata_t *b = f->b;
	h5b_partition_t *p = b->user_layout;
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

 	TRY (b->diskshape = hdf5_get_dataset_space (dataset));

	TRY (rank = hdf5_get_dims_of_dataspace(b->diskshape, field_dims, NULL));
	if (rank != 3)
		H5_PRIV_FUNC_LEAVE (
			h5_error(
				H5_ERR_INVAL,
				"H5Block dataset has bad rank '%d' instead"
				" of rank 3! Is the file corrupt?",
				rank));
	
	if ( (field_dims[0] < (hsize_t)b->k_max) ||
	     (field_dims[1] < (hsize_t)b->j_max) ||
	     (field_dims[2] < (hsize_t)b->i_max) )
		H5_PRIV_FUNC_LEAVE (
			h5_error(
				H5_ERR_LAYOUT,
				"H5Block dataset has invalid layout. "
				"Is the file corrupt?"));

	h5_debug (
		"[%d]: field_dims: (%lld,%lld,%lld)",
		f->myproc,
		(long long)field_dims[2],
		(long long)field_dims[1],
		(long long)field_dims[0] );

	TRY (b->memshape = hdf5_create_dataspace (rank, part_dims, NULL));

	TRY (hdf5_select_hyperslab_of_dataspace(
		     b->diskshape,
		     H5S_SELECT_SET,
		     start,
		     stride,
		     part_dims,
		     NULL));

	h5_debug (
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

	H5_PRIV_FUNC_RETURN (H5_SUCCESS);
}

static h5_err_t
read_data (
	h5_file_t *const f,		/*!< IN: file handle */
	const char *dataset_name,	/*!< IN: name of dataset */
	void *data,			/*!< OUT: ptr to read buffer */
	const hid_t type      		/*!< IN: data type */
	) {
	H5_PRIV_FUNC_ENTER (h5_err_t,
			    "f=%p, dataset_name=%s, data=%p, type=%d",
			    f, dataset_name, data, type);
	hid_t dataset;
	h5b_fdata_t *b = f->b;

	TRY (dataset = hdf5_open_dataset (b->field_gid, dataset_name));
	TRY (_select_hyperslab_for_reading (f, dataset) );
#ifdef PARALLEL_IO
	TRY (h5_start_throttle (f));
#endif
	TRY (hdf5_read_dataset(
		     dataset,
		     type,
		     f->b->memshape,
		     f->b->diskshape,
		     f->xfer_prop,
		     data));
#ifdef PARALLEL_IO
	TRY (h5_end_throttle (f));
#endif
	TRY (hdf5_close_dataset(dataset));

	H5_PRIV_FUNC_RETURN (H5_SUCCESS);
}

h5_err_t
h5b_read_scalar_data (
	h5_file_t *const f,		/*!< IN: file handle */
	const char *field_name,		/*!< IN: name of field */
	void *data,			/*!< OUT: read bufer */
	const hid_t type		/*!< IN: data type */
	) {
	H5_CORE_API_ENTER (h5_err_t,
			   "f=%p, field_name='%s', data=%p, type=%d",
			   f, field_name, data, type);
	CHECK_TIMEGROUP( f );
	CHECK_LAYOUT( f );
	TRY( h5bpriv_open_field_group(f, field_name) );
	TRY( read_data(f, H5_BLOCKNAME_X, data, type) );
	H5_CORE_API_RETURN (H5_SUCCESS);
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
	H5_CORE_API_ENTER (h5_err_t,
			   "f=%p, field_name='%s', "
			   "xdata=%p, "
			   "ydata=%p, "
			   "zdata=%p, "
			   "type=%d",
			    f, field_name, xdata, ydata, zdata, type);
	CHECK_TIMEGROUP( f );
	CHECK_LAYOUT( f );
	TRY( h5bpriv_open_field_group(f, field_name) );
	TRY( read_data(f, H5_BLOCKNAME_X, xdata, type) );
	TRY( read_data(f, H5_BLOCKNAME_Y, ydata, type) );
	TRY( read_data(f, H5_BLOCKNAME_Z, zdata, type) );
	H5_CORE_API_RETURN (H5_SUCCESS);
}

