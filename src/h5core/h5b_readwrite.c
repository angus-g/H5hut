static h5_err_t
_create_block_group (
	const h5_file_t *const f		/*!< IN: file handle */
	) {

	herr_t herr;
	struct h5b_fdata *b = f->b;

	if ( b->blockgroup > 0 ) {
		herr = H5Gclose ( b->blockgroup );
		if ( herr < 0 ) return HANDLE_H5G_CLOSE_ERR;
		f->b->blockgroup = -1;
	}

	herr = H5Gcreate (
		f->timegroup,
		H5BLOCK_GROUPNAME_BLOCK,
#ifndef H5_USE_16_API
		H5P_DEFAULT,
		H5P_DEFAULT,
		H5P_DEFAULT
#else
		0
#endif
		);
	if ( herr < 0 ) return HANDLE_H5G_CREATE_ERR ( H5BLOCK_GROUPNAME_BLOCK );

	f->b->blockgroup = herr;
	return H5_SUCCESS;
}

/*!
  \ingroup h5block_private

  \internal

  \return \c H5_SUCCESS or error code
*/
h5_int64_t
_H5Block_create_field_group (
	h5_file_t *const f,			/*!< IN: file handle */
	const char *name		/*!< IN: name of field group to create */
	) {

	h5_int64_t h5err;
	struct h5b_fdata *b = f->b;


	if ( ! _H5Part_have_group ( f->timegroup, H5BLOCK_GROUPNAME_BLOCK ) ) {
		h5err = _create_block_group ( f );
	} else {
		h5err = _open_block_group ( f );
	}
	if ( h5err < 0 ) return h5err;

	h5err = _select_hyperslab_for_writing ( f );
	if ( h5err < 0 ) return h5err;

	if ( _H5Part_have_group ( b->blockgroup, name ) )
		return  HANDLE_H5PART_GROUP_EXISTS_ERR ( name );

	herr_t herr = H5Gcreate (
		b->blockgroup,
		name,
#ifndef H5_USE_16_API
		H5P_DEFAULT,
		H5P_DEFAULT,
		H5P_DEFAULT
#else
		0
#endif
		);
	if ( herr < 0 ) return HANDLE_H5G_CREATE_ERR ( name );
	b->field_group_id = herr;

	return H5_SUCCESS;
}	


h5_int64_t
_H5Block_select_hyperslab_for_reading (
	h5_file_t *const f,			/*!< IN: file handle */
	hid_t dataset
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

	h5_int64_t herr = _release_hyperslab ( f );
	if ( herr < 0 )	return HANDLE_H5S_CLOSE_ERR;

 	b->diskshape = H5Dget_space ( dataset );
	if ( b->diskshape < 0 ) return HANDLE_H5D_GET_SPACE_ERR;

	rank = H5Sget_simple_extent_dims ( b->diskshape, NULL, NULL );
	if ( rank < 0 )  return HANDLE_H5S_GET_SIMPLE_EXTENT_DIMS_ERR;
	if ( rank != 3 ) return HANDLE_H5PART_DATASET_RANK_ERR ( rank, 3 );

	rank = H5Sget_simple_extent_dims ( b->diskshape, field_dims, NULL );
	if ( rank < 0 )  return HANDLE_H5S_GET_SIMPLE_EXTENT_DIMS_ERR;
	
	if ( (field_dims[0] < (hsize_t)b->k_max) ||
	     (field_dims[1] < (hsize_t)b->j_max) ||
	     (field_dims[2] < (hsize_t)b->i_max) ) return HANDLE_H5PART_LAYOUT_ERR;

	_H5Part_print_debug (
		"PROC[%d]: field_dims: (%lld,%lld,%lld)",
		f->myproc,
		(long long)field_dims[2],
		(long long)field_dims[1],
		(long long)field_dims[0] );

	b->diskshape = H5Screate_simple ( rank, field_dims,field_dims );
	if ( b->diskshape < 0 )
		return HANDLE_H5S_CREATE_SIMPLE_3D_ERR ( field_dims );

	f->b->memshape = H5Screate_simple ( rank, part_dims, part_dims );
	if ( b->memshape < 0 )
		return HANDLE_H5S_CREATE_SIMPLE_3D_ERR ( part_dims );

	herr = H5Sselect_hyperslab (
		b->diskshape,
		H5S_SELECT_SET,
		start,
		stride,
		part_dims,
		NULL );
	if ( herr < 0 ) return HANDLE_H5S_SELECT_HYPERSLAB_ERR;

	_H5Part_print_debug (
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

/*!
  \ingroup h5block_private

  \internal

  \return \c H5_SUCCESS or error code
*/
h5_int64_t
_H5Block_read_data (
	h5_file_t *const f,			/*!< IN: file handle */
	const char *name,		/*!< IN: name of dataset to read */
	void *data,			/*!< OUT: ptr to read buffer */
	hid_t type      		/*!< IN: data type */
	) {

	h5_int64_t herr;
	struct h5b_fdata *b = f->b;

	hid_t dataset_id = H5Dopen ( b->field_group_id, name
#ifndef H5_USE_16_API
		, H5P_DEFAULT
#endif
		);
	if ( dataset_id < 0 ) return HANDLE_H5D_OPEN_ERR ( name );

	herr = _H5Block_select_hyperslab_for_reading ( f, dataset_id );
	if ( herr < 0 ) return herr;

#ifdef PARALLEL_IO
	herr = _H5Part_start_throttle ( f );
	if ( herr < 0 ) return herr;
#endif

	herr = H5Dread ( 
		dataset_id,
		type,
		f->b->memshape,
		f->b->diskshape,
		f->xfer_prop,
		data );
	if ( herr < 0 ) return HANDLE_H5D_READ_ERR ( name, f->timestep );

#ifdef PARALLEL_IO
	herr = _H5Part_end_throttle ( f );
	if ( herr < 0 ) return herr;
#endif

	herr = H5Dclose ( dataset_id );
	if ( herr < 0 ) return HANDLE_H5D_CLOSE_ERR;

	return H5_SUCCESS;
}

/********************** functions for writing ********************************/

/*!
  \ingroup h5block_private

  \internal

  \return \c H5_SUCCESS or error code
*/
static h5_int64_t
_select_hyperslab_for_writing (
	h5_file_t *const f		/*!< IN: file handle */
	) {

	/*
	  re-use existing hyperslab
	*/
	if ( f->b->shape >= 0 ) return H5_SUCCESS;

	herr_t herr;
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


	b->shape = H5Screate_simple ( rank, field_dims, field_dims );
	if ( b->shape < 0 )
		return HANDLE_H5S_CREATE_SIMPLE_3D_ERR ( field_dims );

	b->diskshape = H5Screate_simple ( rank, field_dims,field_dims );
	if ( b->diskshape < 0 )
		return HANDLE_H5S_CREATE_SIMPLE_3D_ERR ( field_dims );

	_H5Part_print_debug (
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

	herr = H5Sselect_hyperslab (
		b->diskshape,
		H5S_SELECT_SET,
		start,
		stride,
		part_dims,
		NULL );
	if ( herr < 0 ) return HANDLE_H5S_SELECT_HYPERSLAB_ERR;

	field_dims[0] = q->k_end - q->k_start + 1;
	field_dims[1] = q->j_end - q->j_start + 1;
	field_dims[2] = q->i_end - q->i_start + 1;

	f->b->memshape = H5Screate_simple ( rank, field_dims, field_dims );
	if ( b->memshape < 0 )
		return HANDLE_H5S_CREATE_SIMPLE_3D_ERR ( part_dims );

	start[0] = p->k_start - q->k_start;
	start[1] = p->j_start - q->j_start;
	start[2] = p->i_start - q->i_start;

	_H5Part_print_debug (
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

	herr = H5Sselect_hyperslab (
		b->memshape,
		H5S_SELECT_SET,
		start,
		stride,
		part_dims,
		NULL );
	if ( herr < 0 ) return HANDLE_H5S_SELECT_HYPERSLAB_ERR;

	return H5_SUCCESS;
}

/*!
  \ingroup h5block_private

  \internal

  \return \c H5_SUCCESS or error code
*/
h5_int64_t
_H5Block_write_data (
	h5_file_t *const f,			/*!< IN: file handle */
	const char *name,		/*!< IN: name of dataset to write */
	const void *data,		/*!< IN: data to write */
	const hid_t type		/*!< IN: data type */
	) {

	herr_t herr;
	hid_t dataset;
	struct h5b_fdata *b = f->b;

#ifndef H5_USE_16_API
	htri_t exists = H5Lexists ( b->field_group_id, name, H5P_DEFAULT );
	if ( exists > 0 ) return HANDLE_H5D_EXISTS_ERR ( name, f->timestep );
#endif

	dataset = H5Dcreate (
		b->field_group_id,
		name,
		type,
		b->shape, 
#ifndef H5_USE_16_API
		H5P_DEFAULT,
		b->create_prop,
		H5P_DEFAULT
#else
		b->create_prop
#endif
		);
	if ( dataset < 0 ) return HANDLE_H5D_CREATE_ERR ( name, f->timestep );

#ifdef PARALLEL_IO
	herr = _H5Part_start_throttle ( f );
	if ( herr < 0 ) return herr;
#endif

	herr = H5Dwrite ( 
		dataset,
		type,
		b->memshape,
		b->diskshape,
		f->xfer_prop,
		data );
	if ( herr < 0 ) return HANDLE_H5D_WRITE_ERR ( name, f->timestep );

#ifdef PARALLEL_IO
	herr = _H5Part_end_throttle ( f );
	if ( herr < 0 ) return herr;
#endif

	herr = H5Dclose ( dataset );
	if ( herr < 0 ) return HANDLE_H5D_CLOSE_ERR;

	return H5_SUCCESS;
}

