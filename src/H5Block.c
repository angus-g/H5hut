/*!
  \defgroup h5block_c_api H5Block C API
*/

/*!
  \internal

  \defgroup h5block_kernel H5Block Kernel
*/

/*!
  \internal

  \defgroup h5block_private H5Block Private
*/

/*!
  \note
  Different field sizes are allowed in the same time-step.

  \note
  The same layout can be used, if the size of the field matches the
  size of the layout.  If the size of the layout doesn't match the
  size of the field, an error will be indicated. 
 
  \note
  In write mode partitions are shrinked to make them non-overlaping. This 
  process may shrink the partitions more than required.

  \note
  In read-mode partitions may not cross boundaries. This means, if the grid
  size is (X, Y, Z), all partitions must fit into this grid.


  \todo
  check whether layout is reasonable

  API function names
*/


#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

#include <hdf5.h>

#include "h5_core/h5_core.h"
#include "h5_core/h5_core_private.h"
#include "H5Part.h"
#include "H5Block.h"

/********************** defining the layout **********************************/

/*!
  \note
  A partition must not be part of another partition.

  A partition must not divide another partition into two pieces.

  After handling the ghost zones, the partition must not be empty

  We must track the overall size somewhere. This is a good place to do it. (?)
*/

/*!
  \ingroup h5block_private

  \internal

  Normalize partition. In a normalized partition start coordinates are
  less or equal end coordinates.
*/
static void
_normalize_partition (
	struct h5b_partition *p		/*!< IN/OUT: partition */
	) {
	h5_size_t x;

	if ( p->i_start > p->i_end ) {
		x = p->i_start;
		p->i_start = p->i_end;
		p->i_end = x;
	}
	if ( p->j_start > p->j_end ) {
		x = p->j_start;
		p->j_start = p->j_end;
		p->j_end = x;
	}
	if ( p->k_start > p->k_end ) {
		x = p->k_start;
		p->k_start = p->k_end;
		p->k_end = x;
	}
}

/*!
  \ingroup h5block_private

  \internal

  Gather layout to all processors

  \par Errors
  \c H5_ERR_MPI	on MPI errors

  \return	H5_SUCCESS or error code
*/
#ifdef PARALLEL_IO
jfsdjk fjksdhf jkdsfjksdhfjkds kjf
static h5_err_t
_allgather (
	h5_file_t * const f		/*!< IN: file handle */
	) {
	struct h5b_partition *partition = &f->b->user_layout[f->myproc];
	struct h5b_partition *layout = f->b->user_layout;

	MPI_Datatype    partition_m;
	size_t n = sizeof (struct h5b_partition) / sizeof (h5_size_t);

	int mpi_err = MPI_Type_contiguous ( n, MPI_LONG, &partition_m );
	if ( mpi_err != MPI_SUCCESS ) return H5_ERR_MPI;
        mpi_err = MPI_Type_commit ( &partition_m );
	if ( mpi_err != MPI_SUCCESS ) return H5_ERR_MPI;
	TRY ( _h5_mpi_allgather (
		      f,
		      partition,
		      1,
		      partition_m,
		      layout,
		      1,
		      partition_m,
		      f->comm ) );

	return H5_SUCCESS;
}
#else
static h5_err_t
_allgather (
	const h5_file_t *f		/*!< IN: file handle */
	) {

	return H5_SUCCESS;
}
#endif

/*!
  \ingroup h5block_private

  \internal

  Get dimension sizes of block.  These informations are stored inside the
  block structure.
*/
static void
_get_dimension_sizes (
	h5_file_t *f			/*!< IN: file handle */
	) {
	int proc;
	struct h5b_fdata *b = f->b;
	struct h5b_partition *partition = b->user_layout;

	b->i_max = 0;
	b->j_max = 0;
	b->k_max = 0;

	for ( proc = 0; proc < f->nprocs; proc++, partition++ ) {
		if ( partition->i_end > b->i_max ) b->i_max = partition->i_end;
		if ( partition->j_end > b->j_max ) b->j_max = partition->j_end;
		if ( partition->k_end > b->k_max ) b->k_max = partition->k_end;
	}
}

#define _NO_GHOSTZONE(p,q) ( (p->i_end < q->i_start) \
  		        ||   (p->j_end < q->j_start) \
		        ||   (p->k_end < q->k_start) )


/*!
  \ingroup h5block_private

  \internal

  Check whether two partitions have a common ghost-zone.

  \return value != \c 0 if yes otherwise \c 0
*/
static int
_have_ghostzone (
	const struct h5b_partition *p,	/*!< IN: partition \c p */
	const struct h5b_partition *q	/*!< IN: partition \c q */
	) {
	return ( ! ( _NO_GHOSTZONE ( p, q ) || _NO_GHOSTZONE ( q, p ) ) );
}

/*!
  \ingroup h5block_private

  \internal

  Calculate "volume" of partition.

  \return volume
*/
static h5_int64_t
_volume_of_partition (
	const struct h5b_partition *p	/*!< IN: partition */
	) {
	return (p->i_end - p->i_start)
		* (p->j_end - p->j_start)
		* (p->k_end - p->k_start);

}

#define MIN( x, y ) ( (x) <= (y) ? (x) : (y) )  
#define MAX( x, y ) ( (x) >= (y) ? (x) : (y) )  

/*!
  \ingroup h5block_private

  \internal

  Calc volume of ghost-zone.

  \return volume
*/
static h5_int64_t
_volume_of_ghostzone (
	const struct h5b_partition *p, /*!< IN: ptr to first partition */
	const struct h5b_partition *q  /*!< IN: ptr to second partition */
	) {

	h5_int64_t dx = MIN ( p->i_end, q->i_end )
		- MAX ( p->i_start, q->i_start ) + 1;
	h5_int64_t dy = MIN ( p->j_end, q->j_end )
		- MAX ( p->j_start, q->j_start ) + 1;
	h5_int64_t dz = MIN ( p->k_end, q->k_end )
		- MAX ( p->k_start, q->k_start ) + 1;

	return dx * dy * dz;
}

/*!
  \ingroup h5block_private

  \internal

  Dissolve ghost-zone by moving the X coordinates.  Nothing will be changed
  if \c { p->i_start <= q->i_end <= p->i_end }.  In this case \c -1 will be
  returned.

  \return 0 or -1
*/
static h5_err_t
_dissolve_X_ghostzone (
	struct h5b_partition *p,	/*!< IN/OUT: ptr to first partition */
	struct h5b_partition *q		/*!< IN/OUT: ptr to second partition */
	) {

	if ( p->i_start > q->i_start )
		return _dissolve_X_ghostzone( q, p );

	if ( q->i_end <= p->i_end )  /* no dissolving		*/
		return -1;

	p->i_end = ( p->i_end + q->i_start ) >> 1;
	q->i_start = p->i_end + 1;
	return 0;
}

/*!
  \ingroup h5block_private

  \internal

  Dissolve ghost-zone by moving the Y coordinates.  Nothing will be changed
  if \c { p->j_start <= q->j_end <= p->j_end }.  In this case \c -1 will be
  returned.

  \return 0 or -1
*/
static h5_err_t
_dissolve_Y_ghostzone (
	struct h5b_partition *p,	/*!< IN/OUT: ptr to first partition */
	struct h5b_partition *q	/*!< IN/OUT: ptr to second partition */
	) {

	if ( p->j_start > q->j_start )
		return _dissolve_Y_ghostzone( q, p );

	if ( q->j_end <= p->j_end )    /* no dissolving		*/
		return -1;

	p->j_end = ( p->j_end + q->j_start ) >> 1;
	q->j_start = p->j_end + 1;
	return 0;
}

/*!
  \ingroup h5block_private

  \internal

  Dissolve ghost-zone by moving the Z coordinates.  Nothing will be changed
  if \c { p->k_start <= q->k_end <= p->k_end }.  In this case \c -1 will be
  returned.

  \return 0 or -1
*/
static h5_err_t
_dissolve_Z_ghostzone (
	struct h5b_partition *p,	/*!< IN/OUT: ptr to first partition */
	struct h5b_partition *q	/*!< IN/OUT: ptr to second partition */
	) {

	if ( p->k_start > q->k_start )
		return _dissolve_Z_ghostzone( q, p );

	if ( q->k_end <= p->k_end )    /* no dissolving		*/
		return -1;

	p->k_end = ( p->k_end + q->k_start ) >> 1;
	q->k_start = p->k_end + 1;
	return 0;
}

/*!
  \ingroup h5block_private

  \internal

  Dissolve ghost-zone for partitions \p and \q.

  Dissolving is done by moving either the X, Y or Z plane.  We never move
  more than one plane per partition.  Thus we always have three possibilities
  to dissolve the ghost-zone.  The "best" is the one with the largest
  remaining volume of the partitions.

  \par Errors
  \c H5_ERR_LAYOUT if dissolving gives a partition with empty "volume"

  \return H5_SUCCESS or error code.
*/
static h5_err_t
_dissolve_ghostzone (
	struct h5b_partition *p,	/*!< IN/OUT: ptr to first partition */
	struct h5b_partition *q		/*!< IN/OUT: ptr to second partition */
	) {

	struct h5b_partition p_;
	struct h5b_partition q_;
	struct h5b_partition p_best;
	struct h5b_partition q_best;
	h5_int64_t vol;
	h5_int64_t max_vol = 0;

	p_ = *p;
	q_ = *q;
	if ( _dissolve_X_ghostzone ( &p_, &q_ ) == 0 ) {
		vol = _volume_of_partition ( &p_ ) 
			+ _volume_of_partition ( &q_ );
		if ( vol > max_vol ) {
			max_vol = vol;
			p_best = p_;
			q_best = q_;
		}
	}

	p_ = *p;
	q_ = *q;
	if ( _dissolve_Y_ghostzone ( &p_, &q_ ) == 0 ) {
		vol = _volume_of_partition ( &p_ )
			+ _volume_of_partition ( &q_ );
		if ( vol > max_vol ) {
			max_vol = vol;
			p_best = p_;
			q_best = q_;
		}
	}
	p_ = *p;
	q_ = *q;

	if ( _dissolve_Z_ghostzone ( &p_, &q_ ) == 0 ) {
		vol = _volume_of_partition ( &p_ )
			+ _volume_of_partition ( &q_ );
		if ( vol > max_vol ) {
			max_vol = vol;
			p_best = p_;
			q_best = q_;
		}
	}
	if ( max_vol <= 0 ) {
		return H5_ERR_LAYOUT;
	}
	*p = p_best;
	*q = q_best;

	return H5_SUCCESS;
}

/*!
  \ingroup h5block_private

  \internal

  Dissolve all ghost-zones.

  Ghost-zone are dissolved in the order of their magnitude, largest first.

  \note
  Dissolving ghost-zones automaticaly is not trivial!  The implemented 
  algorithmn garanties, that there are no ghost-zones left and that we
  have the same result on all processors.
  But there may be zones which are not assigned to a partition any more.
  May be we should check this and return an error in this case.  Then
  the user have to decide to continue or to abort.

  \par Error Codes
  \c H5_ERR_NOMEM

  \return H5_SUCCESS or error code.
*/
static h5_err_t
_dissolve_ghostzones (
	h5_file_t *f	/*!< IN: file handle */
	) {

	struct h5b_fdata *b = f->b;
	struct h5b_partition *p;
	struct h5b_partition *q;
	int proc_p, proc_q;

	struct list {
		struct list *prev;
		struct list *next;
		struct h5b_partition *p;
		struct h5b_partition *q;
		h5_int64_t vol;
	} *p_begin, *p_el, *p_max, *p_end, *p_save;

	memcpy ( b->write_layout, b->user_layout,
		 f->nprocs * sizeof (*f->b->user_layout) );

	TRY ( p_begin = h5priv_alloc ( f, NULL, sizeof ( *p_begin ) ) );
	p_max = p_end = p_begin;
	memset ( p_begin, 0, sizeof ( *p_begin ) );

	for ( proc_p = 0, p = b->write_layout;
	      proc_p < f->nprocs-1;
	      proc_p++, p++ ) {
		for ( proc_q = proc_p+1, q = &b->write_layout[proc_q];
		      proc_q < f->nprocs;
		      proc_q++, q++ ) {

			if ( _have_ghostzone ( p, q ) ) {
				TRY ( p_el = h5priv_alloc (
					      f, NULL, sizeof ( *p_el ) ) );

				p_el->p = p;
				p_el->q = q;
				p_el->vol = _volume_of_ghostzone ( p, q );
				p_el->prev = p_end;
				p_el->next = NULL;
				
				if ( p_el->vol > p_max->vol )
					p_max = p_el;

				p_end->next = p_el;
				p_end = p_el;
			}
		}
	}
	while ( p_begin->next ) {
		if ( p_max->next ) p_max->next->prev = p_max->prev;
		p_max->prev->next = p_max->next;
		
		_dissolve_ghostzone ( p_max->p, p_max->q ); /* should check error */

		free ( p_max );
		p_el = p_max = p_begin->next;

		while ( p_el ) {
			if ( _have_ghostzone ( p_el->p, p_el->q ) ) {
				p_el->vol = _volume_of_ghostzone ( p_el->p, p_el->q );
				if ( p_el->vol > p_max->vol )
					p_max = p_el;
				p_el = p_el->next;
			} else {
				if ( p_el->next )
					p_el->next->prev = p_el->prev;
				p_el->prev->next = p_el->next;
				p_save = p_el->next;
				free ( p_el );
				p_el = p_save;
			}
		}

	}
	free ( p_begin );

	h5_debug ( f, "Layout defined by user:");
	for ( proc_p = 0, p = b->user_layout;
	      proc_p < f->nprocs;
	      proc_p++, p++ ) {
		h5_debug (
			f,
			"PROC[%d]: proc[%d]: %lld:%lld, %lld:%lld, %lld:%lld  ",
			f->myproc, proc_p,
			(long long)p->i_start, (long long)p->i_end,
			(long long)p->j_start, (long long)p->j_end,
			(long long)p->k_start, (long long)p->k_end );
	}

	h5_debug ( f, "Layout after dissolving ghost-zones:");
	for ( proc_p = 0, p = b->write_layout;
	      proc_p < f->nprocs;
	      proc_p++, p++ ) {
		h5_debug (
			f,
			"PROC[%d]: proc[%d]: %lld:%lld, %lld:%lld, %lld:%lld  ",
			f->myproc, proc_p,
			(long long)p->i_start, (long long)p->i_end,
			(long long)p->j_start, (long long)p->j_end,
			(long long)p->k_start, (long long)p->k_end );
	}
	return H5_SUCCESS;
}

/*!
  \ingroup h5block_private

  \internal

  Release previously defined hyperslab

  \par Errors
  \c H5_ERR_HDF5	if one of the dataspaces couldn't be closed

  \return H5_SUCCESS or error code
*/
h5_err_t
_release_hyperslab (
	h5_file_t *f			/*!< IN: file handle */
	) {
	struct h5b_fdata *b = f->b;

	TRY( h5priv_close_hdf5_dataspace( f, b->shape ) );
	f->b->shape = -1;

	TRY( h5priv_close_hdf5_dataspace( f, b->diskshape ) );
	f->b->diskshape = -1;

	TRY( h5priv_close_hdf5_dataspace( f, f->b->memshape ) );
	f->b->memshape = -1;

	return H5_SUCCESS;
}

/*!
  \ingroup h5block_c_api

  Define the field layout given the dense index space at the actual
  time step.

  \par Errors
  \c H5_ERR_MPI		on MPI errors
  \c H5_ERR_HDF5	on HDF5 errors
  \c H5_ERR_NOMEM	if we run out of memory
  
  \return \c H5PART_SUCCESS on success or error code
*/
h5_err_t
H5BlockDefine3DFieldLayout(
	h5_file_t *f,			/*!< IN: File handle		*/
	const h5_size_t i_start,	/*!< OUT: start index of \c i	*/ 
	const h5_size_t i_end,		/*!< OUT: end index of \c i	*/  
	const h5_size_t j_start,	/*!< OUT: start index of \c j	*/ 
	const h5_size_t j_end,		/*!< OUT: end index of \c j	*/ 
	const h5_size_t k_start,	/*!< OUT: start index of \c j	*/ 
	const h5_size_t k_end		/*!< OUT: end index of \c j	*/
	) {

	SET_FNAME ( f, __func__ );

	struct h5b_fdata *b = f->b;
	struct h5b_partition *p = &b->user_layout[f->myproc];
	p->i_start = i_start;
	p->i_end =   i_end;
	p->j_start = j_start;
	p->j_end =   j_end;
	p->k_start = k_start;
	p->k_end =   k_end;

	_normalize_partition( p );

	TRY (  _allgather ( f ) );

	_get_dimension_sizes ( f );

	h5_err_t herr = _dissolve_ghostzones ( f );
	if ( herr < 0 ) return HANDLE_H5_LAYOUT_ERR ( f );

	TRY( _release_hyperslab ( f ) );

	b->have_layout = 1;

	return H5_SUCCESS;
}

/*!
  \ingroup h5block_c_api

  Define the chunk dimensions and enable chunking in the underlying
  HDF5 dataset.

  \return \c H5_SUCCESS
*/
h5_err_t
H5BlockDefine3DChunk(
	h5_file_t *f,			/*!< IN: File handle */
	const h5_size_t *dims		/*!< IN: array containing
					  the chunk dimensions */
	) {

	SET_FNAME ( f, __func__ );

	struct h5b_fdata *b = f->b;

	b->chunk[0] = dims[2];
	b->chunk[1] = dims[1];
	b->chunk[2] = dims[0];

	return H5_SUCCESS;
}

/*!
  \ingroup h5block_c_api

  Return partition of processor \c proc as specified with
  \c H5BlockDefine3dLayout().

  \par Errors
  \c H5_ERR_INVAL if proc is invalid.

  \return \c H5_SUCCESS on success or error code
*/
h5_err_t
H5Block3dGetPartitionOfProc (
	h5_file_t *f,			/*!< IN: File handle */
	const h5_id_t proc,		/*!< IN: Processor to get partition from */
	h5_size_t *i_start,		/*!< OUT: start index of \c i	*/ 
	h5_size_t *i_end,		/*!< OUT: end index of \c i	*/  
	h5_size_t *j_start,		/*!< OUT: start index of \c j	*/ 
	h5_size_t *j_end,		/*!< OUT: end index of \c j	*/ 
	h5_size_t *k_start,		/*!< OUT: start index of \c k	*/ 
	h5_size_t *k_end		/*!< OUT: end index of \c k	*/ 
	) {

	SET_FNAME ( f, __func__ );
	CHECK_LAYOUT ( f );

	if ( ( proc < 0 ) || ( proc >= f->nprocs ) )
		return H5_ERR_INVAL;

	struct h5b_partition *p = &f->b->user_layout[(size_t)proc];

	*i_start = p->i_start;
	*i_end =   p->i_end;
	*j_start = p->j_start;
	*j_end =   p->j_end;
	*k_start = p->k_start;
	*k_end =   p->k_end;

	return H5_SUCCESS;
}

/*!
  \ingroup h5block_c_api

  Return reduced (ghost-zone free) partition of processor \c proc
  as specified with \c H5BlockDefine3dLayout().

  \par Errors
  \c H5_ERR_INVAL if proc is invalid.
  \c H5_ERR_LAYOUT if partitioning not yet defined.

  \return \c H5_SUCCESS on success or error code
*/
h5_err_t
H5Block3dGetReducedPartitionOfProc (
	h5_file_t *f,			/*!< IN: File handle */
	h5_id_t proc,			/*!< IN: Processor to get partition from */
	h5_size_t *i_start,		/*!< OUT: start index of \c i */ 
	h5_size_t *i_end,		/*!< OUT: end index of \c i */  
	h5_size_t *j_start,		/*!< OUT: start index of \c j */ 
	h5_size_t *j_end,		/*!< OUT: end index of \c j */ 
	h5_size_t *k_start,		/*!< OUT: start index of \c j */ 
	h5_size_t *k_end		/*!< OUT: end index of \c j */ 
	) {

	SET_FNAME ( f, __func__ );
	CHECK_LAYOUT ( f );

	if ( ( proc < 0 ) || ( proc >= f->nprocs ) )
		return H5_ERR_INVAL;

	struct h5b_partition *p = &f->b->write_layout[(size_t)proc];

	*i_start = p->i_start;
	*i_end =   p->i_end;
	*j_start = p->j_start;
	*j_end =   p->j_end;
	*k_start = p->k_start;
	*k_end =   p->k_end;

	return H5_SUCCESS;
}


/*!
  \ingroup h5block_c_api

  Returns the processor computing the reduced (ghostzone-free) 
  partition given by the coordinates \c i, \c j and \c k.

  \par Errors
  \c H5_ERR_LAYOUT if no partitioning defined
  \c H5_ERR_INVAL if given point is not in a partition

  \return \c processor id or error code
*/
h5_id_t
H5Block3dGetProcOf (
	h5_file_t *f,			/*!< IN: File handle */
	h5_size_t i,			/*!< IN: \c i coordinate */
	h5_size_t j,			/*!< IN: \c j coordinate */
	h5_size_t k			/*!< IN: \c k coordinate */
	) {

	SET_FNAME ( f, __func__ );
	CHECK_LAYOUT ( f );

	struct h5b_partition *layout = f->b->write_layout;
	int proc;

	for ( proc = 0; proc < f->nprocs; proc++, layout++ ) {
		if ( (layout->i_start <= i) && (i <= layout->i_end) &&
		     (layout->j_start <= j) && (j <= layout->j_end) &&
		     (layout->k_start <= k) && (k <= layout->k_end) ) 
			return (h5_id_t)proc;
	}
	
	return H5_ERR_INVAL;
}

/********************** helper functions for reading and writing *************/

/*!
  \ingroup h5block_private

  \internal

  \par Errors
  \c H5_ERR_HDF5	on HDF5 errors

  \return \c H5_SUCCESS or error code
*/
static h5_err_t
_open_block_group (
	h5_file_t * const f		/*!< IN: file handle */
	) {

	struct h5b_fdata *b = f->b;

	if ( (f->step_idx != b->step_idx) && (b->blockgroup > 0) ) {
		TRY( h5priv_close_hdf5_group( f, b->blockgroup ) );
		f->b->blockgroup = -1;
	}

	if ( b->blockgroup < 0 ) {
		TRY(
			b->blockgroup = h5priv_open_group (
				f,
				f->step_gid,
				H5BLOCK_GROUPNAME_BLOCK )
			);
	}
	b->step_idx = f->step_idx;

	return H5_SUCCESS;
}

/********************** functions for reading ********************************/

/*!
  \ingroup h5block_private

  \internal

*/
static h5_err_t
_have_object (
	const hid_t id,
	const char *name
	) {
	return (H5Gget_objinfo( id, name, 1, NULL ) >= 0 ? 1 : 0);
}

/*!
  \ingroup h5block_private

  \internal

  Open field with name \c name in current step. 

  \par Errors
  \c H5_ERR_NOENT	if field with name \c name not defined.
  \c H5_ERR_HDF5	on HDF5 errors.

  \return \c H5_SUCCESS or error code
*/
static h5_err_t
_open_field_group (
	h5_file_t *f,			/*!< IN: file handle */
	const char *name
	) {

	struct h5b_fdata *b = f->b;

	TRY( _open_block_group ( f ) );
	TRY( b->field_group_id = h5priv_open_group( f, b->blockgroup, name ) );

	return H5_SUCCESS;
}

/*!
  \ingroup h5block_private

  \internal

  Close current field group.

  \par Errors
  \c H5_ERR_HDF5

  \return \c H5_SUCCESS or error code
*/
h5_err_t
_close_field_group (
	h5_file_t *f			/*!< IN: file handle */
	) {

	return h5priv_close_hdf5_group ( f, f->b->field_group_id );
}

/*!
  \ingroup h5block_private

  \internal

  Set hyperslab for reading.

  \par Errors
  \c H5_ERR_HDF5	on HDF5 errors.
  \c H5_ERR_INVAL	if rank of dataset != 3.
  \c H5_ERR_LAYOUT	if field dimensions don't fit.

  \return \c H5_SUCCESS or error code
*/
static h5_err_t
_select_hyperslab_for_reading (
	h5_file_t *f,			/*!< IN: file handle */
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

	TRY( _release_hyperslab ( f ) );
	TRY ( b->diskshape = h5priv_get_hdf5_dataset_space ( f, dataset ) );
	TRY ( rank = h5priv_get_dims_of_hdf5_dataspace ( f, b->diskshape, NULL, NULL ) );
	if ( rank != 3 ) return HANDLE_H5_DATASET_RANK_ERR ( f, rank, 3 );
	TRY ( rank = h5priv_get_dims_of_hdf5_dataspace (
		      f, b->diskshape, field_dims, NULL ) );
	
	if ( (field_dims[0] < (hsize_t)b->k_max) ||
	     (field_dims[1] < (hsize_t)b->j_max) ||
	     (field_dims[2] < (hsize_t)b->i_max) ) return HANDLE_H5_LAYOUT_ERR ( f );

	h5_debug (
		f,
		"PROC[%d]: \n"
		"\tfield_dims: (%lld,%lld,%lld)",
		f->myproc,
		(long long)field_dims[2],
		(long long)field_dims[1],
		(long long)field_dims[0] );

	TRY ( b->diskshape = h5priv_create_hdf5_dataspace (
		      f, rank, field_dims,field_dims ) );
	TRY ( b->memshape = h5priv_create_hdf5_dataspace (
		      f, rank, part_dims, part_dims ) );
	TRY ( h5priv_select_hyperslab_of_hdf5_dataspace (
		      f,
		      b->diskshape,
		      H5S_SELECT_SET,
		      start,
		      stride,
		      part_dims,
		      NULL ) );

	h5_debug (
		f,
		"PROC[%d]: Select hyperslab: \n"
		"\tstart:  (%ld,%ld,%ld)\n"
		"\tstride: (%ld,%ld,%ld)\n"
		"\tdims:   (%ld,%ld,%ld)",
		f->myproc,
		(long)start[2],	(long)start[1],	(long)start[0],
		(long)stride[2], (long)stride[1], (long)stride[0],
		(long)part_dims[2], (long)part_dims[1], (long)part_dims[0]  );

	return H5_SUCCESS;
}

/*!
  \ingroup h5block_private

  \internal

  \par Errors
  \c H5_ERR_HDF5	on HDF5 errors
  \c H5_ERR_INVAL	if rank of dataset != 3.
  \c H5_ERR_LAYOUT	if field dimensions don't fit.

  \return \c H5_SUCCESS or error code
*/
static h5_err_t
_read_data (
	h5_file_t *f,			/*!< IN: file handle */
	const char *name,		/*!< IN: name of dataset to read */
	h5_float64_t *data,		/*!< OUT: ptr to read buffer */
	hid_t type      		/*!< IN: data type */
	) {

	struct h5b_fdata *b = f->b;
	hid_t dataset_id;
	TRY(
		dataset_id = h5priv_open_hdf5_dataset (
			f,
			b->field_group_id, name
			)
		);
	TRY( 
		_select_hyperslab_for_reading ( f, dataset_id ) );
	TRY( 
		h5priv_read_hdf5_dataset(
			f,
			dataset_id,
			type,
			f->b->memshape,
			f->b->diskshape,
			H5P_DEFAULT,
			data )
		);
	TRY(
		h5priv_close_hdf5_dataset( f, dataset_id )
		);

	return H5_SUCCESS;
}

h5_err_t
h5b_read_scalar_field (
	h5_file_t *f,			/*!< IN: file handle */
	const char *field_name,		/*!< IN: field name */
	const char *dataset_name,	/*!< IN: name of dataset to write */
	void *data,			/*!< OUT: ptr to read buffer */
	const hid_t type		/*!< IN: data type */
	) {
	CHECK_TIMEGROUP ( f );
	CHECK_LAYOUT ( f );

	TRY ( _open_field_group ( f, field_name ) );
	TRY ( _read_data ( f, dataset_name, data, type ) );
	TRY ( h5priv_close_hdf5_group ( f, f->b->field_group_id ) );

	return H5_SUCCESS;
}

/*!
  \ingroup h5block_c_api

  Read a 3-dimensional field \c name into the buffer starting at \c data from
  the current time-step using the defined field layout. Values are real valued
  scalars.

  You must use the FORTRAN indexing scheme to access items in \c data.

  \par Errors
  \c H5_ERR_LAYOUT	if no partitioning defined or field dimensions don't fit.\n
  \c H5_ERR_HDF5	on HDF5 errors\n
  \c H5_ERR_INVAL	if rank of dataset != 3.\n
  ...

  \return \c H5_SUCCESS or error code
*/
h5_err_t
H5Block3dReadScalarField (
	h5_file_t *f,			/*!< IN: file handle */
	const char *name,		/*!< IN: name of dataset to read */
	h5_float64_t *data		/*!< OUT: ptr to read buffer */
	) {

	SET_FNAME ( f, __func__ );
	return h5b_read_scalar_field ( f, name, "0", data, H5T_NATIVE_DOUBLE );
}

h5_err_t
H5Block3dReadScalarFieldFloat64 (
	h5_file_t *f,			/*!< IN: file handle */
	const char *name,		/*!< IN: name of dataset to read */
	h5_float64_t *data		/*!< OUT: ptr to read buffer */
	) {

	SET_FNAME ( f, __func__ );
	return h5b_read_scalar_field ( f, name, "0", data, H5T_NATIVE_DOUBLE );
}


h5_err_t
H5Block3dReadScalarFieldFloat32 (
	h5_file_t *f,			/*!< IN: file handle */
	const char *name,		/*!< IN: name of dataset to read */
	h5_float32_t *data		/*!< OUT: ptr to read buffer */
	) {

	SET_FNAME ( f, __func__ );
	return h5b_read_scalar_field ( f, name, "0", data, H5T_NATIVE_FLOAT );
}

h5_err_t
H5Block3dReadScalarFieldInt64 (
	h5_file_t *f,			/*!< IN: file handle */
	const char *name,		/*!< IN: name of dataset to read */
	h5_int64_t *data		/*!< OUT: ptr to read buffer */
	) {

	SET_FNAME ( f, __func__ );
	return h5b_read_scalar_field ( f, name, "0", data, H5T_NATIVE_INT64 );
}

h5_err_t
H5Block3dReadScalarFieldInt32 (
	h5_file_t *f,			/*!< IN: file handle */
	const char *name,		/*!< IN: name of dataset to read */
	h5_int32_t *data		/*!< OUT: ptr to read buffer */
	) {

	SET_FNAME ( f, __func__ );
	return h5b_read_scalar_field ( f, name, "0", data, H5T_NATIVE_INT32 );
}

h5_err_t
h5b_3d_read_3d_vectorfield (
	h5_file_t *f,			/*!< IN: file handle */
	const char *name,		/*!< IN: name of dataset to read */
	void *x_data,			/*!< OUT: ptr to read buffer X axis */
	void *y_data,			/*!< OUT: ptr to read buffer Y axis */
	void *z_data,			/*!< OUT: ptr to read buffer Z axis */
	const hid_t type		/*!< IN: data type */
	) {
	CHECK_TIMEGROUP ( f );
	CHECK_LAYOUT ( f );

	TRY ( _open_field_group ( f, name ) );
	TRY ( _read_data ( f, "0", x_data, type ) );
	TRY ( _read_data ( f, "1", y_data, type ) );
	TRY ( _read_data ( f, "2", z_data, type ) );
	TRY ( h5priv_close_hdf5_group ( f, f->b->field_group_id ) );

	return H5_SUCCESS;
}

/*!
  \ingroup h5block_c_api

  Read a 3-dimensional field \c name with 3-dimensional vectors as values 
  into the buffers starting at \c x_data, \c y_data and \c z_data from the
  current time-step using the defined field layout. Values are 3-dimensional
  vectors with real values.

  You must use the FORTRAN indexing scheme to access items in the buffers.

  \par Errors
  \c H5_ERR_HDF5	on HDF5 errors.\n
  \c H5_ERR_INVAL	if rank of dataset != 3.\n
  \c H5_ERR_LAYOUT	if field dimensions don't fit.\n

  \return \c H5_SUCCESS or error code
*/
h5_err_t
H5Block3dRead3dVectorField (
	h5_file_t *f,			/*!< IN: file handle */
	const char *name,		/*!< IN: name of dataset to read */
	h5_float64_t *x_data,		/*!< OUT: ptr to read buffer X axis */
	h5_float64_t *y_data,		/*!< OUT: ptr to read buffer Y axis */
	h5_float64_t *z_data		/*!< OUT: ptr to read buffer Z axis */
	) {

	SET_FNAME ( f, __func__ );
	return h5b_3d_read_3d_vectorfield (
		f,
		name,
		x_data,
		y_data,
		z_data,
		H5T_NATIVE_DOUBLE );
}

/********************** functions for writing ********************************/

/*!
  \ingroup h5block_private

  \internal

  \return \c H5_SUCCESS or error code
*/
static h5_err_t
_select_hyperslab_for_writing (
	h5_file_t *f		/*!< IN: file handle */
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


	TRY ( b->shape = h5priv_create_hdf5_dataspace (
		      f, rank, field_dims, field_dims ) );
	TRY ( b->diskshape = h5priv_create_hdf5_dataspace (
		      f, rank, field_dims,field_dims ) );

	h5_debug (
		f,
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

	TRY ( h5priv_select_hyperslab_of_hdf5_dataspace (
		      f,
		      b->diskshape,
		      H5S_SELECT_SET,
		      start,
		      stride,
		      part_dims,
		      NULL ) );

	field_dims[0] = q->k_end - q->k_start + 1;
	field_dims[1] = q->j_end - q->j_start + 1;
	field_dims[2] = q->i_end - q->i_start + 1;

	TRY ( b->memshape = h5priv_create_hdf5_dataspace (
		      f, rank, field_dims, field_dims ) );

	start[0] = p->k_start - q->k_start;
	start[1] = p->j_start - q->j_start;
	start[2] = p->i_start - q->i_start;

	h5_debug (
		f,
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

	TRY ( h5priv_select_hyperslab_of_hdf5_dataspace (
		      f,
		      b->memshape,
		      H5S_SELECT_SET,
		      start,
		      stride,
		      part_dims,
		      NULL ) );

	return H5_SUCCESS;
}

/*!
  \ingroup h5block_private

  \internal

  \return \c H5_SUCCESS or error code
*/
static h5_int64_t
_create_field_group (
	h5_file_t *f,			/*!< IN: file handle */
	const char *name		/*!< IN: name of field group to create */
	) {
	struct h5b_fdata *b = f->b;


	TRY ( _open_block_group ( f ) );
	TRY ( _select_hyperslab_for_writing ( f ) );
	if ( _have_object ( b->blockgroup, name ) )
		return  HANDLE_H5_GROUP_EXISTS_ERR ( f, name );
	TRY ( b->field_group_id = h5priv_open_group ( f, b->blockgroup, name ) );

	return H5_SUCCESS;
}	

/*!
  \ingroup h5block_private

  \internal

  \return \c H5_SUCCESS or error code
*/
static h5_err_t
_write_field_data (
	h5_file_t *f,			/*!< IN: file handle */
	const char *name,		/*!< IN: name of dataset to write */
	const h5_float64_t *data	/*!< IN: data to write */
	) {

	struct h5b_fdata *b = f->b;

	return h5_write_data (
		f,
		name,
		data,
		H5T_NATIVE_DOUBLE,
		b->field_group_id,
		b->memshape,
		b->diskshape );
}

/*!
  \ingroup h5block_c_api

  Write a 3-dimensional field \c name from the buffer starting at \c data 
  to the current time-step using the defined field layout. Values are real 
  valued scalars.

  You must use the FORTRAN indexing scheme to access items in \c data.

  \return \c H5_SUCCESS or error code
*/
h5_err_t
H5Block3dWriteScalarField (
	h5_file_t *f,			/*!< IN: file handle */
	const char *name,		/*!< IN: name of dataset to write */
	const h5_float64_t *data	/*!< IN: scalar data to write */
	) {

	SET_FNAME ( f, __func__ );
	CHECK_WRITABLE_MODE ( f );
	CHECK_TIMEGROUP ( f );
	CHECK_LAYOUT ( f );

	TRY ( _create_field_group ( f, name ) );
	TRY ( _write_field_data (
		f,
		"0",
		data ) );
	TRY ( h5priv_close_hdf5_group ( f, f->b->field_group_id ) );

	return H5_SUCCESS;
}

/*!
  \ingroup h5block_c_api
*/
/*!
  Write a 3-dimensional field \c name with 3-dimensional vectors as values 
  from the buffers starting at \c x_data, \c y_data and \c z_data to the
  current time-step using the defined field layout. Values are 3-dimensional
  vectors with real values.

  You must use the FORTRAN indexing scheme to access items in \c data.

  \return \c H5_SUCCESS or error code
*/
h5_err_t
H5Block3dWrite3dVectorField (
	h5_file_t *f,			/*!< IN: file handle */
	const char *name,		/*!< IN: name of dataset to write */
	const h5_float64_t *x_data,	/*!< IN: X axis data */
	const h5_float64_t *y_data,	/*!< IN: Y axis data */
	const h5_float64_t *z_data	/*!< IN: Z axis data */
	) {

	SET_FNAME ( f, __func__ );
	CHECK_WRITABLE_MODE ( f );
	CHECK_TIMEGROUP ( f );
	CHECK_LAYOUT ( f );

	TRY ( _create_field_group ( f, name ) );
	TRY ( _write_field_data ( f, "0", x_data ) );
	TRY ( _write_field_data ( f, "1", y_data ) );
	TRY ( _write_field_data ( f, "2", z_data ) );
	TRY ( h5priv_close_hdf5_group ( f, f->b->field_group_id ) );

	return H5_SUCCESS;
}

/********************** query information about available fields *************/

/*!
  \ingroup h5block_c_api

  Query number of fields in current time step.

  \return \c H5_SUCCESS or error code
*/
h5_id_t
H5BlockGetNumFields (
	h5_file_t *f			/*!< IN: file handle */
	) {

	SET_FNAME ( f, __func__ );
	CHECK_TIMEGROUP( f );

	if ( ! _have_object ( f->step_gid, H5BLOCK_GROUPNAME_BLOCK ) )
		return 0;

	return hdf5_get_num_objects (
		f->step_gid, H5BLOCK_GROUPNAME_BLOCK, H5G_GROUP );
}

/*!
  \ingroup h5block_private

  \internal

  \return \c H5_SUCCESS or error code
*/
static h5_err_t
_get_field_info (
	h5_file_t *f,			/*!< IN: file handle */
	const char *field_name,		/*!< IN: field name to get info about */
	h5_int64_t *grid_rank,		/*!< OUT: rank of grid */
	h5_int64_t *grid_dims,	/*!< OUT: dimensions of grid */
	h5_int64_t *field_dims	/*!< OUT: rank of field  (1 or 3) */
	) {

	hsize_t dims[16];
	h5_id_t i, j;
	hid_t group_id;
	hid_t dataset_id;
	hid_t dataspace_id;

	TRY ( _open_block_group ( f ) );
	TRY ( group_id = h5priv_open_hdf5_group( f, f->b->blockgroup, field_name ) );
	TRY ( dataset_id = h5priv_open_hdf5_dataset ( f, group_id, "0" ) );
 	TRY ( dataspace_id = h5priv_get_hdf5_dataset_space ( f, dataset_id ) );
	TRY ( *grid_rank = h5priv_get_dims_of_hdf5_dataspace (
		      f, dataspace_id, dims, NULL ) );

	for ( i = 0, j = *grid_rank-1; i < *grid_rank; i++, j-- )
		grid_dims[i] = (h5_int64_t)dims[j];

	TRY ( *field_dims = hdf5_get_num_objects (
		f->b->blockgroup,
		field_name,
		H5G_DATASET ) );
	TRY( h5priv_close_hdf5_dataspace( f, dataspace_id ) );
	TRY( h5priv_close_hdf5_dataset( f, dataset_id ) );
	TRY( h5priv_close_hdf5_group( f, group_id ) ); 

	return H5_SUCCESS;
}

/*!
  \ingroup h5block_c_api

  Get the name, rank and dimensions of the field specified by the
  index \c idx.

  This function can be used to retrieve all fields bound to the
  current time-step by looping from \c 0 to the number of fields
  minus one.  The number of fields bound to the current time-step
  can be queried by calling the function \c H5BlockGetNumFields().

  \return \c H5_SUCCESS or error code
*/
h5_err_t
H5BlockGetFieldInfo (
	h5_file_t *f,			/*!< IN: file handle */
	const h5_id_t idx,		/*!< IN: index of field */
	char *field_name,		/*!< OUT: field name */
	const h5_id_t len_field_name,	/*!< IN: buffer size */
	h5_int64_t *grid_rank,		/*!< OUT: grid rank */
	h5_int64_t *grid_dims,		/*!< OUT: grid dimensions */
	h5_int64_t *field_dims		/*!< OUT: field rank */
	) {

	SET_FNAME ( f, __func__ );
	CHECK_TIMEGROUP( f );

	TRY ( hdf5_get_object_name (
		f->step_gid,
		H5BLOCK_GROUPNAME_BLOCK,
		H5G_GROUP,
		idx,
		field_name,
		len_field_name ) );

	return _get_field_info (
		f, field_name, grid_rank, grid_dims, field_dims );
}

/*!
  \ingroup h5block_c_api

  Get the rank and dimensions of the field specified by its name.

  \return \c H5_SUCCESS or error code
*/
h5_int64_t
H5BlockGetFieldInfoByName (
	h5_file_t *f,				/*!< IN: file handle */
	const char *field_name,			/*!< IN: field name */
	h5_int64_t *grid_rank,			/*!< OUT: grid rank */
	h5_int64_t *grid_dims,			/*!< OUT: grid dimensions */
	h5_int64_t *field_dims			/*!< OUT: field rank */
	) {

	SET_FNAME ( f, __func__ );
	CHECK_TIMEGROUP( f );

	return _get_field_info (
		f, field_name, grid_rank, grid_dims, field_dims );
}

/********************** reading and writing attribute ************************/

/*!
  \ingroup h5block_private

  \internal

  \return \c H5_SUCCESS or error code
*/
static h5_int64_t
_write_field_attrib (
	h5_file_t *f,				/*!< IN: file handle */
	const char *field_name,			/*!< IN: field name */
	const char *attrib_name,		/*!< IN: attribute name */
	const hid_t attrib_type,		/*!< IN: attribute type */
	const void *attrib_value,		/*!< IN: attribute value */
	const h5_int64_t attrib_nelem	/*!< IN: number of elements */
	) {

	TRY ( _open_field_group ( f, field_name ) );
	TRY ( h5_write_attrib (
		f,
		f->b->field_group_id,
		attrib_name,
		attrib_type,
		attrib_value,
		attrib_nelem ) );
	TRY ( h5priv_close_hdf5_group ( f, f->b->field_group_id ) );

	return H5_SUCCESS;
}

/*!
  \ingroup h5block_c_api

  Write \c attrib_value with type \c attrib_type as attribute \c attrib_name
  to field \c field_name.

  \return \c H5_SUCCESS or error code
*/
h5_int64_t
H5BlockWriteFieldAttrib (
	h5_file_t *f,				/*!< IN: file handle */
	const char *field_name,			/*!< IN: field name */
	const char *attrib_name,		/*!< IN: attribute name */
	const h5_int64_t attrib_type,	/*!< IN: attribute type */
	const void *attrib_value,		/*!< IN: attribute value */
	const h5_int64_t attrib_nelem	/*!< IN: number of elements */
	) {

	SET_FNAME ( f, __func__ );
	CHECK_WRITABLE_MODE( f );
	CHECK_TIMEGROUP( f );

	return _write_field_attrib (
		f,
		field_name,
		attrib_name, (hid_t)attrib_type, attrib_value,
		(hid_t)attrib_nelem );
}

/*!
  \ingroup h5block_c_api

  Write string \c attrib_value as attribute \c attrib_name to field
  \c field_name..

  \return \c H5_SUCCESS or error code
*/
h5_int64_t
H5BlockWriteFieldAttribString (
	h5_file_t *f,				/*!< IN: file handle */
	const char *field_name,			/*!< IN: field name */
	const char *attrib_name,		/*!< IN: attribute name */
	const char *attrib_value		/*!< IN: attribute value */
	) {

	SET_FNAME ( f, __func__ );
	CHECK_WRITABLE_MODE( f );
	CHECK_TIMEGROUP( f );

	return _write_field_attrib (
		f,
		field_name,
		attrib_name, H5T_NATIVE_CHAR, attrib_value,
		strlen ( attrib_value ) + 1 );
}

/*!
  \ingroup h5block_c_api

  Query the number of attributes of field \c field_name.

  \return number of attributes or error code
*/
h5_int64_t
H5BlockGetNumFieldAttribs (
	h5_file_t *f,				/*!< IN: file handle */
	const char *field_name			/*<! IN: field name */
	) {
	h5_int64_t nattribs;

	SET_FNAME ( f, __func__ );
	CHECK_TIMEGROUP( f );

	TRY ( _open_field_group ( f, field_name ) );
	TRY ( nattribs = h5priv_get_num_hdf5_attribute ( f, f->b->field_group_id ) );
	TRY ( h5priv_close_hdf5_group ( f, f->b->field_group_id ) );

	return nattribs;
}


/*!
  \ingroup h5block_c_api

  Query information about a attribute given by index \c attrib_idx and
  field name \c field_name. The function returns the name of the attribute,
  the type of the attribute and the number of elements of this type.

  \return \c H5_SUCCESS or error code
*/
h5_int64_t
H5BlockGetFieldAttribInfo (
	h5_file_t *f,				/*!< IN: file handle */
	const char *field_name,			/*!< IN: field name */
	const h5_int64_t attrib_idx,	/*!< IN: attribute index */
	char *attrib_name,			/*!< OUT: attribute name */
	const h5_int64_t len_of_attrib_name,/*!< IN: buffer size */
	h5_int64_t *attrib_type,		/*!< OUT: attribute type */
	h5_int64_t *attrib_nelem		/*!< OUT: number of elements */
	) {

	SET_FNAME ( f, __func__ );
	CHECK_TIMEGROUP( f );

	TRY ( _open_field_group ( f, field_name ) );
	TRY ( h5_get_attrib_info (
		f,
		f->b->field_group_id,
		attrib_idx,
		attrib_name,
		len_of_attrib_name,
		attrib_type,
		attrib_nelem ) );
	TRY ( h5priv_close_hdf5_group ( f, f->b->field_group_id ) );

	return H5_SUCCESS;
}

/*!
  \ingroup h5block_private

  \internal

  Read attribute \c attrib_name of field \c field_name.

  \return \c H5_SUCCESS or error code
*/
static h5_int64_t
_read_field_attrib (
	h5_file_t *f,				/*!< IN: file handle */
	const char *field_name,			/*!< IN: field name */
	const char *attrib_name,		/*!< IN: attribute name */
	void *attrib_value			/*!< OUT: value */
	) {

	struct h5b_fdata *b = f->b;

	TRY ( _open_field_group ( f, field_name ) );
	TRY ( h5_read_attrib (
		f,
		b->field_group_id,
		attrib_name,
		attrib_value ) );
	TRY ( h5priv_close_hdf5_group ( f, f->b->field_group_id ) );

	return H5_SUCCESS;
}

/*!
  \ingroup h5block_c_api

  Read attribute \c attrib_name of field \c field_name.

  \return \c H5_SUCCESS or error code
*/
h5_int64_t
H5BlockReadFieldAttrib (
	h5_file_t *f,				/*!< IN: file handle */
	const char *field_name,			/*!< IN: field name */
	const char *attrib_name,		/*!< IN: attribute name */
	void *attrib_value			/*!< OUT: value */
	) {

	SET_FNAME ( f, __func__ );
	CHECK_TIMEGROUP( f );
	
	return _read_field_attrib (
		f, field_name, attrib_name, attrib_value );
}


#define H5BLOCK_FIELD_ORIGIN_NAME	"__Origin__"
#define H5BLOCK_FIELD_SPACING_NAME	"__Spacing__"

/*!
  \ingroup h5block_c_api

  Get field origin.

  \return \c H5_SUCCESS or error code
*/
h5_int64_t
H5Block3dGetFieldOrigin (
	h5_file_t *f,				/*!< IN: file handle */
	const char *field_name,			/*!< IN: field name */
	h5_float64_t *x_origin,		/*!< OUT: X origin */
	h5_float64_t *y_origin,		/*!< OUT: Y origin */
	h5_float64_t *z_origin		/*!< OUT: Z origin */
	) {

	SET_FNAME ( f, __func__ );
	CHECK_TIMEGROUP( f );

	h5_float64_t origin[3];

	TRY ( _read_field_attrib (
		f,
		field_name,
		H5BLOCK_FIELD_ORIGIN_NAME,
		origin ) );

	*x_origin = origin[0];
	*y_origin = origin[1];
	*z_origin = origin[2];

	return H5_SUCCESS;
}

/*!
  \ingroup h5block_c_api

  Set field origin.

  \return \c H5_SUCCESS or error code
*/
h5_int64_t
H5Block3dSetFieldOrigin (
	h5_file_t *f,				/*!< IN: file handle */
	const char *field_name,			/*!< IN: field name */
	const h5_float64_t x_origin,	/*!< IN: X origin */
	const h5_float64_t y_origin,	/*!< IN: Y origin */
	const h5_float64_t z_origin		/*!< IN: Z origin */
	) {

	SET_FNAME ( f, __func__ );
	CHECK_WRITABLE_MODE( f );
	CHECK_TIMEGROUP( f );

	h5_float64_t origin[3] = { x_origin, y_origin, z_origin };

	return _write_field_attrib (
		f,
		field_name,
		H5BLOCK_FIELD_ORIGIN_NAME,
		(hid_t)H5PART_FLOAT64, 
		origin,
		3 );
}

/*!
  \ingroup h5block_c_api

  Get field spacing for field \c field_name in the current time step.

  \return \c H5_SUCCESS or error code
*/
h5_int64_t
H5Block3dGetFieldSpacing (
	h5_file_t *f,				/*!< IN: file handle */
	const char *field_name,			/*!< IN: field name */
	h5_float64_t *x_spacing,		/*!< OUT: X spacing */
	h5_float64_t *y_spacing,		/*!< OUT: Y spacing */
	h5_float64_t *z_spacing		/*!< OUT: Z spacing */
	) {

	SET_FNAME ( f, __func__ );
	CHECK_TIMEGROUP( f );

	h5_float64_t spacing[3];

	TRY ( _read_field_attrib (
		f,
		field_name,
		H5BLOCK_FIELD_SPACING_NAME,
		spacing ) );

	*x_spacing = spacing[0];
	*y_spacing = spacing[1];
	*z_spacing = spacing[2];

	return H5_SUCCESS;
}

/*!
  \ingroup h5block_c_api

  Set field spacing for field \c field_name in the current time step.

  \return \c H5_SUCCESS or error code
*/
h5_int64_t
H5Block3dSetFieldSpacing (
	h5_file_t *f,				/*!< IN: file handle */
	const char *field_name,			/*!< IN: field name */
	const h5_float64_t x_spacing,	/*!< IN: X spacing */
	const h5_float64_t y_spacing,	/*!< IN: Y spacing */
	const h5_float64_t z_spacing	/*!< IN: Z spacing */
	) {

	SET_FNAME ( f, __func__ );
	CHECK_WRITABLE_MODE( f );
	CHECK_TIMEGROUP( f );

	h5_float64_t spacing[3] = { x_spacing, y_spacing, z_spacing };

	return _write_field_attrib (
		f,
		field_name,
		H5BLOCK_FIELD_SPACING_NAME,
		(hid_t)H5PART_FLOAT64, 
		spacing,
		3 );
}

/*!
  \ingroup h5block_c_api
*/
/*
  Checks whether the current step has field data or not.

  \return \c H5_SUCCESS if field data is available otherwise \c
  H5_ERR_NOENTRY.
*/
h5_int64_t
H5BlockHasFieldData (
	h5_file_t *f		/*!< IN: file handle */
	) {

	SET_FNAME ( f, __func__ );
	CHECK_TIMEGROUP( f );

	if ( ! _have_object ( f->step_gid, H5BLOCK_GROUPNAME_BLOCK ) ) {
		return H5_ERR_NOENTRY;
	}
	return H5_SUCCESS;
}
