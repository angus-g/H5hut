#include <string.h>
#include <stdlib.h>

#include "h5core/h5_core.h"
#include "h5_core_private.h"

#define MIN( x, y ) ( (x) <= (y) ? (x) : (y) )  
#define MAX( x, y ) ( (x) >= (y) ? (x) : (y) )  

/*!
  \note
  A partition must not be part of another partition.

  A partition must not divide another partition into two pieces.

  After handling the ghost zones, the partition must not be empty

  We must track the overall size somewhere. This is a good place to do it. (?)
*/

static void
_normalize_partition (
	h5b_partition_t *const p	/*!< IN/OUT: partition */
	) {
	h5_size_t tmp;

	p->i_start = MAX(0, p->i_start);
	p->j_start = MAX(0, p->j_start);
	p->k_start = MAX(0, p->k_start);

	if ( p->i_start > p->i_end ) {
		tmp = p->i_start;
		p->i_start = p->i_end;
		p->i_end = tmp;
	}
	if ( p->j_start > p->j_end ) {
		tmp = p->j_start;
		p->j_start = p->j_end;
		p->j_end = tmp;
	}
	if ( p->k_start > p->k_end ) {
		tmp = p->k_start;
		p->k_start = p->k_end;
		p->k_end = tmp;
	}
}

#ifdef PARALLEL_IO
/* MLH: this could be improved with an MPI_Reduce and MAX operator...
 * but the user_layout array-of-structs would need to be a struct-of-arrays */
static void
_get_max_dimensions (
	h5_file_t *const f,
	h5b_partition_t *const user_layout
	) {
	int proc;
	h5b_fdata_t *b = f->b;
	h5b_partition_t *p = user_layout;

	b->i_max = 0;
	b->j_max = 0;
	b->k_max = 0;

	for ( proc = 0; proc < f->nprocs; proc++, p++ ) {
		if ( p->i_end > b->i_max ) b->i_max = p->i_end;
		if ( p->j_end > b->j_max ) b->j_max = p->j_end;
		if ( p->k_end > b->k_max ) b->k_max = p->k_end;
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
static inline int
have_ghostzone (
	const h5b_partition_t *const p,	/*!< IN: partition \c p */
	const h5b_partition_t *const q	/*!< IN: partition \c q */
	) {
	return ( ! ( _NO_GHOSTZONE ( p, q ) || _NO_GHOSTZONE ( q, p ) ) );
}

/*!
  \ingroup h5block_private

  \internal

  Calculate volume of partition.

  \return volume
*/
static inline h5_int64_t
volume_of_partition (
	const h5b_partition_t *const p	/*!< IN: partition */
	) {
	return (p->i_end - p->i_start)
		* (p->j_end - p->j_start)
		* (p->k_end - p->k_start);

}

/*!
  \ingroup h5block_private

  \internal

  Calc volume of ghost-zone.

  \return volume
*/
static inline h5_int64_t
volume_of_ghostzone (
	const h5b_partition_t *const p,	/*!< IN: ptr to first partition */
	const h5b_partition_t *const q	/*!< IN: ptr to second partition */
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

  \return H5_SUCCESS or -1
*/
static h5_int64_t
_dissolve_X_ghostzone (
	h5b_partition_t *const p,	/*!< IN/OUT: ptr to first partition */
	h5b_partition_t *const q	/*!< IN/OUT: ptr to second partition */
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

  \return H5_SUCCESS or -1
*/
static h5_int64_t
_dissolve_Y_ghostzone (
	h5b_partition_t *const p,	/*!< IN/OUT: ptr to first partition */
	h5b_partition_t *const q	/*!< IN/OUT: ptr to second partition */
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

  \return H5_SUCCESS or -1
*/
static h5_int64_t
_dissolve_Z_ghostzone (
	h5b_partition_t *const p,	/*!< IN/OUT: ptr to first partition */
	h5b_partition_t *const q	/*!< IN/OUT: ptr to second partition */
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

  \return H5_SUCCESS or error code.
*/
static h5_err_t
_dissolve_ghostzone (
	h5_file_t *const f,
	h5b_partition_t *const p,	/*!< IN/OUT: ptr to first partition */
	h5b_partition_t *const q	/*!< IN/OUT: ptr to second partition */
	) {
	UNUSED_ARGUMENT (f);
	h5b_partition_t p_;
	h5b_partition_t q_;
	h5b_partition_t p_best;
	h5b_partition_t q_best;
	h5_int64_t vol;
	h5_int64_t max_vol = 0;

	p_ = *p;
	q_ = *q;
	if ( _dissolve_X_ghostzone ( &p_, &q_ ) == 0 ) {
		vol = volume_of_partition ( &p_ ) 
			+ volume_of_partition ( &q_ );
		if ( vol > max_vol ) {
			max_vol = vol;
			p_best = p_;
			q_best = q_;
		}
	}

	p_ = *p;
	q_ = *q;
	if ( _dissolve_Y_ghostzone ( &p_, &q_ ) == 0 ) {
		vol = volume_of_partition ( &p_ )
			+ volume_of_partition ( &q_ );
		if ( vol > max_vol ) {
			max_vol = vol;
			p_best = p_;
			q_best = q_;
		}
	}
	p_ = *p;
	q_ = *q;

	if ( _dissolve_Z_ghostzone ( &p_, &q_ ) == 0 ) {
		vol = volume_of_partition ( &p_ )
			+ volume_of_partition ( &q_ );
		if ( vol > max_vol ) {
			max_vol = vol;
			p_best = p_;
			q_best = q_;
		}
	}
	if ( max_vol <= 0 ) {
		return h5_error (
			H5_ERR_LAYOUT,
			"Cannot dissolve ghostzones in specified layout!" );
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

  \b {Error Codes}
  \b H5PART_NOMEM_ERR

  \return H5_SUCCESS or error code.
*/
static inline h5_err_t
_dissolve_ghostzones (
	h5_file_t *const f,
	const h5b_partition_t *const user_layout,
	h5b_partition_t *const write_layout
	) {
	H5_PRIV_FUNC_ENTER (h5_err_t);
	h5b_partition_t *p;
	h5b_partition_t *q;
	int proc_p, proc_q;

	struct list {
		struct list *prev;
		struct list *next;
		h5b_partition_t *p;
		h5b_partition_t *q;
		h5_int64_t vol;
	} *p_begin, *p_el, *p_max, *p_end, *p_save;

	memcpy( write_layout, user_layout, f->nprocs*sizeof(h5b_partition_t) );

	TRY( p_begin = (struct list*)h5_alloc (NULL, sizeof(*p_begin)) );
	p_max = p_end = p_begin;
	
	memset( p_begin, 0, sizeof ( *p_begin ) );

	for ( proc_p = 0, p = write_layout;
		proc_p < f->nprocs-1;
		proc_p++, p++ ) {
		for ( proc_q = proc_p+1, q = &write_layout[proc_q];
			proc_q < f->nprocs;
			proc_q++, q++ ) {

			if ( have_ghostzone ( p, q ) ) {
				TRY( p_el = (struct list*)h5_alloc (NULL, sizeof(*p_el)) );

				p_el->p = p;
				p_el->q = q;
				p_el->vol = volume_of_ghostzone ( p, q );
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
		
		_dissolve_ghostzone ( f, p_max->p, p_max->q );

		h5_free (p_max);
		p_el = p_max = p_begin->next;

		while ( p_el ) {
			if ( have_ghostzone ( p_el->p, p_el->q ) ) {
				p_el->vol = volume_of_ghostzone ( p_el->p, p_el->q );
				if ( p_el->vol > p_max->vol )
					p_max = p_el;
				p_el = p_el->next;
			} else {
				if ( p_el->next )
					p_el->next->prev = p_el->prev;
				p_el->prev->next = p_el->next;
				p_save = p_el->next;
				h5_free (p_el);
				p_el = p_save;
			}
		}

	}
	h5_free (p_begin);
	H5_PRIV_FUNC_RETURN (H5_SUCCESS);
}
#endif

h5_err_t
h5bpriv_release_hyperslab (
	h5_file_t *const f			/*!< IN: file handle */
	) {
	H5_PRIV_API_ENTER1 (h5_err_t, "f=0x%p", f);
	if (f->b->shape > 0) {
		TRY (hdf5_close_dataspace (f->b->shape));
		f->b->shape = -1;
	}
	if (f->b->diskshape > 0) {
		TRY (hdf5_close_dataspace(f->b->diskshape));
		f->b->diskshape = -1;
	}
	if (f->b->memshape > 0) {
		TRY (hdf5_close_dataspace(f->b->memshape));
		f->b->memshape = -1;
	}
	H5_PRIV_API_RETURN (H5_SUCCESS);
}

h5_err_t
h5bpriv_open_block_group (
	h5_file_t *const f		/*!< IN: file handle */
	) {
	H5_PRIV_API_ENTER1 (h5_err_t, "f=0x%p", f);
	h5b_fdata_t *b = f->b;

	TRY (hdf5_close_group (b->block_gid));
	b->block_gid = hdf5_open_group (f->step_gid, H5_BLOCKNAME);
	if (f->b->block_gid < 0)
		H5_PRIV_API_LEAVE (h5_error(
			H5_ERR_INVAL,
			"Time step does not contain H5Block data!"));

	H5_PRIV_API_RETURN (H5_SUCCESS);
}

static h5_err_t
_create_block_group (
	h5_file_t *const f		/*!< IN: file handle */
	) {
	H5_PRIV_FUNC_ENTER1 (h5_err_t, "f=0x%p", f);
	h5_err_t exists;
	TRY (exists = hdf5_link_exists (f->step_gid, H5_BLOCKNAME));

	if (exists > 0) {
		TRY (h5bpriv_open_block_group(f));
	} else {
		TRY (hdf5_close_group(f->b->block_gid) );
		TRY (f->b->block_gid = hdf5_create_group(
			     f->step_gid, H5_BLOCKNAME) );
	}
	H5_PRIV_FUNC_RETURN (H5_SUCCESS);
}

h5_err_t
h5bpriv_have_field_group (
	h5_file_t *const f,			/*!< IN: file handle */
	const char *name
	) {
	H5_PRIV_API_ENTER2 (h5_err_t, "f=0x%p, name=\"%s\"", f, name);
	char name2[H5_DATANAME_LEN];
	h5_normalize_dataset_name (name, name2);

	TRY( h5bpriv_open_block_group(f) );
	H5_PRIV_API_RETURN (hdf5_link_exists(f->b->block_gid, name2));
}

h5_err_t
h5bpriv_open_field_group (
	h5_file_t *const f,			/*!< IN: file handle */
	const char *name
	) {
	H5_PRIV_API_ENTER2 (h5_err_t, "f=0x%p, name=\"%s\"", f, name);
	char name2[H5_DATANAME_LEN];
	h5_normalize_dataset_name (name, name2);
	
	TRY (hdf5_close_group (f->b->field_gid));
	TRY (h5bpriv_open_block_group (f));
	f->b->field_gid = hdf5_open_group (f->b->block_gid, name2);
	if (f->b->field_gid < 0)
		return h5_error(
			H5_ERR_INVAL,
			"Field '%s' does not exist!", name2);

	H5_PRIV_API_RETURN (H5_SUCCESS);
}

h5_err_t
h5bpriv_create_field_group (
	h5_file_t *const f,		/*!< IN: file handle */
	const char *name		/*!< IN: name of field group to create */
	) {
	H5_PRIV_API_ENTER2 (h5_err_t, "f=0x%p, name=\"%s\"", f, name);
	h5b_fdata_t *b = f->b;

	TRY( _create_block_group(f) );

	char name2[H5_DATANAME_LEN];
	h5_normalize_dataset_name (name, name2);

	h5_err_t exists;
	TRY (exists = hdf5_link_exists ( b->block_gid, name2));

	if (exists > 0) {
		TRY (h5bpriv_open_field_group (f, name2));
	} else {
		TRY (hdf5_close_group (f->b->field_gid) );
		TRY (b->field_gid = hdf5_create_group (b->block_gid, name2));
	}

	H5_CORE_API_RETURN (H5_SUCCESS);
}	

h5_int64_t
h5b_3d_has_view (
	h5_file_t *const f		/*!< IN: File handle		*/
	) {
	return f->b->have_layout;
}

h5_err_t
h5b_3d_set_view (
	h5_file_t *const f,		/*!< IN: File handle		*/
	const h5_size_t i_start,	/*!< IN: start index of \c i	*/ 
	const h5_size_t i_end,		/*!< IN: end index of \c i	*/  
	const h5_size_t j_start,	/*!< IN: start index of \c j	*/ 
	const h5_size_t j_end,		/*!< IN: end index of \c j	*/ 
	const h5_size_t k_start,	/*!< IN: start index of \c k	*/ 
	const h5_size_t k_end		/*!< IN: end index of \c k	*/
	) {
	H5_CORE_API_ENTER7 (h5_err_t,
			    "f=0X%p, "
			    "i_start=%llu, i_end=%llu, "
			    "j_start=%llu, j_end=%llu, "
			    "k_start=%llu, k_end=%llu",
			    f,
			    (long long unsigned)i_start, (long long unsigned)i_end,
			    (long long unsigned)j_start, (long long unsigned)j_end,
			    (long long unsigned)k_start, (long long unsigned)k_end);
	h5b_partition_t *p = f->b->user_layout;
	p->i_start = i_start;
	p->i_end =   i_end;
	p->j_start = j_start;
	p->j_end =   j_end;
	p->k_start = k_start;
	p->k_end =   k_end;
	_normalize_partition(p);

#ifdef PARALLEL_IO
	h5b_fdata_t *b = f->b;
	h5b_partition_t *user_layout;
	h5b_partition_t *write_layout;

	size_t size = f->nprocs * sizeof (h5b_partition_t);
	TRY( user_layout = h5_alloc (NULL, size) );
	TRY( write_layout = h5_alloc (NULL, size) );

	TRY( h5priv_mpi_allgather(
		p, 1, f->b->partition_mpi_t,
		user_layout, 1, f->b->partition_mpi_t, f->comm) );

	_get_max_dimensions(f, user_layout);

	TRY( _dissolve_ghostzones(f, user_layout, write_layout) );
	b->user_layout[0] = user_layout[f->myproc];
	b->write_layout[0] = write_layout[f->myproc];
	b->have_layout = 1;

	p = b->user_layout;
	h5_debug (
		"[%d] User layout: %lld:%lld, %lld:%lld, %lld:%lld",
		f->myproc,
		(long long)p->i_start, (long long)p->i_end,
		(long long)p->j_start, (long long)p->j_end,
		(long long)p->k_start, (long long)p->k_end );

	p = b->write_layout;
	h5_debug (
		"[%d] Ghost-zone layout: %lld:%lld, %lld:%lld, %lld:%lld",
		f->myproc,
		(long long)p->i_start, (long long)p->i_end,
		(long long)p->j_start, (long long)p->j_end,
		(long long)p->k_start, (long long)p->k_end );



	h5_free(user_layout);
	h5_free(write_layout);

	TRY( h5bpriv_release_hyperslab(f) );
#endif

	H5_CORE_API_RETURN (H5_SUCCESS);
}

h5_err_t
h5b_3d_get_view (
	h5_file_t *const f,		/*!< IN: File handle */
	h5_size_t *const i_start,	/*!< OUT: start index of \c i	*/ 
	h5_size_t *const i_end,		/*!< OUT: end index of \c i	*/  
	h5_size_t *const j_start,	/*!< OUT: start index of \c j	*/ 
	h5_size_t *const j_end,		/*!< OUT: end index of \c j	*/ 
	h5_size_t *const k_start,	/*!< OUT: start index of \c k	*/ 
	h5_size_t *const k_end		/*!< OUT: end index of \c k	*/ 
	) {
	H5_CORE_API_ENTER7 (h5_err_t,
			    "f=0X%p, "
			    "i_start=0x%p, i_end=0x%p, "
			    "j_start=0x%p, j_end=0x%p, "
			    "k_start=0x%p, k_end=0x%p",
			    f,
			    i_start, i_end,
			    j_start, j_end,
			    k_start, k_end);
	h5b_partition_t *p = f->b->user_layout;

	*i_start = p->i_start;
	*i_end =   p->i_end;
	*j_start = p->j_start;
	*j_end =   p->j_end;
	*k_start = p->k_start;
	*k_end =   p->k_end;

	H5_CORE_API_RETURN (H5_SUCCESS);
}

h5_err_t
h5b_3d_get_reduced_view (
	h5_file_t *const f,		/*!< IN: File handle */
	h5_size_t *const i_start,	/*!< OUT: start index of \c i	*/ 
	h5_size_t *const i_end,		/*!< OUT: end index of \c i	*/  
	h5_size_t *const j_start,	/*!< OUT: start index of \c j	*/ 
	h5_size_t *const j_end,		/*!< OUT: end index of \c j	*/ 
	h5_size_t *const k_start,	/*!< OUT: start index of \c k	*/ 
	h5_size_t *const k_end		/*!< OUT: end index of \c k	*/ 
	) {
	H5_CORE_API_ENTER7 (h5_err_t,
			    "f=0X%p, "
			    "i_start=0x%p, i_end=0x%p, "
			    "j_start=0x%p, j_end=0x%p, "
			    "k_start=0x%p, k_end=0x%p",
			    f,
			    i_start, i_end,
			    j_start, j_end,
			    k_start, k_end);
	h5b_partition_t *p = f->b->write_layout;

	*i_start = p->i_start;
	*i_end =   p->i_end;
	*j_start = p->j_start;
	*j_end =   p->j_end;
	*k_start = p->k_start;
	*k_end =   p->k_end;

	H5_CORE_API_RETURN (H5_SUCCESS);
}

h5_err_t
h5b_3d_set_chunk (
	h5_file_t *const f,		/*!< IN: File handle */
	const h5_size_t i,		/*!< IN: size of \c i */ 
	const h5_size_t j,		/*!< IN: size of \c j */  
	const h5_size_t k		/*!< IN: size of \c k */ 
	) {
	H5_CORE_API_ENTER4 (h5_err_t,
			    "f=0x%p, i=%llu, j=%llu, k=%llu",
			    f,
			    (long long unsigned)i,
			    (long long unsigned)j,
			    (long long unsigned)k);
	if ( i == 0 || j == 0 || k == 0 )
	{
		h5_info ("Disabling chunking" );
		TRY (hdf5_set_layout_property(f->b->dcreate_prop, H5D_CONTIGUOUS));
	} else 
	{
		h5_info ("Setting chunk to (%lld,%lld,%lld)",
			 (long long)i, (long long)j, (long long)k);
		hsize_t dims[3] = { k, j, i };
		TRY (hdf5_set_chunk_property (f->b->dcreate_prop, 1, dims));
	}

	H5_CORE_API_RETURN (H5_SUCCESS);
}

h5_err_t
h5b_3d_get_chunk (
	h5_file_t *const f,		/*!< IN: File handle */
	const char *field_name, 	/*!< IN: name of dataset */
	h5_size_t *const i,		/*!< OUT: size of \c i */ 
	h5_size_t *const j,		/*!< OUT: size of \c j */  
	h5_size_t *const k		/*!< OUT: size of \c k */ 
	) {
	H5_CORE_API_ENTER4 (h5_err_t,
			    "f=0x%p, i=0x%p, j=0x%p, k=0x%p",
			    f, i, j, k);
	CHECK_TIMEGROUP ( f );

	h5b_fdata_t *b = f->b;

	TRY( h5bpriv_open_field_group ( f, field_name ) );

	hid_t dataset_id;
	hid_t plist_id;
	hsize_t hdims[3];

	TRY (dataset_id = hdf5_open_dataset (b->field_gid, H5_BLOCKNAME_X));
	TRY (plist_id = hdf5_get_dataset_create_plist (dataset_id));
	TRY (hdf5_get_chunk_property (plist_id, 3, hdims));
	TRY (hdf5_close_property (plist_id));
	TRY (hdf5_close_dataset (dataset_id));

	*i = hdims[2];
	*j = hdims[1];
	*k = hdims[0];

	h5_info("Found chunk dimensions (%lld,%lld,%lld)",
		(long long)hdims[0],
		(long long)hdims[1],
		(long long)hdims[2] );

	H5_CORE_API_RETURN (H5_SUCCESS);
}

#ifdef PARALLEL_IO
h5_err_t
h5b_3d_set_grid (
	h5_file_t *const f,		/*!< IN: File handle */
	const h5_size_t i,		/*!< IN: dimension in \c i */ 
	const h5_size_t j,		/*!< IN: dimension in \c j */  
	const h5_size_t k		/*!< IN: dimension in \c k */ 
	) {
	H5_CORE_API_ENTER4 (h5_err_t,
			    "f=0x%p, i=%llu, j=%llu, k=%llu",
			    f,
			    (long long unsigned)i,
			    (long long unsigned)j,
			    (long long unsigned)k);
	if (i*j*k != f->nprocs) {
		H5_CORE_API_LEAVE (
			h5_error(H5_ERR_INVAL,
				 "Grid dimensions (%lld,%lld,%lld) do not multiply "
				 "out to %d MPI processors!",
				 (long long)i,
				 (long long)j,
				 (long long)k,
				 f->nprocs));
	}

	f->b->k_grid = i;
	f->b->j_grid = j;
	f->b->i_grid = k;

	int dims[3] = { k, j, i };
	int period[3] = { 0, 0, 0 };
	TRY( h5priv_mpi_cart_create(
		f->comm, 3, dims, period, 0, &f->b->cart_comm) );

	f->b->have_grid = 1;

	H5_CORE_API_RETURN (H5_SUCCESS);
}

h5_err_t
h5b_3d_get_grid_coords (
	h5_file_t *const f,		/*!< IN: File handle */
	const int proc,			/*!< IN: MPI processor */
	h5_int64_t *i,			/*!< OUT: index in \c i */ 
	h5_int64_t *j,			/*!< OUT: index in \c j */  
	h5_int64_t *k			/*!< OUT: index in \c k */ 
	) {
	H5_CORE_API_ENTER5 (h5_err_t,
			    "f=0x%p, proc=%d, i=0x%p, j=0x%p, k=0x%p",
			    f, proc, i, j, k);
	if ( ! f->b->have_grid )
		H5_CORE_API_LEAVE (
			h5_error(H5_ERR_INVAL,
				 "Grid dimensions have not been set!"));

    	int coords[3];
	TRY( h5priv_mpi_cart_coords(f->b->cart_comm, proc, 3, coords) );
	*k = coords[0];
	*j = coords[1];
	*i = coords[2];
	H5_CORE_API_RETURN (H5_SUCCESS);
}

h5_err_t
h5b_3d_set_dims (
	h5_file_t *const f,		/*!< IN: File handle */
	const h5_size_t i,		/*!< IN: dimension in \c i */ 
	const h5_size_t j,		/*!< IN: dimension in \c j */  
	const h5_size_t k		/*!< IN: dimension in \c k */ 
	) {
	H5_CORE_API_ENTER4 (h5_err_t,
			    "f=0x%p, i=%llu, j=%llu, k=%llu",
			    f,
			    (long long unsigned)i,
			    (long long unsigned)j,
			    (long long unsigned)k);
	if ( ! f->b->have_grid )
		H5_CORE_API_LEAVE (
			h5_error(H5_ERR_INVAL,
				 "Grid dimensions have not been set!"));

	h5_size_t dims[3] = { k, j, i };
	h5_size_t check_dims[3] = { k, j, i };

	TRY( h5priv_mpi_bcast(
		check_dims, 3, MPI_LONG_LONG, 0, f->comm) );

	if (	dims[0] != check_dims[0] ||
		dims[1] != check_dims[1] ||
		dims[2] != check_dims[2]
	) {
		H5_CORE_API_LEAVE (
			h5_error(H5_ERR_INVAL,
				 "[%d] Block dimensions do not agree: "
				 "(%lld,%lld,%lld) != (%lld,%lld,%lld)!",
				 f->myproc,
				 (long long)dims[0],
				 (long long)dims[1],
				 (long long)dims[2],
				 (long long)check_dims[0],
				 (long long)check_dims[1],
				 (long long)check_dims[2]));
	}
	h5_int64_t coords[3];
	TRY( h5b_3d_get_grid_coords(f,
		f->myproc, coords+0, coords+1, coords+2) );

	h5b_fdata_t *b = f->b;

	b->user_layout->i_start =	 coords[2]*dims[2];
	b->user_layout->i_end =		(coords[2]+1)*dims[2] - 1;
	b->user_layout->j_start =	 coords[1]*dims[1];
	b->user_layout->j_end =		(coords[1]+1)*dims[1] - 1;
	b->user_layout->k_start =	 coords[0]*dims[0];
	b->user_layout->k_end =		(coords[0]+1)*dims[0] - 1;

	b->write_layout[0] = b->user_layout[0];

	b->i_max = b->i_grid * dims[2] - 1;
	b->j_max = b->j_grid * dims[1] - 1;
	b->k_max = b->k_grid * dims[0] - 1;

	b->have_layout = 1;

	H5_CORE_API_RETURN (H5_SUCCESS);
}
#endif

h5_err_t
h5b_3d_set_halo (
	h5_file_t *const f,		/*!< IN: File handle */
	const h5_size_t i,		/*!< IN: radius in \c i */ 
	const h5_size_t j,		/*!< IN: radius in \c j */  
	const h5_size_t k		/*!< IN: radius in \c k */ 
	) {
	H5_CORE_API_ENTER4 (h5_err_t,
			    "f=0x%p, i=%llu, j=%llu, k=%llu",
			    f,
			    (long long unsigned)i,
			    (long long unsigned)j,
			    (long long unsigned)k);

	if ( ! f->b->have_grid ) {
		H5_CORE_API_LEAVE (
			h5_error(H5_ERR_INVAL,
				 "Grid dimensions have not been set!"));
	} else if ( ! f->b->have_layout ) {
		H5_CORE_API_LEAVE (
			h5_error(H5_ERR_INVAL,
				 "Block dimensions for grid have not been set!"));
	}
	h5b_fdata_t *b = f->b;

	b->user_layout->i_start	-= i;
	b->user_layout->i_end	+= i;
	b->user_layout->j_start	-= j;
	b->user_layout->j_end	+= j;
	b->user_layout->k_start	-= k;
	b->user_layout->k_end	+= k;

	H5_CORE_API_RETURN (H5_SUCCESS);
}

h5_ssize_t
h5b_get_num_fields (
	h5_file_t *const f		/*!< IN: File handle */
	) {
	H5_CORE_API_ENTER1 (h5_ssize_t, "f=0x%p", f);
	CHECK_TIMEGROUP( f );

	TRY (h5bpriv_open_block_group(f));
	H5_CORE_API_RETURN (hdf5_get_num_objs_in_group (f->b->block_gid));
}

h5_err_t
h5b_get_field_info_by_name (
	h5_file_t *const f,			/*!< IN: file handle */
	const char *name,			/*!< OUT: field name */
	h5_size_t *field_rank,			/*!< OUT: field rank */
	h5_size_t *field_dims,			/*!< OUT: field dimensions */
	h5_size_t *elem_rank,			/*!< OUT: element rank */
	h5_int64_t *type			/*!< OUT: datatype */
	) {
	H5_CORE_API_ENTER6 (h5_err_t,
			    "f=0x%p, name=\"%s\", "
			    "field_rank=0x%p, field_dims=0x%p, elem_rank=0x%p, type=0x%p",
			    f, name, field_rank, field_dims, elem_rank, type);
	CHECK_TIMEGROUP( f );

	hsize_t dims[16]; /* give it plenty of space even though we don't expect rank > 3 */
	hsize_t _field_rank, _elem_rank;
	h5_size_t i, j;

	TRY( h5bpriv_open_field_group(f, name) );

	hid_t dataset_id;
 	hid_t dataspace_id;

	TRY (dataset_id = hdf5_open_dataset (f->b->field_gid, H5_BLOCKNAME_X));
	TRY (dataspace_id = hdf5_get_dataset_space (dataset_id) );

	TRY (_field_rank = hdf5_get_dims_of_dataspace (dataspace_id, dims, NULL));
	if (field_rank) *field_rank = (h5_size_t) _field_rank;
 
	if (field_dims) {
		for ( i = 0, j = _field_rank-1; i < _field_rank; i++, j-- )
			field_dims[i] = (h5_size_t)dims[j];
	}

	TRY (_elem_rank = hdf5_get_num_objs_in_group (f->b->field_gid));
	if (elem_rank) *elem_rank = (h5_size_t) _elem_rank;

	hid_t h5type;
	TRY (h5type = hdf5_get_dataset_type (dataset_id));

	if ( type )
		TRY( *type = h5_normalize_h5_type(h5type) );

	TRY (hdf5_close_dataspace (dataspace_id));
	TRY (hdf5_close_dataset (dataset_id));

	H5_CORE_API_RETURN (H5_SUCCESS);
}

h5_err_t
h5b_get_field_info (
	h5_file_t *const f,			/*!< IN: file handle */
	const h5_size_t idx,			/*!< IN: index of field */
	char *name,				/*!< OUT: field name */
	const h5_size_t len_name,		/*!< IN: buffer size */
	h5_size_t *field_rank,			/*!< OUT: field rank */
	h5_size_t *field_dims,			/*!< OUT: field dimensions */
	h5_size_t *elem_rank,			/*!< OUT: element rank */
	h5_int64_t *type			/*!< OUT: datatype */
	) {
	H5_CORE_API_ENTER8 (h5_err_t,
			    "f=0x%p, idx=%llu, "
			    "name=0x%p, len_name=%llu, "
			    "field_rank=0x%p, field_dims=0x%p, elem_rank=0x%p, type=0x%p",
			    f,
			    (long long unsigned)idx,
			    name, (long long unsigned)len_name, 
			    field_rank, field_dims, elem_rank, type);
	CHECK_TIMEGROUP( f );

	TRY (h5bpriv_open_block_group(f));
	TRY (hdf5_get_objname_by_idx(
		     f->b->block_gid,
		     (hsize_t)idx,
		     name,
		     (size_t)len_name) );

	H5_CORE_API_RETURN (h5b_get_field_info_by_name (
				    f,
				    name, field_rank, field_dims, elem_rank, type));
}

