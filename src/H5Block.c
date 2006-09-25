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

#include <hdf5.h>
#include "H5Part.h"
#include "H5PartErrors.h"
#include "H5PartPrivate.h"

#include "H5BlockTypes.h"
#include "H5Block.h"
#include "H5BlockPrivate.h"
#include "H5BlockErrors.h"


/*!
  Check whether \c f points to a valid file handle
*/

static h5part_int64_t
_file_is_valid (
	const H5PartFile *f
	) {

	if ( f == NULL )
		return H5PART_ERR_BADFD;
	if ( f->file == 0 )
		return H5PART_ERR_BADFD;
	if ( f->block == NULL )
		return H5PART_ERR_BADFD;
	return H5PART_SUCCESS;
}


/********************** file open and close **********************************/

static h5part_int64_t
_close (
	H5PartFile *f
	);


#define INIT( f ) { \
	h5part_int64_t herr = _init ( f ); \
	if ( herr < 0 ) return herr; \
}

static h5part_int64_t
_init (
	H5PartFile *f
	) {
	h5part_int64_t herr;
	struct H5BlockStruct *b; 

	herr = _file_is_valid ( f );
	if ( herr == H5PART_SUCCESS ) return H5PART_SUCCESS;

	if ( (f == 0) || (f->file == 0) ) return HANDLE_H5PART_BADFD_ERR;

	/*
	  hack for non-parallel processing, should be set in H5Part
	*/
	if ( f->nprocs == 0 ) f->nprocs = 1;

	f->block = malloc( sizeof (*f->block) );
	if ( f->block == NULL ) {
		return HANDLE_H5PART_NOMEM_ERR;
	}
	b = f->block;
	memset ( b, 0, sizeof (*b) );
	b->user_layout = malloc ( f->nprocs * sizeof (b->user_layout[0]) );
	if ( b->user_layout == NULL ) {
		return HANDLE_H5PART_NOMEM_ERR;
	}
	b->write_layout = malloc ( f->nprocs * sizeof (b->write_layout[0]) );
	if ( b->write_layout == NULL ) {
		return HANDLE_H5PART_NOMEM_ERR;
	}
	b->timestep = -1;
	b->blockgroup = -1;
	b->shape = -1;
	b->diskshape = -1;
	b->memshape = -1;
	b->field_group_id = -1;
	b->have_layout = 0;

	f->close_block = _close;

	return H5PART_SUCCESS;
}

static h5part_int64_t
_close (
	H5PartFile *f
	) {

	herr_t herr;
	struct H5BlockStruct *b = f->block;

	if ( b->blockgroup >= 0 ) {
		herr = H5Gclose ( b->blockgroup );
		if ( herr < 0 ) return HANDLE_H5G_CLOSE_ERR;
		b->blockgroup = -1;
	}
	if ( b->shape >= 0 ) {
		herr = H5Sclose ( b->shape );
		if ( herr < 0 ) return HANDLE_H5S_CLOSE_ERR;
		b->shape = -1;
	}
	if ( b->diskshape >= 0 ) {
		herr = H5Sclose ( b->diskshape );
		if ( herr < 0 ) return HANDLE_H5S_CLOSE_ERR;
		b->diskshape = -1;
	}
	if ( b->memshape >= 0 ) {
		herr = H5Sclose ( b->memshape );
		if ( herr < 0 ) return HANDLE_H5S_CLOSE_ERR;
		b->memshape = -1;
	}
	free ( f->block );
	f->block = NULL;
	f->close_block = NULL;

	return H5PART_SUCCESS;
}

/********************** defining the layout **********************************/

/*!
  \note
  A partition must not be part of another partition.

  A partition must not divide another partition into two pieces.

  After handling the ghost zones, the partition must not be empty

  We must track the overall size somewhere. This is a good place to do it. (?)
*/

/*!
  Normalize partition

  "Normal" means that the start coordinates are less or equal the
  end coordinates.
*/
static void
_normalize_partition (
	struct H5BlockPartition *p
	) {
	h5part_float64_t x;

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
  Gather layout to all processors
*/
#ifdef PARALLEL_IO
h5part_int64_t
_allgather (
	const H5PartFile *f
	) {
	struct H5BlockPartition *partition = &f->block->user_layout[f->myproc];
	struct H5BlockPartition *layout = f->block->user_layout;

	MPI_Datatype    partition_m;
	size_t n = sizeof (struct H5BlockPartition) / sizeof (h5part_int64_t);

	MPI_Type_contiguous ( n, MPI_LONG_LONG, &partition_m );
        MPI_Type_commit ( &partition_m );

	MPI_Allgather ( partition, 1, partition_m, layout, 1, partition_m,
			f->comm );

	return H5PART_SUCCESS;
}
#else
h5part_int64_t
_allgather (
	const H5PartFile *f
	) {

	return H5PART_SUCCESS;
}
#endif

static void
_get_dimension_sizes (
	H5PartFile *f
	) {
	int proc;
	struct H5BlockStruct *b = f->block;
	struct H5BlockPartition *partition = b->user_layout;

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

/*
#define _HAVE_GHOSTZONE(p,q) ( (p->i_end >= q->i_start) \
  		          &&   (p->j_end >= q->j_start) \
		          &&   (p->k_end >= q->k_start) \
			  &&   (q->i_end >= p->i_start) \
  		          &&   (q->j_end >= p->j_start) \
		          &&   (q->k_end >= p->k_start) )
*/

static int
_have_ghostzone (
	const struct H5BlockPartition *p,
	const struct H5BlockPartition *q
	) {
	return ( ! ( _NO_GHOSTZONE ( p, q ) || _NO_GHOSTZONE ( q, p ) ) );
}

static h5part_int64_t
_volume_of_partition (
	const struct H5BlockPartition *p
	) {
	return (p->i_end - p->i_start)
		* (p->j_end - p->j_start)
		* (p->k_end - p->k_start);

}

#define MIN( x, y ) ( (x) <= (y) ? (x) : (y) )  
#define MAX( x, y ) ( (x) >= (y) ? (x) : (y) )  

static h5part_int64_t
_volume_of_ghostzone (
	const struct H5BlockPartition *p,
	const struct H5BlockPartition *q
	) {

	h5part_int64_t dx = MIN ( p->i_end, q->i_end )
		- MAX ( p->i_start, q->i_start ) + 1;
	h5part_int64_t dy = MIN ( p->j_end, q->j_end )
		- MAX ( p->j_start, q->j_start ) + 1;
	h5part_int64_t dz = MIN ( p->k_end, q->k_end )
		- MAX ( p->k_start, q->k_start ) + 1;

	return dx * dy * dz;
}

static h5part_int64_t
_dissolve_X_ghostzone (
	struct H5BlockPartition *p,
	struct H5BlockPartition *q
	) {

	if ( p->i_start > q->i_start )
		return _dissolve_X_ghostzone( q, p );

	if ( q->i_end <= p->i_end )  /* no dissolving		*/
		return -1;

	p->i_end = ( p->i_end + q->i_start ) >> 1;
	q->i_start = p->i_end + 1;
	return 0;
}

static h5part_int64_t
_dissolve_Y_ghostzone (
	struct H5BlockPartition *p,
	struct H5BlockPartition *q
	) {

	if ( p->j_start > q->j_start )
		return _dissolve_Y_ghostzone( q, p );

	if ( q->j_end <= p->j_end )    /* no dissolving		*/
		return -1;

	p->j_end = ( p->j_end + q->j_start ) >> 1;
	q->j_start = p->j_end + 1;
	return 0;
}

static h5part_int64_t
_dissolve_Z_ghostzone (
	struct H5BlockPartition *p,
	struct H5BlockPartition *q
	) {

	if ( p->k_start > q->k_start )
		return _dissolve_Z_ghostzone( q, p );

	if ( q->k_end <= p->k_end )    /* no dissolving		*/
		return -1;

	p->k_end = ( p->k_end + q->k_start ) >> 1;
	q->k_start = p->k_end + 1;
	return 0;
}

static h5part_int64_t
_dissolve_ghostzone (
	struct H5BlockPartition *p,
	struct H5BlockPartition *q
	) {

	struct H5BlockPartition p_;
	struct H5BlockPartition q_;
	struct H5BlockPartition p_best;
	struct H5BlockPartition q_best;
	h5part_int64_t vol;
	h5part_int64_t max_vol = 0;

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
		return H5PART_ERR_LAYOUT;
	}
	*p = p_best;
	*q = q_best;

	return H5PART_SUCCESS;
}

#if OLD_DISSOLVE_GHOSTZONES
static h5part_int64_t
_dissolve_ghostzones (
	H5PartFile *f
	) {

	struct H5BlockStruct *b = f->block;
	struct H5BlockPartition *p, *max_p = NULL;
	struct H5BlockPartition *q, *max_q = NULL;
	int proc_p, proc_q;
	h5part_int64_t vol, max_vol;
	int max_proc_p, max_proc_q;

	memcpy ( b->write_layout, b->user_layout,
		 f->nprocs * sizeof (*f->block->user_layout) );

	while ( 1 ) {
		max_vol = 0;
		for ( proc_p = 0, p = b->write_layout;
		      proc_p < f->nprocs-1;
		      proc_p++, p++ ) {
			for ( proc_q = proc_p+1, q = &b->write_layout[proc_q];
			      proc_q < f->nprocs;
			      proc_q++, q++ ) {

				if ( ! _have_ghostzone ( p, q ) )
					continue;
				vol = _volume_of_ghostzone ( p, q );
				if ( vol > max_vol ) {
					max_vol = vol;
					max_proc_p = proc_p;
					max_proc_q = proc_q;
					max_p = p;
					max_q = q;
				}
			}
		}
		if ( max_vol == 0 )
			break;

		_dissolve_ghostzone ( max_p, max_q );
	}

	_H5Part_print_debug ("PROC[%d]: Layout after dissolving ghost-zones:",
			     f->myproc );
	for ( proc_p = 0, p = b->write_layout;
	      proc_p < f->nprocs;
	      proc_p++, p++ ) {
		_H5Part_print_debug (
			"PROC[%d]: proc[%d]: %lld:%lld, %lld:%lld, %lld:%lld  ",
			f->myproc, proc_p,
			(long long)p->i_start,
			(long long)p->i_end,
			(long long)p->j_start,
			(long long)p->j_end,
			(long long)p->k_start,
			(long long)p->k_end );
	}
	return H5PART_SUCCESS;
}
#else
static h5part_int64_t
_dissolve_ghostzones (
	H5PartFile *f
	) {

	struct H5BlockStruct *b = f->block;
	struct H5BlockPartition *p;
	struct H5BlockPartition *q;
	int proc_p, proc_q;

	struct list {
		struct list *prev;
		struct list *next;
		struct H5BlockPartition *p;
		struct H5BlockPartition *q;
		h5part_int64_t vol;
	} *p_begin, *p_el, *p_max, *p_end, *p_save;

	memcpy ( b->write_layout, b->user_layout,
		 f->nprocs * sizeof (*f->block->user_layout) );

	p_begin = p_max = p_end = malloc ( sizeof ( *p_begin ) );
	if ( p_begin == NULL ) return HANDLE_H5PART_NOMEM_ERR;
	
	memset ( p_begin, 0, sizeof ( *p_begin ) );

	for ( proc_p = 0, p = b->write_layout;
	      proc_p < f->nprocs-1;
	      proc_p++, p++ ) {
		for ( proc_q = proc_p+1, q = &b->write_layout[proc_q];
		      proc_q < f->nprocs;
		      proc_q++, q++ ) {

			if ( _have_ghostzone ( p, q ) ) {
				p_el = malloc ( sizeof ( *p_el ) );
				if ( p_el == NULL )
					return HANDLE_H5PART_NOMEM_ERR;

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
		
		_dissolve_ghostzone ( p_max->p, p_max->q );

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

	_H5Part_print_debug ("Layout after dissolving ghost-zones:");
	for ( proc_p = 0, p = b->write_layout;
	      proc_p < f->nprocs;
	      proc_p++, p++ ) {
		_H5Part_print_debug (
			"PROC[%d]: proc[%d]: %lld:%lld, %lld:%lld, %lld:%lld  ",
			f->myproc, proc_p,
			(long long)p->i_start, (long long)p->i_end,
			(long long)p->j_start, (long long)p->j_end,
			(long long)p->k_start, (long long)p->k_end );
	}
	return H5PART_SUCCESS;
}
#endif

h5part_int64_t
_release_hyperslab (
	H5PartFile *f
	) {
	herr_t herr;

	if ( f->block->shape > 0 ) {
		herr = H5Sclose ( f->block->shape );
		if ( herr < 0 ) return H5PART_ERR_HDF5;
		f->block->shape = -1;
	}
	if ( f->block->diskshape > 0 ) {
		herr = H5Sclose ( f->block->diskshape );
		if ( herr < 0 ) return H5PART_ERR_HDF5;
		f->block->diskshape = -1;
	}
	if ( f->block->memshape > 0 ) {
		herr = H5Sclose ( f->block->memshape );
		if ( herr < 0 ) return H5PART_ERR_HDF5;
		f->block->memshape = -1;
	}
	return H5PART_SUCCESS;
}

h5part_int64_t
H5BlockDefine3DFieldLayout(
	H5PartFile *f,
 	const h5part_int64_t i_start,
	const h5part_int64_t i_end,
	const h5part_int64_t j_start,
	const h5part_int64_t j_end,
	const h5part_int64_t k_start,
	const h5part_int64_t k_end
	) {

	SET_FNAME ( "H5BlockDefine3DFieldLayout" );
	INIT( f );

	struct H5BlockStruct *b = f->block;
	struct H5BlockPartition *p = &b->user_layout[f->myproc];
	p->i_start = i_start;
	p->i_end =   i_end;
	p->j_start = j_start;
	p->j_end =   j_end;
	p->k_start = k_start;
	p->k_end =   k_end;

	_normalize_partition( p );

	h5part_int64_t herr = _allgather ( f );
	if ( herr < 0 ) return HANDLE_MPI_ALLGATHER_ERR;

	_get_dimension_sizes ( f );

	herr = _dissolve_ghostzones ( f );
	if ( herr < 0 ) return HANDLE_H5PART_LAYOUT_ERR;

	herr = _release_hyperslab ( f );
	if ( herr < 0 )	return HANDLE_H5S_CLOSE_ERR;

	b->have_layout = 1;

	return H5PART_SUCCESS;
}

h5part_int64_t
H5Block3dGetPartitionOfProc (
	H5PartFile *f,
	h5part_int64_t proc,
	h5part_int64_t *i_start, 
	h5part_int64_t *i_end,
	h5part_int64_t *j_start,
	h5part_int64_t *j_end,
	h5part_int64_t *k_start,
	h5part_int64_t *k_end ) {

	SET_FNAME ( "H5Block3dGetProcOf" );
	INIT ( f );
	CHECK_LAYOUT ( f );

	if ( ( proc < 0 ) || ( proc >= f->nprocs ) )
		return -1;

	struct H5BlockPartition *p = &f->block->user_layout[(size_t)proc];

	*i_start = p->i_start;
	*i_end =   p->i_end;
	*j_start = p->j_start;
	*j_end =   p->j_end;
	*k_start = p->k_start;
	*k_end =   p->k_end;

	return H5PART_SUCCESS;
}

h5part_int64_t
H5Block3dGetReducedPartitionOfProc (
	H5PartFile *f,
	h5part_int64_t proc,
	h5part_int64_t *i_start, 
	h5part_int64_t *i_end,
	h5part_int64_t *j_start,
	h5part_int64_t *j_end,
	h5part_int64_t *k_start,
	h5part_int64_t *k_end
	) {

	SET_FNAME ( "H5Block3dGetProcOf" );
	INIT ( f );
	CHECK_LAYOUT ( f );

	if ( ( proc < 0 ) || ( proc >= f->nprocs ) )
		return -1;

	struct H5BlockPartition *p = &f->block->write_layout[(size_t)proc];

	*i_start = p->i_start;
	*i_end =   p->i_end;
	*j_start = p->j_start;
	*j_end =   p->j_end;
	*k_start = p->k_start;
	*k_end =   p->k_end;

	return H5PART_SUCCESS;
}


h5part_int64_t
H5Block3dGetProcOf (
	H5PartFile *f,
	h5part_int64_t i,
	h5part_int64_t j,
	h5part_int64_t k
	) {

	SET_FNAME ( "H5Block3dGetProcOf" );
	INIT ( f );
	CHECK_LAYOUT ( f );

	struct H5BlockPartition *layout = f->block->write_layout;
	int proc;

	for ( proc = 0; proc < f->nprocs; proc++, layout++ ) {
		if ( (layout->i_start <= i) && (i <= layout->i_end) &&
		     (layout->j_start <= j) && (j <= layout->j_end) &&
		     (layout->k_start <= k) && (k <= layout->k_end) ) 
			return (h5part_int64_t)proc;
	}
	
	return -1;
}

/********************** helper functions for reading and writing *************/

static h5part_int64_t
_open_block_group (
	const H5PartFile *f
	) {

	h5part_int64_t herr;
	struct H5BlockStruct *b = f->block;

	if ( (f->timestep != b->timestep) && (b->blockgroup > 0) ) {
		herr = H5Gclose ( b->blockgroup );
		if ( herr < 0 ) return HANDLE_H5G_CLOSE_ERR;
		f->block->blockgroup = -1;
	}

	if ( b->blockgroup < 0 ) {
		herr = H5Gopen ( f->timegroup, H5BLOCK_GROUP_NAME );
		if ( herr < 0 ) return HANDLE_H5G_OPEN_ERR ( H5BLOCK_GROUP_NAME );
		b->blockgroup = herr;
	}
	b->timestep = f->timestep;

	return H5PART_SUCCESS;
}

/********************** functions for reading ********************************/

static h5part_int64_t
_have_object (
	const hid_t id,
	const char *name
	) {
	return (H5Gget_objinfo( id, name, 1, NULL ) >= 0 ? 1 : 0);
}

static h5part_int64_t
_open_field_group (
	H5PartFile *f,
	const char *name
	) {

	herr_t herr;
	struct H5BlockStruct *b = f->block;

	herr = _open_block_group ( f );
	if ( herr < 0 ) return herr;

	if ( ! _have_object ( b->blockgroup, name ) )
		return HANDLE_H5PART_NOENT_ERR ( name );

	herr = H5Gopen ( b->blockgroup, name );
	if ( herr < 0 ) return HANDLE_H5G_OPEN_ERR ( name );

	b->field_group_id = herr;

	return H5PART_SUCCESS;
}

h5part_int64_t
_close_field_group (
	H5PartFile *f
	) {

	herr_t herr = H5Gclose ( f->block->field_group_id );
	if ( herr < 0 ) return HANDLE_H5G_CLOSE_ERR;

	return H5PART_SUCCESS;
}

static h5part_int64_t
_select_hyperslab_for_reading (
	H5PartFile *f,
	hid_t dataset
	) {

	struct H5BlockStruct *b = f->block;
	struct H5BlockPartition *p = &b->user_layout[f->myproc];
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

	herr_t herr = _release_hyperslab ( f );
	if ( herr < 0 )	return HANDLE_H5S_CLOSE_ERR;

 	b->diskshape = H5Dget_space ( dataset );
	if ( b->diskshape < 0 ) return HANDLE_H5D_GET_SPACE_ERR;

	rank = H5Sget_simple_extent_dims ( b->diskshape, NULL, NULL );
	if ( rank < 0 )  return HANDLE_H5S_GET_SIMPLE_EXTENT_DIMS_ERR;
	if ( rank != 3 ) return HANDLE_H5PART_DATASET_RANK_ERR ( rank, 3 );

	rank = H5Sget_simple_extent_dims ( b->diskshape, field_dims, NULL );
	if ( rank < 0 )  return HANDLE_H5S_GET_SIMPLE_EXTENT_DIMS_ERR;
	
	if ( (field_dims[0] < b->k_max) ||
	     (field_dims[1] < b->j_max) ||
	     (field_dims[2] < b->i_max) ) return HANDLE_H5PART_LAYOUT_ERR;

	_H5Part_print_debug (
		"PROC[%d]: \n"
		"\tfield_dims: (%lld,%lld,%lld)",
		f->myproc,
		(long long)field_dims[2],
		(long long)field_dims[1],
		(long long)field_dims[0] );

	b->diskshape = H5Screate_simple ( rank, field_dims,field_dims );
	if ( b->diskshape < 0 )
		return HANDLE_H5S_CREATE_SIMPLE_3D_ERR ( field_dims );

	f->block->memshape = H5Screate_simple ( rank, part_dims, part_dims );
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

	return H5PART_SUCCESS;
}

h5part_int64_t
_read_data (
	H5PartFile *f,
	const char *name,
	h5part_float64_t *data
	) {

	struct H5BlockStruct *b = f->block;

	hid_t dataset_id = H5Dopen ( b->field_group_id, name );
	if ( dataset_id < 0 ) return HANDLE_H5D_OPEN_ERR ( name );

	h5part_int64_t herr = _select_hyperslab_for_reading ( f, dataset_id );
	if ( herr < 0 ) return herr;

	herr = H5Dread ( 
		dataset_id,
		H5T_NATIVE_DOUBLE,
		f->block->memshape,
		f->block->diskshape,
		H5P_DEFAULT,
		data );
	if ( herr < 0 ) return HANDLE_H5D_READ_ERR ( name, f->timestep );

	herr = H5Dclose ( dataset_id );
	if ( herr < 0 ) return HANDLE_H5D_CLOSE_ERR;

	return H5PART_SUCCESS;
}

h5part_int64_t
H5Block3dReadScalarField (
	H5PartFile *f,
	const char *name,
	h5part_float64_t *data
	) {

	SET_FNAME ( "H5Block3dReadScalarField" );
	INIT ( f );
	CHECK_TIMEGROUP ( f );
	CHECK_LAYOUT ( f );

	h5part_int64_t herr = _open_field_group ( f, name );
	if ( herr < 0 ) return herr;

	herr = _read_data ( f, "0", data );
	if ( herr < 0 ) return herr;

	herr = _close_field_group ( f );
	if ( herr < 0 ) return herr;

	return H5PART_SUCCESS;
}

h5part_int64_t
H5Block3dRead3dVectorField (
	H5PartFile *f,
	const char *name,
	h5part_float64_t *x_data,
	h5part_float64_t *y_data,
	h5part_float64_t *z_data
	) {

	SET_FNAME ( "H5Block3dRead3dVectorField" );
	INIT ( f );
	CHECK_TIMEGROUP ( f );
	CHECK_LAYOUT ( f );

	h5part_int64_t herr = _open_field_group ( f, name );
	if ( herr < 0 ) return herr;

	herr = _read_data ( f, "0", x_data );
	if ( herr < 0 ) return herr;
	herr = _read_data ( f, "1", y_data );
	if ( herr < 0 ) return herr;
	herr = _read_data ( f, "2", z_data );
	if ( herr < 0 ) return herr;

	herr = _close_field_group ( f );
	if ( herr < 0 ) return herr;

	return H5PART_SUCCESS;
}

/********************** functions for writing ********************************/

static h5part_int64_t
_select_hyperslab_for_writing (
	H5PartFile *f
	) {

	/*
	  re-use existing hyperslab
	*/
	if ( f->block->shape >= 0 ) return H5PART_SUCCESS;

	herr_t herr;
	struct H5BlockStruct *b = f->block;
	struct H5BlockPartition *p = &b->write_layout[f->myproc];
	struct H5BlockPartition *q = &b->user_layout[f->myproc];

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

	f->block->memshape = H5Screate_simple ( rank, field_dims, field_dims );
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

	return H5PART_SUCCESS;
}


static h5part_int64_t
_create_block_group (
	const H5PartFile *f
	) {

	herr_t herr;
	struct H5BlockStruct *b = f->block;

	if ( b->blockgroup > 0 ) {
		herr = H5Gclose ( b->blockgroup );
		if ( herr < 0 ) return HANDLE_H5G_CLOSE_ERR;
		f->block->blockgroup = -1;
	}

	herr = H5Gcreate ( f->timegroup, H5BLOCK_GROUP_NAME, 0 );
	if ( herr < 0 ) return HANDLE_H5G_CREATE_ERR ( H5BLOCK_GROUP_NAME );

	f->block->blockgroup = herr;
	return H5PART_SUCCESS;
}

static h5part_int64_t
_create_field_group (
	H5PartFile *f,
	const char *name
	) {

	herr_t herr;
	struct H5BlockStruct *b = f->block;


	if ( ! _have_object ( f->timegroup, H5BLOCK_GROUP_NAME ) ) {
		herr = _create_block_group ( f );
	} else {
		herr = _open_block_group ( f );
	}
	if ( herr < 0 ) return herr;

	herr = _select_hyperslab_for_writing ( f );
	if ( herr < 0 ) return herr;

	if ( _have_object ( b->blockgroup, name ) )
		return  HANDLE_H5PART_GROUP_EXISTS_ERR ( name );

	herr = H5Gcreate ( b->blockgroup, name, 0 );
	if ( herr < 0 ) return HANDLE_H5G_CREATE_ERR ( name );
	b->field_group_id = herr;

	return H5PART_SUCCESS;
}	

h5part_int64_t
_write_data (
	H5PartFile *f,
	const char *name,
	const h5part_float64_t *data
	) {

	herr_t herr;
	hid_t dataset;
	struct H5BlockStruct *b = f->block;

	dataset = H5Dcreate (
		b->field_group_id,
		name,
		H5T_NATIVE_DOUBLE,
		b->shape, 
		H5P_DEFAULT );
	if ( dataset < 0 ) return HANDLE_H5D_CREATE_ERR ( name, f->timestep );

	herr = H5Dwrite ( 
		dataset,
		H5T_NATIVE_DOUBLE,
		b->memshape,
		b->diskshape,
		H5P_DEFAULT,
		data );
	if ( herr < 0 ) return HANDLE_H5D_WRITE_ERR ( name, f->timestep );

	herr = H5Dclose ( dataset );
	if ( herr < 0 ) return HANDLE_H5D_CLOSE_ERR;

	return H5PART_SUCCESS;
}

h5part_int64_t
H5Block3dWriteScalarField (
	H5PartFile *f,
	const char *name,
	const h5part_float64_t *data
	) {

	SET_FNAME ( "H5Block3dWriteScalarField" );
	INIT ( f );
	CHECK_WRITABLE_MODE ( f );
	CHECK_TIMEGROUP ( f );
	CHECK_LAYOUT ( f );

	h5part_int64_t herr = _create_field_group ( f, name );
	if ( herr < 0 ) return herr;

	herr = _write_data ( f, "0", data );
	if ( herr < 0 ) return herr;

	herr = _close_field_group ( f );
	if ( herr < 0 ) return herr;

	return H5PART_SUCCESS;
}

h5part_int64_t
H5Block3dWrite3dVectorField (
	H5PartFile *f,
	const char *name,
	const h5part_float64_t *x_data,
	const h5part_float64_t *y_data,
	const h5part_float64_t *z_data
	) {

	SET_FNAME ( "H5Block3dWrite3dVectorField" );
	INIT ( f );
	CHECK_WRITABLE_MODE ( f );
	CHECK_TIMEGROUP ( f );
	CHECK_LAYOUT ( f );

	h5part_int64_t herr = _create_field_group ( f, name );
	if ( herr < 0 ) return herr;

	herr = _write_data ( f, "0", x_data );
	if ( herr < 0 ) return herr;
	herr = _write_data ( f, "1", y_data );
	if ( herr < 0 ) return herr;
	herr = _write_data ( f, "2", z_data );
	if ( herr < 0 ) return herr;

	herr = _close_field_group ( f );
	if ( herr < 0 ) return herr;

	return H5PART_SUCCESS;
}

/********************** query information about available fields *************/

h5part_int64_t
H5BlockGetNumFields (
	H5PartFile *f
	) {

	SET_FNAME ( "H5BlockGetNumFields" );
	INIT ( f );
	CHECK_TIMEGROUP( f );

	if ( ! _have_object ( f->timegroup, H5BLOCK_GROUP_NAME ) )
		return 0;

	return _H5Part_get_num_objects ( f->timegroup, H5BLOCK_GROUP_NAME, H5G_GROUP );
}

static h5part_int64_t
_get_field_info (
	H5PartFile *f,
	const char *field_name,
	h5part_int64_t *grid_rank,
	h5part_int64_t *grid_dims,
	h5part_int64_t *field_dims
	) {

	hsize_t dims[16];
	h5part_int64_t i, j;

	h5part_int64_t herr = _open_block_group ( f );
	if ( herr < 0 ) return herr;

	hid_t group_id = H5Gopen ( f->block->blockgroup, field_name );
	if ( group_id < 0 ) return HANDLE_H5G_OPEN_ERR ( field_name );

	hid_t dataset_id = H5Dopen ( group_id, "x" );
	if ( dataset_id < 0 ) return HANDLE_H5D_OPEN_ERR ( "x" );

 	hid_t dataspace_id = H5Dget_space ( dataset_id );
	if ( dataspace_id < 0 ) return HANDLE_H5D_GET_SPACE_ERR;

	*grid_rank = H5Sget_simple_extent_dims ( dataspace_id, dims, NULL );
	if ( *grid_rank < 0 )  return HANDLE_H5S_GET_SIMPLE_EXTENT_DIMS_ERR;

	for ( i = 0, j = *grid_rank-1; i < *grid_rank; i++, j-- )
		grid_dims[i] = (h5part_int64_t)dims[j];

	*field_dims = _H5Part_get_num_objects (
		f->block->blockgroup,
		field_name,
		H5G_DATASET );
	if ( *field_dims < 0 ) return *field_dims;

	herr = H5Sclose ( dataspace_id );
	if ( herr < 0 ) return HANDLE_H5S_CLOSE_ERR;

	herr = H5Dclose ( dataset_id );
	if ( herr < 0 ) return HANDLE_H5D_CLOSE_ERR;

	herr = H5Gclose ( group_id ); 
	if ( herr < 0 ) return HANDLE_H5G_CLOSE_ERR;

	return H5PART_SUCCESS;
}

h5part_int64_t
H5BlockGetFieldInfo (
	H5PartFile *f,
	const h5part_int64_t idx,
	char *field_name,
	const h5part_int64_t len_field_name,
	h5part_int64_t *grid_rank,
	h5part_int64_t *grid_dims,
	h5part_int64_t *field_dims
	) {

	SET_FNAME ( "H5BlockGetFieldInfo" );
	INIT ( f );
	CHECK_TIMEGROUP( f );

	h5part_int64_t herr = _H5Part_get_object_name (
		f->timegroup,
		H5BLOCK_GROUP_NAME,
		H5G_GROUP,
		idx,
		field_name,
		len_field_name );
	if ( herr < 0 ) return herr;

	return _get_field_info (
		f, field_name, grid_rank, grid_dims, field_dims );
}

h5part_int64_t
H5BlockGetFieldInfoByName (
	H5PartFile *f,
	const char *field_name,
	h5part_int64_t *grid_rank,
	h5part_int64_t *grid_dims,
	h5part_int64_t *field_dims
	) {

	SET_FNAME ( "H5BlockGetFieldInfo" );
	INIT ( f );
	CHECK_TIMEGROUP( f );

	return _get_field_info (
		f, field_name, grid_rank, grid_dims, field_dims );
}

/********************** reading and writing attribute ************************/

static h5part_int64_t
_write_field_attrib (
	H5PartFile *f,
	const char *field_name,
	const char *attrib_name,
	const hid_t attrib_type,
	const void *attrib_value,
	const h5part_int64_t attrib_nelem
	) {

	h5part_int64_t herr = _open_field_group ( f, field_name );
	if ( herr < 0 ) return herr;

	_H5Part_write_attrib (
		f->block->field_group_id,
		attrib_name,
		attrib_type,
		attrib_value,
		attrib_nelem );
	if ( herr < 0 ) return herr;

	herr = _close_field_group ( f );
	if ( herr < 0 ) return herr;

	return H5PART_SUCCESS;
}

h5part_int64_t
H5BlockWriteFieldAttrib (
	H5PartFile *f,
	const char *field_name,
	const char *attrib_name,
	const h5part_int64_t attrib_type,
	const void *attrib_value,
	const h5part_int64_t attrib_nelem
	) {

	SET_FNAME ( "H5BlockWriteFieldAttrib" );
	INIT ( f );
	CHECK_WRITABLE_MODE( f );
	CHECK_TIMEGROUP( f );

	return _write_field_attrib (
		f,
		field_name,
		attrib_name, attrib_type, attrib_value,
		attrib_nelem );
}

h5part_int64_t
H5BlockWriteFieldAttribString (
	H5PartFile *f,
	const char *field_name,
	const char *attrib_name,
	const char *attrib_value
	) {

	SET_FNAME ( "H5BlockWriteFieldAttribString" );
	INIT ( f );
	CHECK_WRITABLE_MODE( f );
	CHECK_TIMEGROUP( f );

	return _write_field_attrib (
		f,
		field_name,
		attrib_name, H5T_NATIVE_CHAR, attrib_value,
		strlen ( attrib_value ) + 1 );
}

h5part_int64_t
H5BlockGetNumFieldAttribs (
	H5PartFile *f,
	const char *field_name
	) {

	SET_FNAME ( "H5BlockGetNumFieldAttribs" );
	INIT ( f );
	CHECK_TIMEGROUP( f );

	h5part_int64_t herr = _open_field_group ( f, field_name );
	if ( herr < 0 ) return herr;

	h5part_int64_t nattribs = H5Aget_num_attrs (
		f->block->field_group_id );
	if ( nattribs < 0 ) HANDLE_H5A_GET_NUM_ATTRS_ERR;

	herr = _close_field_group ( f );
	if ( herr < 0 ) return herr;

	return nattribs;
}


h5part_int64_t
H5BlockGetFieldAttribInfo (
	H5PartFile *f,
	const char *field_name,
	const h5part_int64_t attrib_idx,
	char *attrib_name,
	const h5part_int64_t len_of_attrib_name,
	h5part_int64_t *attrib_type,
	h5part_int64_t *attrib_nelem
	) {

	SET_FNAME ( "H5BlockGetFieldAttribInfo" );
	INIT ( f );
	CHECK_TIMEGROUP( f );

	h5part_int64_t herr = _open_field_group ( f, field_name );
	if ( herr < 0 ) return herr;

	herr = _H5Part_get_attrib_info (
		f->block->field_group_id,
		attrib_idx,
		attrib_name,
		len_of_attrib_name,
		attrib_type,
		attrib_nelem );
	if ( herr < 0 ) return herr;

	herr = _close_field_group ( f );
	if ( herr < 0 ) return herr;

	return H5PART_SUCCESS;
}


static h5part_int64_t
_read_field_attrib (
	H5PartFile *f,
	const char *field_name,
	const char *attrib_name,
	void *attrib_value
	) {

	struct H5BlockStruct *b = f->block;

	h5part_int64_t herr = _open_field_group ( f, field_name );
	if ( herr < 0 ) return herr;

	herr = _H5Part_read_attrib (
		b->field_group_id,
		attrib_name,
		attrib_value );
	if ( herr < 0 ) return herr;

	herr = _close_field_group ( f );
	if ( herr < 0 ) return herr;

	return H5PART_SUCCESS;
}

h5part_int64_t
H5BlockReadFieldAttrib (
	H5PartFile *f,
	const char *field_name,
	const char *attrib_name,
	void *attrib_value
	) {

	SET_FNAME ( "H5PartReadFieldAttrib" );
	INIT ( f );
	CHECK_TIMEGROUP( f );
	
	return _read_field_attrib (
		f, field_name, attrib_name, attrib_value );
}

h5part_int64_t
H5Block3dGetFieldOrigin (
	H5PartFile *f,
	const char *field_name,
	h5part_float64_t *x_origin,
	h5part_float64_t *y_origin,
	h5part_float64_t *z_origin
	) {

	SET_FNAME ( "H5BlockSetFieldOrigin" );
	INIT ( f );
	CHECK_TIMEGROUP( f );

	h5part_float64_t origin[3];

	h5part_int64_t herr = _read_field_attrib (
		f, field_name, "Origin", origin );

	*x_origin = origin[0];
	*y_origin = origin[1];
	*z_origin = origin[2];
	return herr;
}

h5part_int64_t
H5Block3dSetFieldOrigin (
	H5PartFile *f,
	const char *field_name,
	const h5part_float64_t x_origin,
	const h5part_float64_t y_origin,
	const h5part_float64_t z_origin
	) {

	SET_FNAME ( "H5BlockSetFieldOrigin" );
	INIT ( f );
	CHECK_WRITABLE_MODE( f );
	CHECK_TIMEGROUP( f );

	h5part_float64_t origin[3] = { x_origin, y_origin, z_origin };

	return _write_field_attrib (
		f,
		field_name,
		"Origin", H5PART_FLOAT64, 
		origin,
		3 );
}

h5part_int64_t
H5Block3dGetFieldSpacing (
	H5PartFile *f,
	const char *field_name,
	h5part_float64_t *x_spacing,
	h5part_float64_t *y_spacing,
	h5part_float64_t *z_spacing
	) {

	SET_FNAME ( "H5BlockGetFieldSpacing" );
	INIT ( f );
	CHECK_TIMEGROUP( f );

	h5part_float64_t spacing[3];

	h5part_int64_t herr = _read_field_attrib (
		f, field_name, "Spacing", spacing );

	*x_spacing = spacing[0];
	*y_spacing = spacing[1];
	*z_spacing = spacing[2];
	return herr;
}

h5part_int64_t
H5Block3dSetFieldSpacing (
	H5PartFile *f,
	const char *field_name,
	const h5part_float64_t x_spacing,
	const h5part_float64_t y_spacing,
	const h5part_float64_t z_spacing
	) {

	SET_FNAME ( "H5BlockSetFieldSpacing" );
	INIT ( f );
	CHECK_WRITABLE_MODE( f );
	CHECK_TIMEGROUP( f );

	h5part_float64_t spacing[3] = { x_spacing, y_spacing, z_spacing };

	return _write_field_attrib (
		f,
		field_name,
		"Spacing", H5PART_FLOAT64, 
		spacing,
		3 );
}

/*
  Checks whether the current time-step has field data or not.

  Returns 0 if field data is available otherwise H5PART_ERR_NOENTRY.
*/
h5part_int64_t
H5BlockHasFieldData (
	H5PartFile *f
	) {

	SET_FNAME ( "H5BlockHasFieldData" );
	INIT ( f );
	CHECK_TIMEGROUP( f );

	if ( ! _have_object ( f->timegroup, H5BLOCK_GROUP_NAME ) ) {
		return H5PART_ERR_NOENTRY;
	}
	return H5PART_SUCCESS;
}
