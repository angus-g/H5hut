#include <stdio.h>
#include <stdlib.h>
#include <hdf5.h>
#include "H5Part.h"
#include "H5Block.h"
#include "H5BlockTypes.h"

static int _debug = 4;

void
_H5Part_vprint_error (
	const char *fmt,
	va_list ap
	) {

	if ( _debug < 1 ) return;
	vfprintf ( stderr, fmt, ap );
	fprintf ( stderr, "\n" );
}

void
_H5Part_print_error (
	const char *fmt,
	...
	) {

	va_list ap;
	va_start ( ap, fmt );
	_H5Part_vprint_error ( fmt, ap );
	va_end ( ap );
}

void
_H5Part_vprint_warn (
	const char *fmt,
	va_list ap
	) {

	if ( _debug < 2 ) return;
	vfprintf ( stderr, fmt, ap );
	fprintf ( stderr, "\n" );
}

void
_H5Part_print_warn (
	const char *fmt,
	...
	) {

	va_list ap;
	va_start ( ap, fmt );
	_H5Part_vprint_warn ( fmt, ap );
	va_end ( ap );
}

void
_H5Part_vprint_info (
	const char *fmt,
	va_list ap
	) {

	if ( _debug < 3 ) return;
	vfprintf ( stdout, fmt, ap );
	fprintf ( stdout, "\n" );
}

void
_H5Part_print_info (
	const char *fmt,
	...
	) {

	va_list ap;
	va_start ( ap, fmt );
	_H5Part_vprint_info ( fmt, ap );
	va_end ( ap );
}

void
_H5Part_vprint_debug (
	const char *fmt,
	va_list ap
	) {

	if ( _debug < 4 ) return;
	vfprintf ( stdout, fmt, ap );
	fprintf ( stdout, "\n" );
}

void
_H5Part_print_debug (
	const char *fmt,
	...
	) {

	va_list ap;
	va_start ( ap, fmt );
	_H5Part_vprint_debug ( fmt, ap );
	va_end ( ap );
}


struct H5BlockPartition Layout1[1] = {
	{ 0, 63, 0, 63, 0, 511 }
};

struct H5BlockPartition Layout8[8] = {
		{  0,63,  0,63,   0, 63},
		{  0,63,  0,63,  64,127},
		{  0,63,  0,63, 128,191},
		{  0,63,  0,63, 192,255},
		{  0,63,  0,63, 256,319},
		{  0,63,  0,63, 320,383},
		{  0,63,  0,63, 384,447},
		{  0,63,  0,63, 448,511}
};

struct H5BlockPartition Layout8G[8] = {
		{  0,63,  0,63,   0, 64},
		{  0,63,  0,63,  63,128},
		{  0,63,  0,63, 127,192},
		{  0,63,  0,63, 191,256},
		{  0,63,  0,63, 255,320},
		{  0,63,  0,63, 319,384},
		{  0,63,  0,63, 383,448},
		{  0,63,  0,63, 447,511}
};

struct H5BlockPartition Layout16[16] = {
		{  0,63,  0,31,   0, 63},
		{  0,63, 32,63,   0, 63},
		{  0,63,  0,31,  64,127},
		{  0,63, 32,63,  64,127},
		{  0,63,  0,31, 128,191},
		{  0,63, 32,63, 128,191},
		{  0,63,  0,31, 192,255},
		{  0,63, 32,63, 192,255},
		{  0,63,  0,31, 256,319},
		{  0,63, 32,63, 256,319},
		{  0,63,  0,31, 320,383},
		{  0,63, 32,63, 320,383},
		{  0,63,  0,31, 384,447},
		{  0,63, 32,63, 384,447},
		{  0,63,  0,31, 448,511},
		{  0,63, 32,63, 448,511}
};

struct H5BlockPartition Layout16G[16] = {
		{  0,63,  0,32,   0, 64},
		{  0,63, 31,63,   0, 64},
		{  0,63,  0,32,  63,128},
		{  0,63, 31,63,  63,128},
		{  0,63,  0,32, 127,192},
		{  0,63, 31,63, 127,192},
		{  0,63,  0,32, 191,256},
		{  0,63, 31,63, 191,256},
		{  0,63,  0,32, 255,320},
		{  0,63, 31,63, 255,320},
		{  0,63,  0,32, 319,384},
		{  0,63, 31,63, 319,384},
		{  0,63,  0,32, 383,448},
		{  0,63, 31,63, 383,448},
		{  0,63,  0,32, 447,511},
		{  0,63, 31,63, 447,511}
};


struct H5BlockPartition Layout32[32] = {
		{  0,31,  0,31,   0, 63},
		{  0,31, 32,63,   0, 63},
		{ 32,63,  0,31,   0, 63},
		{ 32,63, 32,63,   0, 63},
		{  0,31,  0,31,  64,127},
		{  0,31, 32,63,  64,127},
		{ 32,63,  0,31,  64,127},
		{ 32,63, 32,63,  64,127},
		{  0,31,  0,31, 128,191},
		{  0,31, 32,63, 128,191},
		{ 32,63,  0,31, 128,191},
		{ 32,63, 32,63, 128,191},
		{  0,31,  0,31, 192,255},
		{  0,31, 32,63, 192,255},
		{ 32,63,  0,31, 192,255},
		{ 32,63, 32,63, 192,255},
		{  0,31,  0,31, 256,319},
		{  0,31, 32,63, 256,319},
		{ 32,63,  0,31, 256,319},
		{ 32,63, 32,63, 256,319},
		{  0,31,  0,31, 320,383},
		{  0,31, 32,63, 320,383},
		{ 32,63,  0,31, 320,383},
		{ 32,63, 32,63, 320,383},
		{  0,31,  0,31, 384,447},
		{  0,31, 32,63, 384,447},
		{ 32,63,  0,31, 384,447},
		{ 32,63, 32,63, 384,447},
		{  0,31,  0,31, 448,511},
		{  0,31, 32,63, 448,511},
		{ 32,63,  0,31, 448,511},
		{ 32,63, 32,63, 448,511}
};

struct H5BlockPartition Layout32G[32] = {
		{  0,32,  0,32,   0, 64},
		{  0,32, 31,63,   0, 64},
		{ 31,63,  0,32,   0, 64},
		{ 31,63, 31,63,   0, 64},
		{  0,32,  0,32,  63,128},
		{  0,32, 31,63,  63,128},
		{ 31,63,  0,32,  63,128},
		{ 31,63, 31,63,  63,128},
		{  0,32,  0,32, 127,192},
		{  0,32, 31,63, 127,192},
		{ 31,63,  0,32, 127,192},
		{ 31,63, 31,63, 127,192},
		{  0,32,  0,32, 191,256},
		{  0,32, 31,63, 191,256},
		{ 31,63,  0,32, 191,256},
		{ 31,63, 31,63, 191,256},
		{  0,32,  0,32, 255,320},
		{  0,32, 31,63, 255,320},
		{ 31,63,  0,32, 255,320},
		{ 31,63, 31,63, 255,320},
		{  0,32,  0,32, 319,384},
		{  0,32, 31,63, 319,384},
		{ 31,63,  0,32, 319,384},
		{ 31,63, 31,63, 319,384},
		{  0,31,  0,31, 383,448},
		{  0,31, 31,63, 383,448},
		{ 31,63,  0,31, 383,448},
		{ 31,63, 31,63, 383,448},
		{  0,32,  0,32, 447,511},
		{  0,32, 31,63, 447,511},
		{ 31,63,  0,32, 447,511},
		{ 31,63, 31,63, 447,511}
};



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

#if 0
static h5part_int64_t
_dissolve_ghostzone_max_volume_of_dissolved_partitions (
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
		vol = _volume_of_partition ( &p_ ) + _volume_of_partition ( &q_ );
		if ( vol > max_vol ) {
			max_vol = vol;
			p_best = p_;
			q_best = q_;
		}
	}

	p_ = *p;
	q_ = *q;
	if ( _dissolve_Y_ghostzone ( &p_, &q_ ) == 0 ) {
		vol = _volume_of_partition ( &p_ ) + _volume_of_partition ( &q_ );
		if ( vol > max_vol ) {
			max_vol = vol;
			p_best = p_;
			q_best = q_;
		}
	}
	p_ = *p;
	q_ = *q;

	if ( _dissolve_Z_ghostzone ( &p_, &q_ ) == 0 ) {
		vol = _volume_of_partition ( &p_ ) + _volume_of_partition ( &q_ );
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


static h5part_int64_t
_dissolve_ghostzones_max_volume_of_dissolved_partitions (
	H5PartFile *f
	) {

	h5part_int64_t herr;
	struct H5BlockStruct *b = f->block;
	struct H5BlockPartition *part1;
	struct H5BlockPartition *part2;
	int proc1, proc2;

	memcpy ( b->write_layout, b->user_layout,
		 f->nprocs * sizeof (*f->block->user_layout) );

	for ( proc1 = 0, part1 = b->write_layout;
	      proc1 < f->nprocs-1;
	      proc1++, part1++ ) {
		for ( proc2 = proc1+1, part2 = &b->write_layout[proc2];
		      proc2 < f->nprocs;
		      proc2++, part2++ ) {

			if ( ! _have_ghostzone ( part1, part2 ) )
				continue;

			herr = _dissolve_ghostzone_max_volume_of_dissolved_partitions (
				part1, part2 );
			if ( herr < 0 ) return herr;
		}
	}
	_H5Part_print_debug ("Layout after dissolving ghost-zones:");
	for ( proc1 = 0, part1 = b->write_layout;
	      proc1 < f->nprocs;
	      proc1++, part1++ ) {
		_H5Part_print_debug (
			"PROC[%d]: proc[%d]: %lld:%lld, %lld:%lld, %lld:%lld  ",
			f->myproc, proc1,
			(long long)part1->i_start,
			(long long)part1->i_end,
			(long long)part1->j_start,
			(long long)part1->j_end,
			(long long)part1->k_start,
			(long long)part1->k_end );
	}
	return H5PART_SUCCESS;
}
#endif

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
		vol = _volume_of_partition ( &p_ ) + _volume_of_partition ( &q_ );
		if ( vol > max_vol ) {
			max_vol = vol;
			p_best = p_;
			q_best = q_;
		}
	}

	p_ = *p;
	q_ = *q;
	if ( _dissolve_Y_ghostzone ( &p_, &q_ ) == 0 ) {
		vol = _volume_of_partition ( &p_ ) + _volume_of_partition ( &q_ );
		if ( vol > max_vol ) {
			max_vol = vol;
			p_best = p_;
			q_best = q_;
		}
	}
	p_ = *p;
	q_ = *q;

	if ( _dissolve_Z_ghostzone ( &p_, &q_ ) == 0 ) {
		vol = _volume_of_partition ( &p_ ) + _volume_of_partition ( &q_ );
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

static h5part_int64_t
_dissolve_ghostzones (
	H5PartFile *f
	) {

	struct H5BlockStruct *b = f->block;
	struct H5BlockPartition *p, *max_p;
	struct H5BlockPartition *q, *max_q;
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

	_H5Part_print_debug ("Layout after dissolving ghost-zones:");
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

static H5PartFile*
_init (
	void
	) {

	H5PartFile *f;
	struct H5BlockStruct *b; 


	f = (H5PartFile*) malloc( sizeof (H5PartFile) );
	if( f == NULL ) {
		return NULL;
	}
	memset (f, 0, sizeof (H5PartFile));
	f->nprocs = 32;

	f->block = malloc( sizeof (*f->block) );
	if ( f->block == NULL ) {
		return NULL;
	}
	b = f->block;
	memset ( b, 0, sizeof (*b) );
	b->user_layout = malloc ( f->nprocs * sizeof (b->user_layout[0]) );
	if ( b->user_layout == NULL ) {
		return NULL;
	}
	b->write_layout = malloc ( f->nprocs * sizeof (b->write_layout[0]) );
	if ( b->write_layout == NULL ) {
		return NULL;
	}
	b->timestep = -1;
	b->blockgroup = -1;
	b->shape = -1;
	b->diskshape = -1;
	b->memshape = -1;
	b->field_group_id = -1;

	return f;
}



int
main (
	int argc,
	char *argv[]
	) {

	H5PartFile *f = _init ();

	memcpy ( f->block->user_layout, Layout32G,
		 f->nprocs * sizeof (*f->block->user_layout) );

	_dissolve_ghostzones ( f );

	return 0;
}
