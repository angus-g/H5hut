/*!
  \defgroup h5multiblock_c_api H5MultiBlock C API

  This package includes C code for writing and reading 3D datasets intended
  for distributed memory applications. The underlying file format is H5Part,
  a simple data storage schema and API derived from HDF5. In particular, we
  use the H5Block subset of H5Part for managing 3D field data. To achieve large
  contiguous reads and writes, the datasets use HDF5's chunking mechanism to
  reorder the layout on disk such that local subfields (or "blocks") of the
  3D field are stored contiguously (as opposed to storing an entire column
  contiguously, as in the Fortran column-major layout for an array). We refer
  to this chunked layout as "multiblock" and the column-major layout as
  "uniblock." Multiblocked datasets exhibit the following constraints:

  * All blocks must be the same size (a requirement of the HDF5 chunking
    mechanism).
  * The block dimensions must divide the field dimensions, to prevent fringe
    data that will lower performance by interfering with contiguous I/O
    operations.
  * For load-balancing purposes, the total number of blocks that the field
    is decomposed into must be a multiple of the number of nodes in the
    distributed system. (For ease of implementation, we require that the
    number of blocks equal the number of nodes.)

*/

/*!
  \internal

  \defgroup h5multiblock_static H5MultiBlock Static
*/

/*!
  \internal

  \defgroup h5multiblock_private H5MultiBlock Private
*/

#include <stdlib.h>
#include <string.h>
#include <math.h>

#include <hdf5.h>
#include "H5Part.h"
#include "H5PartErrors.h"
#include "H5PartPrivate.h"

#include "H5Block.h"
#include "H5BlockTypes.h"
#include "H5BlockErrors.h"
#include "H5BlockPrivate.h"

#include "H5MultiBlock.h"
#include "H5MultiBlockTypes.h"
#include "H5MultiBlockErrors.h"
#include "H5MultiBlockPrivate.h"

#ifdef PARALLEL_IO


/*******************************************************************************
* Static functions
*******************************************************************************/

/*!
  \ingroup h5multiblock_static

  \internal

  Check whether \c f points to a valid file handle.

  \return	H5PART_SUCCESS or error code
*/
static h5part_int64_t
_file_is_valid (
	const H5PartFile *f		/*!< IN: file handle */
	) {

	if ( f == NULL )
		return H5PART_ERR_BADFD;
	if ( f->file == 0 )
		return H5PART_ERR_BADFD;
	if ( f->block == NULL )
		return H5PART_ERR_BADFD;
	if ( f->multiblock == NULL )
		return H5PART_ERR_BADFD;
	return H5PART_SUCCESS;
}

/*!
  \ingroup h5multiblock_static

  \internal

  Gets the block decomposition and offsets.

  \return	H5PART_SUCCESS or error code
*/
static h5part_int64_t
_get_decomp_and_offsets (
	const H5PartFile *f		/*!< IN: file handle */
	) {

	h5part_int64_t i, j, k;
	h5part_int64_t nblocks;

	struct H5MultiBlockStruct *mb = f->multiblock;

	mb->decomp[0] = mb->field_dims[0] / mb->block_dims[0];
	mb->decomp[1] = mb->field_dims[1] / mb->block_dims[1];
	mb->decomp[2] = mb->field_dims[2] / mb->block_dims[2];

	_H5Part_print_debug ("PROC[%d]: Block decomposition: (%ld,%ld,%ld)",
			f->myproc,
			mb->decomp[0],
			mb->decomp[1],
			mb->decomp[2] );

	i = f->myproc % mb->decomp[0];
	j = (f->myproc / mb->decomp[0]) % mb->decomp[1];
	k = f->myproc / (mb->decomp[0] * mb->decomp[1]);

	/* keep track of blocks that border the edges of the field */
	if (i == 0)			mb->field_edges | H5MB_EDGE_X0;
	if (i == mb->decomp[0] - 1)	mb->field_edges | H5MB_EDGE_X1;
	if (j == 0)			mb->field_edges | H5MB_EDGE_Y0;
	if (j == mb->decomp[1] - 1)	mb->field_edges | H5MB_EDGE_Y1;
	if (j == 0)			mb->field_edges | H5MB_EDGE_Z0;
	if (j == mb->decomp[2] - 1)	mb->field_edges | H5MB_EDGE_Z1;

	mb->offsets[0] = i * mb->block_dims[0];
	mb->offsets[1] = j * mb->block_dims[1];
	mb->offsets[2] = k * mb->block_dims[2];

	_H5Part_print_debug ("PROC[%d]: Block offsets: (%ld,%ld,%ld)",
			f->myproc,
			mb->offsets[0],
			mb->offsets[1],
			mb->offsets[0] );

	nblocks = mb->decomp[0] * mb->decomp[1] * mb->decomp[2];

	if (f->myproc == 0) {
		_H5Part_print_info ("Number of blocks: %ld", nblocks);
	}

	if (nblocks != f->nprocs) {
		return HANDLE_H5PART_BLOCK_DECOMP_ERR;
	}

	return H5PART_SUCCESS;
}

/*!
  \ingroup h5multiblock_static

  \internal

  Calculates an integer \c divisor of \c m that is >= the \c n root of \c m.
  
  Used for finding block decompositions for an arbitrary number of processors.

  \return \c divisor
*/
static int
_nth_root_int_divisor (const int m, const int n)
{
	int i, root;
	double p;

	p = 1.0 / (double) n;
	root = (int) ceil ( pow ((double) m, p) );
	for (i=root; i<=m; i++)
	{
		if (m % i == 0) return i;
	}

	return i;
}

/*!
  \ingroup h5multiblock_static

  \internal

  Allocates a block using the dimensions read from the file and the halo
  radii specified by the user.

  \return	H5PART_SUCCESS or error code
*/
static h5part_int64_t
_alloc_block (
	const H5PartFile *f,		/*!< IN: file handle */
	char **data,			/*!< IN/OUT: buffer to alloc */
	hid_t type			/*!< IN: HDF5 datatype of buffer */
	) {

	char *buffer;	
	size_t datasize;
	size_t nelems;

	struct H5MultiBlockStruct *mb = f->multiblock;

	/* size of datatype */
	datasize = H5Tget_size ( type );

	/* number of elements */
	nelems  = mb->block_dims[0] + 2*mb->halo_radii[0];
	nelems *= mb->block_dims[1] + 2*mb->halo_radii[1];
	nelems *= mb->block_dims[2] + 2*mb->halo_radii[2];

	buffer = (char*) malloc ( nelems * datasize );
	if ( ! buffer ) return HANDLE_H5PART_NOMEM_ERR;

	*data = buffer;

	return H5PART_SUCCESS;
}

/*!
  \ingroup h5multiblock_static

  \internal

  Takes a contiguous buffer of data and rearranges it in place to add padding
  with \c radius layers. Assumes that \c buffer is already large enough to
  perform this operation.

  \return	H5PART_SUCCESS or error code
*/
static h5part_int64_t
_pad_block (
	const H5PartFile *f,		/*!< IN: file handle */
	char *data,			/*!< IN/OUT: buffer to pad */
	hid_t type			/*!< IN: HDF5 datatype of buffer */
	) {

	size_t datasize;
	h5part_int64_t j, k;
	h5part_int64_t iDst, iSrc;
	h5part_int64_t xSize, xySize;
	h5part_int64_t hxSize, hxySize;
	h5part_int64_t hxInset;

	struct H5MultiBlockStruct *mb = f->multiblock;

	/* size of datatype */
	datasize = H5Tget_size ( type );

	/* size of row in original block */
	xSize = mb->block_dims[0] * datasize;

	/* size of slab in original block */
	xySize = xSize * mb->block_dims[1];

	/* size of row/slab with halo regions */
	hxSize = (mb->block_dims[0] + 2*mb->halo_radii[0]) * datasize;
	hxySize = hxSize * (mb->block_dims[1] + 2*mb->halo_radii[1]);

	/* inset of row in halo region */
	hxInset = mb->halo_radii[0] * datasize;
	
	for (k=(mb->block_dims[2]-1);k>=0;k--)
	{
		for (j=(mb->block_dims[1]-1);j>=0;j--)
		{
			iSrc =	k*xySize + j*xSize;

			iDst =	(k + mb->halo_radii[2]) * hxySize +
				(j + mb->halo_radii[1]) * hxSize +
				hxInset;

			memcpy ( data + iDst, data + iSrc, xSize );
		}
	}

	return H5PART_SUCCESS;
}

/*!
  \ingroup h5multiblock_static

  \internal

  Exchanges halo regions among neighboring blocks using MPI.

  \return	H5PART_SUCCESS or error code
*/
static h5part_int64_t
_halo_exchange (
	const H5PartFile *f,		/*!< IN: file handle */
	char *data,			/*!< IN/OUT: local buffer */
	hid_t type			/*!< IN: HDF5 datatype of buffer */
	) {

	return H5PART_SUCCESS;
}

/*******************************************************************************
* Private functions
*******************************************************************************/

/*!
  \ingroup h5multiblock_private

  \internal

  Initialize H5MultiBlock internal structure.

  \return	H5PART_SUCCESS or error code
*/
h5part_int64_t
_H5MultiBlock_init (
	H5PartFile *f			/*!< IN: file handle */
	) {

	struct H5MultiBlockStruct *mb;

	BLOCK_INIT( f );

	if (f->multiblock != NULL) return H5PART_SUCCESS;

	f->multiblock =
		(struct H5MultiBlockStruct*) malloc( sizeof (*f->multiblock) );
	if ( f->multiblock == NULL ) {
		return HANDLE_H5PART_NOMEM_ERR;
	}
	mb = f->multiblock;

	mb->field_edges = 0;
	mb->halo = 0;
	mb->read = 0;
	mb->have_decomp = 0;

	return H5PART_SUCCESS;
}

/*!
  \ingroup h5multiblock_private

  \internal

  Free H5MultiBlock internal struct;

  \return	H5PART_SUCCESS or error code
*/
h5part_int64_t
_H5MultiBlock_close (
	H5PartFile *f		/*!< IN: file handle */
	) {

	free ( f->multiblock );
	f->multiblock = NULL;
	f->close_multiblock = NULL;

	return H5PART_SUCCESS;
}

/*!
  \ingroup h5multiblock_private

  \internal

  Read a multiblock field \c name into the buffer starting at \c data
  using the current time-step and the block decomposition and dimensions
  defined in the file.

  \return	H5PART_SUCCESS or error code
*/
h5part_int64_t
_H5MultiBlock_read_data (
	H5PartFile *f,			/*!< IN: file handle */
	const char *field_name,		/*!< IN: Name of field */
	char **data,			/*!< OUT: ptr to read buffer ptr*/
	hid_t type			/*!< IN: HDF5 datatype of buffer */
	) {

        MULTIBLOCK_INIT( f );

	hid_t dataset_id;
	hid_t dataspace_id;
	int rank;
	h5part_int64_t herr;
	
	struct H5BlockStruct *b = f->block;
	struct H5MultiBlockStruct *mb = f->multiblock;

	char * const fname = _H5Part_get_funcname();

	herr = _H5Block_open_field_group ( f, field_name );
	if ( herr < 0 ) return herr;

#if H5_VERS_MAJOR == 1 && H5_VERS_MINOR == 8
	dataset_id = H5Dopen2 ( b->field_group_id, "0", H5P_DEFAULT );
#else
	dataset_id = H5Dopen ( b->field_group_id, "0" );
#endif
	if ( dataset_id < 0 ) return HANDLE_H5D_OPEN_ERR ( "0" );

	/* read block dimensions from field attribute */
	herr = _H5Part_read_attrib (
		b->field_group_id,
		H5MULTIBLOCK_ATTR_NAME,
		mb->block_dims );
	if ( herr < 0 ) return herr;

	_H5Part_print_debug ("PROC[%d]: Block dimensions: (%ld,%ld,%ld)",
			f->myproc,
			mb->block_dims[0],
			mb->block_dims[1],
			mb->block_dims[2] );

 	dataspace_id = H5Dget_space ( dataset_id );
	if ( dataspace_id < 0 ) return HANDLE_H5D_GET_SPACE_ERR;

	rank = H5Sget_simple_extent_dims (
			dataspace_id,
			(hsize_t*)mb->field_dims,
			NULL );
	if ( rank < 0 )  return HANDLE_H5S_GET_SIMPLE_EXTENT_DIMS_ERR;
	if ( rank != 3 ) return HANDLE_H5PART_DATASET_RANK_ERR ( rank, 3 );

	herr = _alloc_block ( f, data, type );
	if ( herr < 0 ) return herr;

	herr = _get_decomp_and_offsets ( f );
	if ( herr < 0 ) return herr;

	mb->have_decomp = 1;

	herr = H5BlockDefine3DFieldLayout ( f,
		mb->offsets[0], mb->offsets[0] + mb->block_dims[0] - 1,
		mb->offsets[1], mb->offsets[1] + mb->block_dims[1] - 1,
		mb->offsets[2], mb->offsets[2] + mb->block_dims[2] - 1);
	if ( herr < 0 ) return herr;

	_H5Part_set_funcname ( fname );

	herr = _H5Block_select_hyperslab_for_reading ( f, dataset_id );
	if ( herr < 0 ) return herr;

	herr = H5Dread ( 
		dataset_id,
		type,
		f->block->memshape,
		f->block->diskshape,
		f->xfer_prop,
		*data );
	if ( herr < 0 ) return HANDLE_H5D_READ_ERR ( field_name, f->timestep );

	herr = H5Dclose ( dataset_id );
	if ( herr < 0 ) return HANDLE_H5D_CLOSE_ERR;

	herr = _H5Block_close_field_group ( f );
	if ( herr < 0 ) return herr;

	if ( mb->halo ) {
		_pad_block ( f, *data, type );
		_halo_exchange ( f, *data, type );
	}

	mb->read = 1;

	return H5PART_SUCCESS;
}

/*!
  \ingroup h5multiblock_private

  \internal

  Write a multiblock field \c name from the buffer starting at \c data
  to the current time-step using the defined block decomposition and dimensions.

  \return	H5PART_SUCCESS or error code
*/
h5part_int64_t
_H5MultiBlock_write_data (
	H5PartFile *f,          /*!< IN: file handle */
	const char *name,       /*!< IN: name of dataset to write */
	const void* data,	/*!< IN: data buffer */
	hid_t type		/*!< IN: HDF5 datatype */
	) {

        MULTIBLOCK_INIT( f );
        CHECK_WRITABLE_MODE( f );
        CHECK_TIMEGROUP( f );
	CHECK_DECOMP ( f );

	hid_t dataset;
	h5part_int64_t herr;

	struct H5BlockStruct *b = f->block;
	struct H5MultiBlockStruct *mb = f->multiblock;

	char * const fname = _H5Part_get_funcname();

	herr = H5BlockDefine3DFieldLayout ( f,
		mb->offsets[0], mb->offsets[0] + mb->block_dims[0] - 1,
		mb->offsets[1], mb->offsets[1] + mb->block_dims[1] - 1,
		mb->offsets[2], mb->offsets[2] + mb->block_dims[2] - 1);
	if ( herr < 0 ) return herr;

	herr = H5BlockDefine3DChunkDims( f,
			mb->block_dims[0],
			mb->block_dims[1],
			mb->block_dims[2]);
	if ( herr < 0 ) return herr;

	_H5Part_set_funcname ( fname );

	herr = _H5Block_create_field_group ( f, name );
	if ( herr < 0 ) return herr;

#if H5_VERS_MAJOR == 1 && H5_VERS_MINOR == 8
	dataset = H5Dcreate2 (
		b->field_group_id,
		"0",
		type,
		b->shape,
		H5P_DEFAULT,
		b->create_prop,
		H5P_DEFAULT );
#else
	dataset = H5Dcreate (
		b->field_group_id,
		"0",
		type,
		b->shape, 
		b->create_prop );
#endif
	if ( dataset < 0 ) return HANDLE_H5D_CREATE_ERR ( name, f->timestep );

	herr = H5Dwrite ( 
		dataset,
		type,
		b->memshape,
		b->diskshape,
		f->xfer_prop,
		data );
	if ( herr < 0 ) return HANDLE_H5D_WRITE_ERR ( name, f->timestep );

	herr = H5Dclose ( dataset );
	if ( herr < 0 ) return HANDLE_H5D_CLOSE_ERR;

	/* write out the block dimensions to a special field attribute */
	herr = _H5Part_write_attrib (
                f->block->field_group_id,
                H5MULTIBLOCK_ATTR_NAME,
                H5T_NATIVE_INT64,
                mb->block_dims,
                3 );
        if ( herr < 0 ) return herr;

	herr = _H5Block_close_field_group ( f );
	if ( herr < 0 ) return herr;

	return H5PART_SUCCESS;
}


/*******************************************************************************
* Public API
*******************************************************************************/

/*!
  \ingroup h5multiblock_c_api

  Define the radius for halo exchanges between the blocks. Blocks on the edges
  of the field will be padded with zero values out to the radius.

  \return \c H5PART_SUCCESS on success
*/
h5part_int64_t
H5MultiBlock3dDefineRadius (
	H5PartFile *f,		/* IN: file handle */
	const h5part_int64_t r	/* IN: radius for i, j and k directions*/
	) {

	SET_FNAME ( "H5MultiBlock3dDefineRadius" );
	MULTIBLOCK_INIT( f );

	struct H5MultiBlockStruct *mb = f->multiblock;

	mb->halo_radii[0] = 
	mb->halo_radii[1] =
	mb->halo_radii[2] = r;

	mb->halo = 1;

	return H5PART_SUCCESS;
}

/*!
  \ingroup h5multiblock_c_api

  Define the radii for halo exchanges between the blocks. Blocks on the edges
  of the field will be padded with zero values out to the radius.

  Different radii can be set for each direction.

  \return \c H5PART_SUCCESS on success
*/
h5part_int64_t
H5MultiBlock3dDefineRadii (
	H5PartFile *f,			/* IN: file handle */
	const h5part_int64_t ri,	/* IN: radius for i direction */
	const h5part_int64_t rj,	/* IN: radius for j direction */
	const h5part_int64_t rk		/* IN: radius for k direction */
	) {

	SET_FNAME ( "H5MultiBlock3dDefineRadii" );
	MULTIBLOCK_INIT( f );

	struct H5MultiBlockStruct *mb = f->multiblock;

	mb->halo_radii[0] = rk;
	mb->halo_radii[1] = rj;
	mb->halo_radii[2] = ri;

	mb->halo = 1;

	return H5PART_SUCCESS;
}

/*!
  \ingroup h5multiblock_c_api

  Define the field and block dimensions for writing.

  \return \c H5PART_SUCCESS on success
  	  \c H5PART_ERR_INVAL if block dims do not divide field dims
*/
h5part_int64_t
H5MultiBlock3dDefineDims (
        H5PartFile *f,				/* IN: file handle */
        const h5part_int64_t *field_dims,	/* IN: field dimensions */
        const h5part_int64_t *block_dims	/* IN: block dimensions */
        ) {

	SET_FNAME ( "H5MultiBlock3dDefineDims" );
	MULTIBLOCK_INIT ( f );

	struct H5MultiBlockStruct *mb = f->multiblock;

	mb->field_dims[0] = field_dims[0];
	mb->field_dims[1] = field_dims[1];
	mb->field_dims[2] = field_dims[2];

	mb->block_dims[0] = block_dims[0];
	mb->block_dims[1] = block_dims[1];
	mb->block_dims[2] = block_dims[2];

	h5part_int64_t herr = _get_decomp_and_offsets ( f );
	if ( herr < 0 ) return H5PART_ERR_INVAL;

	mb->have_decomp = 1;

	return H5PART_SUCCESS;
}

/*!
  \ingroup h5multiblock_c_api

  Returns the field dimensions of the last field that was read.

  \return \c H5PART_SUCCESS on success<br>
  	  \c H5PART_ERR_INVAL if no field has been read yet

*/
h5part_int64_t
H5MultiBlock3dGetFieldDims(
	H5PartFile *f,
	h5part_int64_t *dims
	) {

	SET_FNAME ( "H5MultiBlock3dGetFieldDims" );
	MULTIBLOCK_INIT ( f );

	struct H5MultiBlockStruct *mb = f->multiblock;

	if ( ! mb->read ) return H5PART_ERR_INVAL;

	dims[0] = mb->field_dims[0];
	dims[1] = mb->field_dims[1];
	dims[2] = mb->field_dims[2];

	return H5PART_SUCCESS;
}

/*!
  \ingroup h5multiblock_c_api

  Returns the block dimensions of the last field that was read.

  \return \c H5PART_SUCCESS on success<br>
  	  \c H5PART_ERR_INVAL if no field has been read yet

*/
h5part_int64_t
H5MultiBlock3dGetBlockDims(
	H5PartFile *f,          /* IN: file handle */
        const char *field_name, /* IN: field name */
	h5part_int64_t *dims    /* OUT: block dimensions */
	) {

	SET_FNAME ( "H5MultiBlock3dGetBlockDims" );
	MULTIBLOCK_INIT ( f );

	struct H5MultiBlockStruct *mb = f->multiblock;

	if ( ! mb->read ) return H5PART_ERR_INVAL;

	dims[0] = mb->block_dims[0];
	dims[1] = mb->block_dims[1];
	dims[2] = mb->block_dims[2];

	return H5PART_SUCCESS;
}

/*!
  \ingroup h5multiblock_c_api

  Return the offsets for the block belonging to processor \c proc.

  \return \c H5PART_SUCCESS on success<br>
	  \c H5PART_ERR_INVAL if proc is invalid
*/
h5part_int64_t
H5MultiBlock3dGetOffsetsOfProc (
	H5PartFile *f,			/* IN: file handle */
	const h5part_int64_t proc,	/* IN: processor number */
	h5part_int64_t *offsets		/* OUT: 3d array of offsets */
	) { 

	SET_FNAME ( "H5MultiBlock3dGetOffsetsOfProc" );
	MULTIBLOCK_INIT ( f );

	if ( ( proc < 0 ) || ( proc >= f->nprocs ) )
		return H5PART_ERR_INVAL;

	struct H5MultiBlockStruct *mb = f->multiblock;

	offsets[0] = mb->offsets[0];
	offsets[1] = mb->offsets[1];
	offsets[2] = mb->offsets[2];

	return H5PART_SUCCESS;
}

/*!
  \ingroup h5multiblock_c_api

  Finds a 3D block decomposition for an arbitrary number of processors
  \c nprocs.

  \return \c H5PART_SUCCESS on success<br>
	  \c H5PART_ERR_INVAL if the decomp doesn't have \c nprocs blocks
*/

h5part_int64_t
H5MultiBlock3dCalculateDecomp (
	const int nprocs,	/*!< IN: number of processors/blocks */
	h5part_int64_t *decomp	/*!< OUT: 3D block decomposition */
	) {

	decomp[0] = _nth_root_int_divisor (nprocs, 3);
	decomp[1] = _nth_root_int_divisor (nprocs / decomp[0], 2);
	decomp[2] = nprocs / decomp[0] / decomp[1];

	if (decomp[0] * decomp[1] * decomp[2] != nprocs) {
		return H5PART_ERR_INVAL;
	}

	return H5PART_SUCCESS;
}

#endif

