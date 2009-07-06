#ifndef __H5MULTIBLOCKERRORS_H
#define __H5MULTIBLOCKERRORS_H

#define _err_handler H5PartGetErrorHandler()

#define CHECK_DECOMP( f ) \
	if ( ! f->multiblock->have_decomp ) \
		return (*_err_handler) ( \
			_H5Part_get_funcname(), \
			H5PART_ERR_DECOMP, \
			"No dimensions defined." )

#define HANDLE_H5PART_BLOCK_DECOMP_ERR \
	 (*_err_handler) ( \
		_H5Part_get_funcname(), \
		H5PART_ERR_DECOMP, \
		"Number of blocks does not equal number of procs" );

#define HANDLE_MPI_TYPE_ERR \
	 (*_err_handler) ( \
		_H5Part_get_funcname(), \
		H5PART_ERR_MPI, \
		"Cannot create/commit/free strided vector MPI datatype." );

#define HANDLE_MPI_SENDRECV_ERR \
	 (*_err_handler) ( \
		_H5Part_get_funcname(), \
		H5PART_ERR_MPI, \
		"Unable to perform point-to-point MPI send/receive." );

#define HANDLE_MPI_INT64_ERR \
	 (*_err_handler) ( \
		_H5Part_get_funcname(), \
		H5PART_ERR_MPI, \
		"Integer overslow error! An offset/count/stride/etc. value " \
		"passed to an MPI function exceeds the 32-bit limit imposed " \
		"by the MPI standard." );

#endif
