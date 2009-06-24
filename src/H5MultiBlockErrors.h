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

#endif
