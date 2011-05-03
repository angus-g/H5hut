#ifndef __H5_ERRNO_H
#define __H5_ERRNO_H

#define H5_SUCCESS		0
#define H5_OK			H5_SUCCESS
#define H5_NOK			-1
#define H5_ERR			-2
#define H5_ERR_BADF		-9
#define H5_ERR_NOMEM		-12
#define H5_ERR_INVAL		-22
#define H5_ERR_BADFD		-77

#define H5_ERR_LAYOUT		-100
#define H5_ERR_NOENTRY		-101

#define H5_ERR_MPI		-201
#define H5_ERR_HDF5		-202
#define H5_ERR_H5		-203
#define H5_ERR_H5PART		-204
#define H5_ERR_H5BLOCK		-205
#define H5_ERR_H5FED		-206

#define H5_ERR_INTERNAL		-253
#define H5_ERR_NOT_IMPLEMENTED	-254

#endif
