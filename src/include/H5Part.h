/*
  Header file for declaring the H5Fed application programming
  interface (API) in the C language.
  
  Copyright 2006-2007
 	Paul Scherrer Institut, Villigen, Switzerland;
 	Benedikt Oswald;
 	Achim Gsell
 	All rights reserved.
 
  Authors
 	Achim Gsell
  
  Warning
	This code is under development.
 
 */
#ifndef __H5Part_H
#define __H5Part_H

#include <hdf5.h>

#ifdef __cplusplus
extern "C" {
#endif

#include "h5core/h5_core.h"
#include "H5.h"
#ifdef PARALLEL_IO
#include <mpi.h>
#endif

// #define H5PART_SUCCESS		H5_SUCCESS
#define H5PART_ERR_NOMEM	H5_ERR_NOMEM
#define H5PART_ERR_INVAL	H5_ERR_INVAL
#define H5PART_ERR_BADFD	H5_ERR_BADFD
#define H5PART_ERR_LAYOUT	H5_ERR_LAYOUT
#define H5PART_ERR_NOENT	H5_ERR_NOENT
#define H5PART_ERR_NOENTRY	H5_ERR_NOENTRY

#define H5PART_ERR_MPI		H5_ERR_MPI
#define H5PART_ERR_HDF5		H5_ERR_HDF5

#define H5PART_READ		H5_O_RDONLY
#define H5PART_WRITE		H5_O_WRONLY
#define H5PART_APPEND		H5_O_APPEND

#define H5PART_INT64		((h5_int64_t)H5T_NATIVE_INT64)
#define H5PART_FLOAT64		((h5_int64_t)H5T_NATIVE_DOUBLE)
#define H5PART_CHAR		((h5_int64_t)H5T_NATIVE_CHAR)


/*============== File Writing Functions ==================== */
h5_int64_t
H5PartDefineStepName (
	h5_file_t *f,
	const char *name,
	const h5_int64_t width
	);

h5_int64_t
H5PartSetNumParticles ( 
	h5_file_t *f, 
	const h5_int64_t nparticles
	);

h5_int64_t
H5PartWriteDataFloat64 (
	h5_file_t *f,
	const char *name,
	const h5_float64_t *array
	);

h5_int64_t
H5PartWriteDataInt64 (
	h5_file_t *f,
	const char *name,
	const h5_int64_t *array
	);

/*================== File Reading Routines =================*/
h5_int64_t
H5PartGetNumDatasets (
	h5_file_t *f
	);

h5_int64_t
H5PartGetDatasetName (
	h5_file_t *f,
	const h5_int64_t idx,
	char *name,
	const h5_int64_t maxlen
	);

h5_int64_t
H5PartGetDatasetInfo (
	h5_file_t *f,
	const h5_int64_t idx,
	char *name,
	const h5_int64_t maxlen,
	h5_int64_t *type,
	h5_int64_t *nelem);


h5_int64_t
H5PartGetNumParticles (
	h5_file_t *f
	);

h5_int64_t
H5PartSetView (
	h5_file_t *f,
	const h5_int64_t start,
	const h5_int64_t end
	);


h5_int64_t
H5PartGetView (
	h5_file_t *f,
	h5_int64_t *start,
	h5_int64_t *end
	);

h5_int64_t
H5PartHasView (
	h5_file_t *f
	);

h5_int64_t
H5PartResetView (
	h5_file_t *f
	);

h5_int64_t
H5PartSetCanonicalView (
	h5_file_t *f
	);

h5_int64_t
H5PartReadDataFloat64(
	h5_file_t *f,
	const char *name,
	h5_float64_t *array
	);

h5_int64_t
H5PartReadDataInt64 (
	h5_file_t *f,
	const char *name,
	h5_int64_t *array
	);

h5_int64_t
H5PartReadParticleStep (
	h5_file_t *f,
	const h5_int64_t step,
	h5_float64_t *x, /* particle positions */
	h5_float64_t *y,
	h5_float64_t *z,
	h5_float64_t *px, /* particle momenta */
	h5_float64_t *py,
	h5_float64_t *pz,
	h5_int64_t *id /* and phase */
	);


#ifdef __cplusplus
}
#endif

#endif
