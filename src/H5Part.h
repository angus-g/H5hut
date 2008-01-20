#ifndef _H5Part_H_
#define _H5Part_H_

#include <stdlib.h>
#include <stdarg.h>
#include <hdf5.h>
#ifdef PARALLEL_IO
#include <mpi.h>
#endif

#ifdef __cplusplus
extern "C" {
#endif

#include "H5PartTypes.h"


#define H5PART_SUCCESS		0
#define H5PART_ERR_NOMEM	-12
#define H5PART_ERR_INVAL	-22
#define H5PART_ERR_BADFD	-77

#define H5PART_ERR_INIT         -200
#define H5PART_ERR_NOENTRY	-201

#define H5PART_ERR_MPI		-201
#define H5PART_ERR_HDF5		-202


#define H5_O_RDONLY		0x01
#define H5_O_WRONLY		0x02
#define H5_O_APPEND		0x03
#define H5_O_RDWR		0x04

#define H5PART_READ		H5_O_RDONLY	# legacy def, will be removed
#define H5PART_WRITE		H5_O_WRONLY	# legacy def, will be removed
#define H5PART_APPEND		H5_O_APPEND	# legacy def, will be removed

#define H5PART_INT64		((h5part_int64_t)H5T_NATIVE_INT64)
#define H5PART_FLOAT64		((h5part_int64_t)H5T_NATIVE_DOUBLE)
#define H5PART_CHAR		((h5part_int64_t)H5T_NATIVE_CHAR)

/*========== File Opening/Closing ===============*/
h5_file*
H5PartOpenFile(
	const char *filename,
	const unsigned flags
	);

#define H5PartOpenFileSerial(x,y) H5PartOpenFile(x,y)

#ifdef PARALLEL_IO
h5_file*
H5PartOpenFileParallel (
	const char *filename,
	const unsigned flags,
	MPI_Comm communicator
	);
#endif


h5part_int64_t
H5PartCloseFile (
	h5_file *f
	);


/*============== File Writing Functions ==================== */
h5part_int64_t
H5PartDefineStepName (
	h5_file *f,
	const char *name,
	const h5part_int64_t width
	);

h5part_int64_t
H5PartSetNumParticles ( 
	h5_file *f, 
	const h5part_int64_t nparticles
	);

h5part_int64_t
H5PartWriteDataFloat64 (
	h5_file *f,
	const char *name,
	const h5part_float64_t *array
	);

h5part_int64_t
H5PartWriteDataInt64 (
	h5_file *f,
	const char *name,
	const h5part_int64_t *array
	);

/*================== File Reading Routines =================*/
h5part_int64_t
H5PartSetStep (
	h5_file *f,
	const h5part_int64_t step
	);

h5part_int64_t
H5PartHasStep (
	h5_file *f,
	const h5part_int64_t step
	);

h5part_int64_t
H5PartGetNumSteps (
	h5_file *f
	);

h5part_int64_t
H5PartGetNumDatasets (
	h5_file *f
	);

h5part_int64_t
H5PartGetDatasetName (
	h5_file *f,
	const h5part_int64_t idx,
	char *name,
	const h5part_int64_t maxlen
	);

h5part_int64_t
H5PartGetDatasetInfo (
	h5_file *f,
	const h5part_int64_t idx,
	char *name,
	const h5part_int64_t maxlen,
	h5part_int64_t *type,
	h5part_int64_t *nelem);


h5part_int64_t
H5PartGetNumParticles (
	h5_file *f
	);

h5part_int64_t
H5PartSetView (
	h5_file *f,
	const h5part_int64_t start,
	const h5part_int64_t end
	);


h5part_int64_t
H5PartGetView (
	h5_file *f,
	h5part_int64_t *start,
	h5part_int64_t *end
	);

h5part_int64_t
H5PartHasView (
	h5_file *f
	);

h5part_int64_t
H5PartResetView (
	h5_file *f
	);

h5part_int64_t
H5PartSetCanonicalView (
	h5_file *f
	);

h5part_int64_t
H5PartReadDataFloat64(
	h5_file *f,
	const char *name,
	h5part_float64_t *array
	);

h5part_int64_t
H5PartReadDataInt64 (
	h5_file *f,
	const char *name,
	h5part_int64_t *array
	);

h5part_int64_t
H5PartReadParticleStep (
	h5_file *f,
	const h5part_int64_t step,
	h5part_float64_t *x, /* particle positions */
	h5part_float64_t *y,
	h5part_float64_t *z,
	h5part_float64_t *px, /* particle momenta */
	h5part_float64_t *py,
	h5part_float64_t *pz,
	h5part_int64_t *id /* and phase */
	);

/**********==============Attributes Interface============***************/
/* currently there is file attributes:  Attributes bound to the file
   and step attributes which are bound to the current step.  You 
   must set the step explicitly before writing the attributes (just
   as you must do when you write a new dataset.  Currently there are no
   attributes that are bound to a particular data array, but this could
   easily be done if required.
*/
h5part_int64_t
H5PartWriteStepAttrib (
	h5_file *f,
	const char *attrib_name,
	const h5part_int64_t attrib_type,
	const void *attrib_value,
	const h5part_int64_t attrib_nelem
	);

h5part_int64_t
H5PartWriteFileAttrib (
	h5_file *f,
	const char *attrib_name,
	const h5part_int64_t attrib_type,
	const void *attrib_value,
	const h5part_int64_t attrib_nelem
	);

h5part_int64_t
H5PartWriteFileAttribString (
	h5_file *f,
	const char *name,
	const char *attrib
	);

h5part_int64_t
H5PartWriteStepAttribString ( 
	h5_file *f,
	const char *name,
	const char *attrib
	);

h5part_int64_t
H5PartGetNumStepAttribs ( /* for current filestep */
	h5_file *f
	);

h5part_int64_t
H5PartGetNumFileAttribs (
	h5_file *f
	);

h5part_int64_t
H5PartGetStepAttribInfo (
	h5_file *f,
	const h5part_int64_t attrib_idx,
	char *attrib_name,
	const h5part_int64_t len_of_attrib_name,
	h5part_int64_t *attrib_type,
	h5part_int64_t *attrib_nelem
	);

h5part_int64_t
H5PartGetFileAttribInfo (
	h5_file *f,
	const h5part_int64_t idx,
	char *name,
	const h5part_int64_t maxnamelen,
	h5part_int64_t *type,
	h5part_int64_t *nelem
	);

h5part_int64_t
H5PartReadStepAttrib (
	h5_file *f,
	const char *name,
	void *data
	);

h5part_int64_t
H5PartReadFileAttrib (
	h5_file *f,
	const char *name,
	void *data
	);

h5part_int64_t
H5PartSetVerbosityLevel (
	const h5part_int64_t level
	);

h5part_int64_t
H5PartSetErrorHandler (
	const h5part_error_handler handler
	);

h5part_int64_t
H5PartGetErrno (
	void
	);

h5part_error_handler
H5PartGetErrorHandler (
	void
	);

h5part_int64_t
H5PartReportErrorHandler (
	const char *funcname,
	const h5part_int64_t eno,
	const char *fmt,
	...
	);

h5part_int64_t
H5PartAbortErrorHandler (
	const char *funcname,
	const h5part_int64_t eno,
	const char *fmt,
	...
	);

#ifdef __cplusplus
}
#endif

#endif
