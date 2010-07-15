#include <stdlib.h>

#include "H5hut.h"
#include "Underscore.h"

#if defined(F77_SINGLE_UNDERSCORE)
#define F77NAME(a,b) a
#elif defined(F77_CRAY_UNDERSCORE)
#define F77NAME(a,b) b
#elif defined(F77_NO_UNDERSCORE)
#else
#error Error, no way to determine how to construct fortran bindings
#endif

#if ! defined(F77_NO_UNDERSCORE)

#define h5pt_setnpoints F77NAME (					\
					h5pt_setnpoints_,		\
					H5PT_SETNPOINTS )
#define h5pt_setnpoints_strided F77NAME (					\
					h5pt_setnpoints_strided_,		\
					H5PT_SETNPOINTS_STRIDED )
#define h5pt_getnsteps F77NAME (					\
					h5pt_getnsteps_,		\
					H5PT_GETNSTEPS )
#define h5pt_getndatasets F77NAME (					\
					h5pt_getndatasets_,		\
					H5PT_GETNDATASETS )
#define h5pt_getnpoints F77NAME (					\
					h5pt_getnpoints_,		\
					H5PT_GETNPOINTS )
#define h5pt_getdatasetname F77NAME (					\
					h5pt_getdatasetname_,		\
					H5PT_GETDATASETNAME )
#define h5pt_setview F77NAME (						\
					h5pt_setview_,			\
					H5PT_SETVIEW )
#define h5pt_setview_indices F77NAME (					\
					h5pt_setview_indices_,		\
					H5PT_SETVIEW_INDICES )
#define h5pt_resetview F77NAME (					\
					h5pt_resetview_,		\
					H5PT_RESETVIEW )
#define h5pt_hasview F77NAME (						\
					h5pt_hasview_,			\
					H5PT_HASVIEW )
#define h5pt_getview F77NAME (						\
					h5pt_getview_,			\
					H5PT_GETVIEW )
#define h5pt_writedata_r8 F77NAME (					\
					h5pt_writedata_r8_,		\
					H5PT_WRITEDATA_R8 )
#define h5pt_writedata_r4 F77NAME (					\
					h5pt_writedata_r4_,		\
					H5PT_WRITEDATA_R4 )
#define h5pt_writedata_i8 F77NAME (					\
					h5pt_writedata_i8_,		\
					H5PT_WRITEDATA_I8 )
#define h5pt_writedata_i4 F77NAME (					\
					h5pt_writedata_i4_,		\
					H5PT_WRITEDATA_I4 )
#define h5pt_readdata_r8 F77NAME (					\
					h5pt_readdata_r8_,		\
					H5PT_READDATA_R8 )
#define h5pt_readdata_r4 F77NAME (					\
					h5pt_readdata_r4_,		\
					H5PT_READDATA_R4 )
#define h5pt_readdata_i8 F77NAME (					\
					h5pt_readdata_i8_,		\
					H5PT_READDATA_I8 )
#define h5pt_readdata_i4 F77NAME (					\
					h5pt_readdata_i4_,		\
					H5PT_READDATA_I4 )

#endif

h5_err_t
h5pt_setnpoints (
	const h5_int64_t *f,
	h5_int64_t *np
	) {

	h5_file_t *filehandle = (h5_file_t*)(size_t)*f;

	return H5PartSetNumParticles ( filehandle, *np );
}

h5_err_t
h5pt_setnpoints_strided (
	const h5_int64_t *f,
	h5_int64_t *np,
        h5_int64_t *stride
	) {

	h5_file_t *filehandle = (h5_file_t*)(size_t)*f;

	return H5PartSetNumParticlesStrided ( filehandle, *np, *stride );
}

/*==============Reading Data Characteristics============*/

h5_err_t
h5pt_getndatasets (
	const h5_int64_t *f
	) {

	h5_file_t *filehandle = (h5_file_t*)(size_t)*f;

	return H5PartGetNumDatasets ( filehandle );
}

h5_err_t
h5pt_getnpoints (
	const h5_int64_t *f
	) {

	h5_file_t *filehandle = (h5_file_t*)(size_t)*f;

	return H5PartGetNumParticles ( filehandle );
}

h5_err_t
h5pt_getdatasetname ( 
	const h5_int64_t *f,
	const h5_int64_t *index,
	char *name,
	const int l_name
	) {

	h5_file_t *filehandle = (h5_file_t*)(size_t)*f;

	h5_int64_t herr =  H5PartGetDatasetName (
		filehandle, *index, name, l_name );

	h5_strc2for ( name, l_name );
	return herr;
}

/*=============Setting and getting views================*/

h5_err_t
h5pt_setview (
	const h5_int64_t *f,
	const h5_int64_t *start,
	const h5_int64_t *end
	) {

	h5_file_t *filehandle = (h5_file_t*)(size_t)*f;

	return H5PartSetView ( filehandle, *start, *end );
}

h5_err_t
h5pt_setview_indices (
	const h5_int64_t *f,
	const h5_int64_t *indices,
	const h5_int64_t *nelem
	) {

	h5_file_t *filehandle = (h5_file_t*)(size_t)*f;

	return H5PartSetViewIndices ( filehandle, indices, *nelem );
}

h5_err_t
h5pt_setview_empty (
	const h5_int64_t *f
	) {

	h5_file_t *filehandle = (h5_file_t*)(size_t)*f;

	return H5PartSetViewEmpty ( filehandle );
}

h5_err_t
h5pt_resetview (
	const h5_int64_t *f
	) {

	h5_file_t *filehandle = (h5_file_t*)(size_t)*f;

	return H5PartResetView ( filehandle );
}

h5_err_t
h5pt_hasview (
	const h5_int64_t *f
	) {

	h5_file_t *filehandle = (h5_file_t*)(size_t)*f;

	return H5PartHasView ( filehandle );
}

h5_err_t
h5pt_getview (
	const h5_int64_t *f,
	h5_int64_t *start,
	h5_int64_t *end
	) {

	h5_file_t *filehandle = (h5_file_t*)(size_t)*f;

	return H5PartGetView ( filehandle, start, end);
}


/*==================Writing data ============*/
h5_err_t
h5pt_writedata_r8 (
	const h5_int64_t *f,
	const char *name,
	const h5_float64_t *data,
	const int l_name ) {

	h5_file_t *filehandle = (h5_file_t*)(size_t)*f;

	char *name2 = h5_strdupfor2c ( name, l_name );

	h5_int64_t herr = H5PartWriteDataFloat64 (
		filehandle, name2, data );

	free ( name2 );

	return herr;
}

h5_err_t
h5pt_writedata_r4 (
	const h5_int64_t *f,
	const char *name,
	const h5_float32_t *data,
	const int l_name ) {

	h5_file_t *filehandle = (h5_file_t*)(size_t)*f;

	char *name2 = h5_strdupfor2c ( name, l_name );

	h5_int64_t herr = H5PartWriteDataFloat32 (
		filehandle, name2, data );

	free ( name2 );

	return herr;
}

h5_err_t
h5pt_writedata_i8 (
	const h5_int64_t *f,
	const char *name,
	const h5_int64_t *data,
	const int l_name ) {

	h5_file_t *filehandle = (h5_file_t*)(size_t)*f;

	char *name2 = h5_strdupfor2c ( name, l_name );

	h5_int64_t herr = H5PartWriteDataInt64 (
		filehandle, name2, data );

	free ( name2 );

	return herr;
}

h5_err_t
h5pt_writedata_i4 (
	const h5_int64_t *f,
	const char *name,
	const h5_int32_t *data,
	const int l_name ) {

	h5_file_t *filehandle = (h5_file_t*)(size_t)*f;

	char *name2 = h5_strdupfor2c ( name, l_name );

	h5_int64_t herr = H5PartWriteDataInt32 (
		filehandle, name2, data );

	free ( name2 );

	return herr;
}


/*==================Reading data ============*/
h5_err_t
h5pt_readdata_r8 (
	const h5_int64_t *f,
	const char *name,
	h5_float64_t *array,
	const int l_name
	) {

	h5_file_t *filehandle = (h5_file_t*)(size_t)*f;

	char *name2 = h5_strdupfor2c ( name, l_name );

	h5_int64_t herr = H5PartReadDataFloat64 (
		filehandle, name2, array );

	free ( name2 );
	return herr;
}

h5_err_t
h5pt_readdata_r4 (
	const h5_int64_t *f,
	const char *name,
	h5_float32_t *array,
	const int l_name
	) {

	h5_file_t *filehandle = (h5_file_t*)(size_t)*f;

	char *name2 = h5_strdupfor2c ( name, l_name );

	h5_int64_t herr = H5PartReadDataFloat32 (
		filehandle, name2, array );

	free ( name2 );
	return herr;
}

h5_err_t
h5pt_readdata_i8 (
	const h5_int64_t *f,
	const char *name,
	h5_int64_t *array,
	const int l_name
	) {

	h5_file_t *filehandle = (h5_file_t*)(size_t)*f;

	char *name2 = h5_strdupfor2c ( name, l_name );

	h5_int64_t herr = H5PartReadDataInt64 (
		filehandle, name2, array );

	free ( name2 );
	return herr;
}

h5_err_t
h5pt_readdata_i4 (
	const h5_int64_t *f,
	const char *name,
	h5_int32_t *array,
	const int l_name
	) {

	h5_file_t *filehandle = (h5_file_t*)(size_t)*f;

	char *name2 = h5_strdupfor2c ( name, l_name );

	h5_int64_t herr = H5PartReadDataInt32 (
		filehandle, name2, array );

	free ( name2 );
	return herr;
}

