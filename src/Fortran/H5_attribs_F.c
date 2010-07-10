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

#define h5_writefileattrib_string F77NAME (				\
					h5_writefileattrib_string_,	\
					H5_writefileattrib_string )
#define h5_writestepattrib_string F77NAME (				\
					h5_writestepattrib_string_,	\
					H5_WRITESTEPATTRIB_STRING )
#define h5_readstepattrib_string F77NAME (				\
					h5_readstepattrib_string_,	\
					h5_READSTEPATTRIB_STRING )
#define h5_readfileattrib_string F77NAME (				\
					h5_readfileattrib_string_,	\
					h5_READFILEATTRIB_STRING )

#endif

h5_err_t
h5_writefileattrib_string (
	const h5_int64_t *f,
	const char *attrib_name,
	const char *attrib_value,
	const int l_attrib_name,
	const int l_attrib_value
	) {

	h5_file_t *filehandle = (h5_file_t*)(size_t)*f;

	char *attrib_name2 = h5_strdupfor2c (attrib_name,l_attrib_name);
	char *attrib_value2= h5_strdupfor2c (attrib_value,l_attrib_value);

	h5_err_t herr = H5WriteFileAttribString (
		filehandle, attrib_name2, attrib_value2 );

	free ( attrib_name2 );
	free ( attrib_value2 );
	return herr;
}

h5_err_t
h5_writestepattrib_string (
	const h5_int64_t *f,
	const char *attrib_name,
	const char *attrib_value,
	const int l_attrib_name,
	const int l_attrib_value
	) {

	h5_file_t *filehandle = (h5_file_t*)(size_t)*f;

	char *attrib_name2 = h5_strdupfor2c (attrib_name,l_attrib_name);
	char *attrib_value2= h5_strdupfor2c (attrib_value,l_attrib_value);

	h5_err_t herr = H5WriteStepAttribString (
		filehandle, attrib_name2, attrib_value2 );

	free ( attrib_name2 );
	free ( attrib_value2 );
	return herr;
}


h5_err_t
h5_readstepattrib_string (
	const h5_int64_t *f,
	const char *attrib_name,
	char *attrib_value,
	const int l_attrib_name,
	const int l_attrib_value
	) {

	h5_file_t *filehandle = (h5_file_t*)(size_t)*f;
	
	char * attrib_name2 = h5_strdupfor2c (attrib_name,l_attrib_name);

	h5_err_t herr = H5ReadStepAttribString (
		filehandle, attrib_name2, attrib_value );

	h5_strc2for ( attrib_value, l_attrib_value );

	free ( attrib_name2 );
	return herr;
}

h5_err_t
h5_readfileattrib_string (
	const h5_int64_t *f,
	const char *attrib_name,
	char *attrib_value,
	const int l_attrib_name,
	const int l_attrib_value
	) {
		
	h5_file_t *filehandle = (h5_file_t*)(size_t)*f;

	char * attrib_name2 = h5_strdupfor2c (attrib_name,l_attrib_name);

	h5_err_t herr = H5ReadFileAttribString (
		filehandle, attrib_name2, attrib_value );

	h5_strc2for ( attrib_value, l_attrib_value );

	free ( attrib_name2 );
	return herr;
}

#if ! defined(F77_NO_UNDERSCORE)
#define h5_writefileattrib_r8 F77NAME ( \
	h5_writefileattrib_r8_, \
	h5_WRITEFILEATTRIB_R8 )
#endif

h5_err_t
h5_writefileattrib_r8 (
	h5_int64_t *f,
	const char *name,
	const h5_float64_t *data,
	const h5_float64_t *nelem,
	const int l_name
	) {

	h5_file_t *filehandle = (h5_file_t*)(size_t)*f;

	char *name2 = h5_strdupfor2c ( name, l_name );

	h5_err_t herr = H5WriteFileAttribFloat64(
					filehandle, name2, data, *nelem);

	free ( name2 );
	return herr;
}

#if ! defined(F77_NO_UNDERSCORE)
#define h5_writefileattrib_r8 F77NAME ( \
	h5_writefileattrib_r8_, \
	h5_WRITEFILEATTRIB_R8 )
#endif

h5_err_t
h5_readfileattrib_r8 (
	h5_int64_t *f,
	const char *name,
	h5_float64_t *data,
	const int l_name
	) {

	h5_file_t *filehandle = (h5_file_t*)(size_t)*f;

	char *name2 = h5_strdupfor2c ( name, l_name );

	h5_err_t herr = H5ReadFileAttribFloat64(filehandle, name2, data);

	free ( name2 );
	return herr;
}

#if ! defined(F77_NO_UNDERSCORE)
#define h5_writefileattrib_r4 F77NAME ( \
	h5_writefileattrib_r4_, \
	h5_WRITEFILEATTRIB_R4 )
#endif

h5_err_t
h5_writefileattrib_r4 (
	h5_int64_t *f,
	const char *name,
	const h5_float32_t *data,
	const h5_float32_t *nelem,
	const int l_name
	) {

	h5_file_t *filehandle = (h5_file_t*)(size_t)*f;

	char *name2 = h5_strdupfor2c ( name, l_name );

	h5_err_t herr = H5WriteFileAttribFloat32(
					filehandle, name2, data, *nelem);

	free ( name2 );
	return herr;
}

#if ! defined(F77_NO_UNDERSCORE)
#define h5_writefileattrib_r4 F77NAME ( \
	h5_writefileattrib_r4_, \
	h5_WRITEFILEATTRIB_R4 )
#endif

h5_err_t
h5_readfileattrib_r4 (
	h5_int64_t *f,
	const char *name,
	h5_float32_t *data,
	const int l_name
	) {

	h5_file_t *filehandle = (h5_file_t*)(size_t)*f;

	char *name2 = h5_strdupfor2c ( name, l_name );

	h5_err_t herr = H5ReadFileAttribFloat32(filehandle, name2, data);

	free ( name2 );
	return herr;
}

#if ! defined(F77_NO_UNDERSCORE)
#define h5_writefileattrib_i8 F77NAME ( \
	h5_writefileattrib_i8_, \
	h5_WRITEFILEATTRIB_I8 )
#endif

h5_err_t
h5_writefileattrib_i8 (
	h5_int64_t *f,
	const char *name,
	const h5_int64_t *data,
	const h5_int64_t *nelem,
	const int l_name
	) {

	h5_file_t *filehandle = (h5_file_t*)(size_t)*f;

	char *name2 = h5_strdupfor2c ( name, l_name );

	h5_err_t herr = H5WriteFileAttribInt64(filehandle, name2, data, *nelem);

	free ( name2 );
	return herr;
}

#if ! defined(F77_NO_UNDERSCORE)
#define h5_writefileattrib_i8 F77NAME ( \
	h5_writefileattrib_i8_, \
	h5_WRITEFILEATTRIB_I8 )
#endif

h5_err_t
h5_readfileattrib_i8 (
	h5_int64_t *f,
	const char *name,
	h5_int64_t *data,
	const int l_name
	) {

	h5_file_t *filehandle = (h5_file_t*)(size_t)*f;

	char *name2 = h5_strdupfor2c ( name, l_name );

	h5_err_t herr = H5ReadFileAttribInt64(filehandle, name2, data);

	free ( name2 );
	return herr;
}

#if ! defined(F77_NO_UNDERSCORE)
#define h5_writefileattrib_i4 F77NAME ( \
	h5_writefileattrib_i4_, \
	h5_WRITEFILEATTRIB_I4 )
#endif

h5_err_t
h5_writefileattrib_i4 (
	h5_int64_t *f,
	const char *name,
	const h5_int32_t *data,
	const h5_int32_t *nelem,
	const int l_name
	) {

	h5_file_t *filehandle = (h5_file_t*)(size_t)*f;

	char *name2 = h5_strdupfor2c ( name, l_name );

	h5_err_t herr = H5WriteFileAttribInt32(filehandle, name2, data, *nelem);

	free ( name2 );
	return herr;
}

#if ! defined(F77_NO_UNDERSCORE)
#define h5_writefileattrib_i4 F77NAME ( \
	h5_writefileattrib_i4_, \
	h5_WRITEFILEATTRIB_I4 )
#endif

h5_err_t
h5_readfileattrib_i4 (
	h5_int64_t *f,
	const char *name,
	h5_int32_t *data,
	const int l_name
	) {

	h5_file_t *filehandle = (h5_file_t*)(size_t)*f;

	char *name2 = h5_strdupfor2c ( name, l_name );

	h5_err_t herr = H5ReadFileAttribInt32(filehandle, name2, data);

	free ( name2 );
	return herr;
}

#if ! defined(F77_NO_UNDERSCORE)
#define h5_writestepattrib_r8 F77NAME ( \
	h5_writestepattrib_r8_, \
	h5_WRITESTEPATTRIB_R8 )
#endif

h5_err_t
h5_writestepattrib_r8 (
	h5_int64_t *f,
	const char *name,
	const h5_float64_t *data,
	const h5_float64_t *nelem,
	const int l_name
	) {

	h5_file_t *filehandle = (h5_file_t*)(size_t)*f;

	char *name2 = h5_strdupfor2c ( name, l_name );

	h5_err_t herr = H5WriteStepAttribFloat64(
					filehandle, name2, data, *nelem);

	free ( name2 );
	return herr;
}

#if ! defined(F77_NO_UNDERSCORE)
#define h5_writestepattrib_r8 F77NAME ( \
	h5_writestepattrib_r8_, \
	h5_WRITESTEPATTRIB_R8 )
#endif

h5_err_t
h5_readstepattrib_r8 (
	h5_int64_t *f,
	const char *name,
	h5_float64_t *data,
	const int l_name
	) {

	h5_file_t *filehandle = (h5_file_t*)(size_t)*f;

	char *name2 = h5_strdupfor2c ( name, l_name );

	h5_err_t herr = H5ReadStepAttribFloat64(filehandle, name2, data);

	free ( name2 );
	return herr;
}

#if ! defined(F77_NO_UNDERSCORE)
#define h5_writestepattrib_r4 F77NAME ( \
	h5_writestepattrib_r4_, \
	h5_WRITESTEPATTRIB_R4 )
#endif

h5_err_t
h5_writestepattrib_r4 (
	h5_int64_t *f,
	const char *name,
	const h5_float32_t *data,
	const h5_float32_t *nelem,
	const int l_name
	) {

	h5_file_t *filehandle = (h5_file_t*)(size_t)*f;

	char *name2 = h5_strdupfor2c ( name, l_name );

	h5_err_t herr = H5WriteStepAttribFloat32(
					filehandle, name2, data, *nelem);

	free ( name2 );
	return herr;
}

#if ! defined(F77_NO_UNDERSCORE)
#define h5_writestepattrib_r4 F77NAME ( \
	h5_writestepattrib_r4_, \
	h5_WRITESTEPATTRIB_R4 )
#endif

h5_err_t
h5_readstepattrib_r4 (
	h5_int64_t *f,
	const char *name,
	h5_float32_t *data,
	const int l_name
	) {

	h5_file_t *filehandle = (h5_file_t*)(size_t)*f;

	char *name2 = h5_strdupfor2c ( name, l_name );

	h5_err_t herr = H5ReadStepAttribFloat32(filehandle, name2, data);

	free ( name2 );
	return herr;
}

#if ! defined(F77_NO_UNDERSCORE)
#define h5_writestepattrib_i8 F77NAME ( \
	h5_writestepattrib_i8_, \
	h5_WRITESTEPATTRIB_I8 )
#endif

h5_err_t
h5_writestepattrib_i8 (
	h5_int64_t *f,
	const char *name,
	const h5_int64_t *data,
	const h5_int64_t *nelem,
	const int l_name
	) {

	h5_file_t *filehandle = (h5_file_t*)(size_t)*f;

	char *name2 = h5_strdupfor2c ( name, l_name );

	h5_err_t herr = H5WriteStepAttribInt64(filehandle, name2, data, *nelem);

	free ( name2 );
	return herr;
}

#if ! defined(F77_NO_UNDERSCORE)
#define h5_writestepattrib_i8 F77NAME ( \
	h5_writestepattrib_i8_, \
	h5_WRITESTEPATTRIB_I8 )
#endif

h5_err_t
h5_readstepattrib_i8 (
	h5_int64_t *f,
	const char *name,
	h5_int64_t *data,
	const int l_name
	) {

	h5_file_t *filehandle = (h5_file_t*)(size_t)*f;

	char *name2 = h5_strdupfor2c ( name, l_name );

	h5_err_t herr = H5ReadStepAttribInt64(filehandle, name2, data);

	free ( name2 );
	return herr;
}

#if ! defined(F77_NO_UNDERSCORE)
#define h5_writestepattrib_i4 F77NAME ( \
	h5_writestepattrib_i4_, \
	h5_WRITESTEPATTRIB_I4 )
#endif

h5_err_t
h5_writestepattrib_i4 (
	h5_int64_t *f,
	const char *name,
	const h5_int32_t *data,
	const h5_int32_t *nelem,
	const int l_name
	) {

	h5_file_t *filehandle = (h5_file_t*)(size_t)*f;

	char *name2 = h5_strdupfor2c ( name, l_name );

	h5_err_t herr = H5WriteStepAttribInt32(filehandle, name2, data, *nelem);

	free ( name2 );
	return herr;
}

#if ! defined(F77_NO_UNDERSCORE)
#define h5_writestepattrib_i4 F77NAME ( \
	h5_writestepattrib_i4_, \
	h5_WRITESTEPATTRIB_I4 )
#endif

h5_err_t
h5_readstepattrib_i4 (
	h5_int64_t *f,
	const char *name,
	h5_int32_t *data,
	const int l_name
	) {

	h5_file_t *filehandle = (h5_file_t*)(size_t)*f;

	char *name2 = h5_strdupfor2c ( name, l_name );

	h5_err_t herr = H5ReadStepAttribInt32(filehandle, name2, data);

	free ( name2 );
	return herr;
}

/*** QUERY ***/

#if ! defined(F77_NO_UNDERSCORE)

#define h5_getnstepattribs F77NAME (					\
					h5_getnstepattribs_,		\
					h5_GETNSTEPATTRIBS )
#define h5_getnfileattribs F77NAME (					\
					h5_getnfileattribs_,		\
					h5_GETNFILEATTRIBS )
#define h5_getstepattribinfo F77NAME (				\
					h5_getstepattribinfo_,	\
					h5_GETSTEPATTRIBINFO )
#define h5_getfileattribinfo F77NAME (				\
					h5_getfileattribinfo_,	\
					h5_GETFILEATTRIBINFO )

#endif

h5_err_t
h5_getnstepattribs (
	const h5_int64_t *f
	) {

	h5_file_t *filehandle = (h5_file_t*)(size_t)*f;

	return H5GetNumStepAttribs ( filehandle );
}

h5_err_t
h5_getnfileattribs (
	const h5_int64_t *f
	) {

	h5_file_t *filehandle = (h5_file_t*)(size_t)*f;

	return H5GetNumFileAttribs ( filehandle );
}

h5_err_t
h5_getstepattribinfo (
	const h5_int64_t *f,
	const h5_int64_t *idx,
	char *name,
	h5_int64_t *nelem,
	const int l_name
	) {

	h5_file_t *filehandle = (h5_file_t*)(size_t)*f;
	h5_int64_t type;

	h5_err_t herr = H5GetStepAttribInfo ( 
		filehandle, *idx, name, l_name, &type, nelem);

	h5_strc2for( name, l_name );
	return herr;
}

h5_err_t
h5_getfileattribinfo (
	const h5_int64_t *f,
	const h5_int64_t *idx,
	char *name,
	h5_int64_t *nelem,
	const int l_name ) {

	h5_file_t *filehandle = (h5_file_t*)(size_t)*f;
	h5_int64_t type;

	h5_err_t herr = H5GetFileAttribInfo ( 
		filehandle, *idx, name, l_name, &type, nelem);

	h5_strc2for( name, l_name );
	return herr;
}

