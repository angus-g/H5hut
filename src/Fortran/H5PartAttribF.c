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

/* Writing attributes */
#define h5_writefileattrib_string F77NAME (				\
					h5pt_writefileattrib_string_,	\
					H5PT_writefileattrib_string )
#define h5_writestepattrib_string F77NAME (				\
					h5pt_writestepattrib_string_,	\
					H5PT_WRITESTEPATTRIB_STRING )

h5_int64_t
h5pt_writefileattrib_string (
	const h5_int64_t *f,
	const char *attrib_name,
	const char *attrib_value,
	const int l_attrib_name,
	const int l_attrib_value
	) {

	h5_file_t *filehandle = (h5_file_t*)(size_t)*f;

	char *attrib_name2 = _H5Part_strdupfor2c (attrib_name,l_attrib_name);
	char *attrib_value2= _H5Part_strdupfor2c (attrib_value,l_attrib_value);

	h5_int64_t herr = H5PartWriteFileAttribString (
		filehandle, attrib_name2, attrib_value2 );

	free ( attrib_name2 );
	free ( attrib_value2 );
	return herr;
}

h5_int64_t
h5pt_writestepattrib_string (
	const h5_int64_t *f,
	const char *attrib_name,
	const char *attrib_value,
	const int l_attrib_name,
	const int l_attrib_value
	) {

	h5_file_t *filehandle = (h5_file_t*)(size_t)*f;

	char *attrib_name2 = _H5Part_strdupfor2c (attrib_name,l_attrib_name);
	char *attrib_value2= _H5Part_strdupfor2c (attrib_value,l_attrib_value);

	h5_int64_t herr = H5PartWriteStepAttribString (
		filehandle, attrib_name2, attrib_value2 );

	free ( attrib_name2 );
	free ( attrib_value2 );
	return herr;
}



/* Reading attributes */
#define h5pt_getnstepattribs F77NAME (					\
					h5pt_getnstepattribs_,		\
					H5PT_GETNSTEPATTRIBS )
#define h5pt_getnfileattribs F77NAME (					\
					h5pt_getnfileattribs_,		\
					H5PT_GETNFILEATTRIBS )
#define h5pt_getstepattribinfo F77NAME (				\
					h5pt_getstepattribinfo_,	\
					H5PT_GETSTEPATTRIBINFO )
#define h5pt_getfileattribinfo F77NAME (				\
					h5pt_getfileattribinfo_,	\
					H5PT_GETFILEATTRIBINFO )
#define h5pt_readstepattrib_string F77NAME (				\
					h5pt_readstepattrib_string_,	\
					H5PT_READSTEPATTRIB_STRING )
#define h5pt_readfileattrib_string F77NAME (				\
					h5pt_readfileattrib_string_,	\
					H5PT_READFILEATTRIB_STRING )



#if ! defined(F77_NO_UNDERSCORE)
#define h5pt_writefileattrib_r8 F77NAME ( \
	h5pt_writefileattrib_r8_, \
	H5PT_WRITEFILEATTRIB_R8 )
#endif

h5_int64_t
h5pt_readstepattrib_string (
	const h5_int64_t *f,
	const char *attrib_name,
	char *attrib_value,
	const int l_attrib_name,
	const int l_attrib_value
	) {

	h5_file_t *filehandle = (h5_file_t*)(size_t)*f;
	
	char * attrib_name2 = _H5Part_strdupfor2c (attrib_name,l_attrib_name);

	h5_int64_t herr = H5PartReadStepAttrib (
		filehandle, attrib_name2, attrib_value );

	_H5Part_strc2for ( attrib_value, l_attrib_value );

	free ( attrib_name2 );
	return herr;
}

h5_int64_t
h5pt_readfileattrib_string (
	const h5_int64_t *f,
	const char *attrib_name,
	char *attrib_value,
	const int l_attrib_name,
	const int l_attrib_value
	) {
		
	h5_file_t *filehandle = (h5_file_t*)(size_t)*f;

	char * attrib_name2 = _H5Part_strdupfor2c (attrib_name,l_attrib_name);

	h5_int64_t herr = H5PartReadFileAttrib (
		filehandle, attrib_name2, attrib_value );

	_H5Part_strc2for ( attrib_value, l_attrib_value );

	free ( attrib_name2 );
	return herr;
}

h5part_int64_t
h5pt_writefileattrib_r8 (
	h5part_int64_t *f,
	const char *name,
	const h5part_float64_t *data,
	const h5part_float64_t *nelem,
	const int l_name
	) {

	H5PartFile *filehandle = (H5PartFile*)(size_t)*f;

	char *name2 =_H5Part_strdupfor2c ( name, l_name );

	h5part_int64_t herr = H5PartWriteFileAttrib (
		filehandle, name2, H5PART_FLOAT64, data, *nelem);

	free ( name2 );
	return herr;
}

#if ! defined(F77_NO_UNDERSCORE)
#define h5pt_writefileattrib_r8 F77NAME ( \
	h5pt_writefileattrib_r8_, \
	H5PT_WRITEFILEATTRIB_R8 )
#endif

h5part_int64_t
h5pt_readfileattrib_r8 (
	h5part_int64_t *f,
	const char *name,
	const h5part_float64_t *data,
	const int l_name
	) {

	H5PartFile *filehandle = (H5PartFile*)(size_t)*f;

	char *name2 =_H5Part_strdupfor2c ( name, l_name );

	h5part_int64_t herr = H5PartReadFileAttrib (
		filehandle, name2, (void*)data);

	free ( name2 );
	return herr;
}

#if ! defined(F77_NO_UNDERSCORE)
#define h5pt_writefileattrib_r4 F77NAME ( \
	h5pt_writefileattrib_r4_, \
	H5PT_WRITEFILEATTRIB_R4 )
#endif

h5part_int64_t
h5pt_writefileattrib_r4 (
	h5part_int64_t *f,
	const char *name,
	const h5part_float32_t *data,
	const h5part_float32_t *nelem,
	const int l_name
	) {

	H5PartFile *filehandle = (H5PartFile*)(size_t)*f;

	char *name2 =_H5Part_strdupfor2c ( name, l_name );

	h5part_int64_t herr = H5PartWriteFileAttrib (
		filehandle, name2, H5PART_FLOAT32, data, *nelem);

	free ( name2 );
	return herr;
}

#if ! defined(F77_NO_UNDERSCORE)
#define h5pt_writefileattrib_r4 F77NAME ( \
	h5pt_writefileattrib_r4_, \
	H5PT_WRITEFILEATTRIB_R4 )
#endif

h5part_int64_t
h5pt_readfileattrib_r4 (
	h5part_int64_t *f,
	const char *name,
	const h5part_float32_t *data,
	const int l_name
	) {

	H5PartFile *filehandle = (H5PartFile*)(size_t)*f;

	char *name2 =_H5Part_strdupfor2c ( name, l_name );

	h5part_int64_t herr = H5PartReadFileAttrib (
		filehandle, name2, (void*)data);

	free ( name2 );
	return herr;
}

#if ! defined(F77_NO_UNDERSCORE)
#define h5pt_writefileattrib_i8 F77NAME ( \
	h5pt_writefileattrib_i8_, \
	H5PT_WRITEFILEATTRIB_I8 )
#endif

h5part_int64_t
h5pt_writefileattrib_i8 (
	h5part_int64_t *f,
	const char *name,
	const h5part_int64_t *data,
	const h5part_int64_t *nelem,
	const int l_name
	) {

	H5PartFile *filehandle = (H5PartFile*)(size_t)*f;

	char *name2 =_H5Part_strdupfor2c ( name, l_name );

	h5part_int64_t herr = H5PartWriteFileAttrib (
		filehandle, name2, H5PART_INT64, data, *nelem);

	free ( name2 );
	return herr;
}

#if ! defined(F77_NO_UNDERSCORE)
#define h5pt_writefileattrib_i8 F77NAME ( \
	h5pt_writefileattrib_i8_, \
	H5PT_WRITEFILEATTRIB_I8 )
#endif

h5part_int64_t
h5pt_readfileattrib_i8 (
	h5part_int64_t *f,
	const char *name,
	const h5part_int64_t *data,
	const int l_name
	) {

	H5PartFile *filehandle = (H5PartFile*)(size_t)*f;

	char *name2 =_H5Part_strdupfor2c ( name, l_name );

	h5part_int64_t herr = H5PartReadFileAttrib (
		filehandle, name2, (void*)data);

	free ( name2 );
	return herr;
}

#if ! defined(F77_NO_UNDERSCORE)
#define h5pt_writefileattrib_i4 F77NAME ( \
	h5pt_writefileattrib_i4_, \
	H5PT_WRITEFILEATTRIB_I4 )
#endif

h5part_int64_t
h5pt_writefileattrib_i4 (
	h5part_int64_t *f,
	const char *name,
	const h5part_int32_t *data,
	const h5part_int32_t *nelem,
	const int l_name
	) {

	H5PartFile *filehandle = (H5PartFile*)(size_t)*f;

	char *name2 =_H5Part_strdupfor2c ( name, l_name );

	h5part_int64_t herr = H5PartWriteFileAttrib (
		filehandle, name2, H5PART_INT32, data, *nelem);

	free ( name2 );
	return herr;
}

#if ! defined(F77_NO_UNDERSCORE)
#define h5pt_writefileattrib_i4 F77NAME ( \
	h5pt_writefileattrib_i4_, \
	H5PT_WRITEFILEATTRIB_I4 )
#endif

h5part_int64_t
h5pt_readfileattrib_i4 (
	h5part_int64_t *f,
	const char *name,
	const h5part_int32_t *data,
	const int l_name
	) {

	H5PartFile *filehandle = (H5PartFile*)(size_t)*f;

	char *name2 =_H5Part_strdupfor2c ( name, l_name );

	h5part_int64_t herr = H5PartReadFileAttrib (
		filehandle, name2, (void*)data);

	free ( name2 );
	return herr;
}

#if ! defined(F77_NO_UNDERSCORE)
#define h5pt_writestepattrib_r8 F77NAME ( \
	h5pt_writestepattrib_r8_, \
	H5PT_WRITESTEPATTRIB_R8 )
#endif

h5part_int64_t
h5pt_writestepattrib_r8 (
	h5part_int64_t *f,
	const char *name,
	const h5part_float64_t *data,
	const h5part_float64_t *nelem,
	const int l_name
	) {

	H5PartFile *filehandle = (H5PartFile*)(size_t)*f;

	char *name2 =_H5Part_strdupfor2c ( name, l_name );

	h5part_int64_t herr = H5PartWriteStepAttrib (
		filehandle, name2, H5PART_FLOAT64, data, *nelem);

	free ( name2 );
	return herr;
}

#if ! defined(F77_NO_UNDERSCORE)
#define h5pt_writestepattrib_r8 F77NAME ( \
	h5pt_writestepattrib_r8_, \
	H5PT_WRITESTEPATTRIB_R8 )
#endif

h5part_int64_t
h5pt_readstepattrib_r8 (
	h5part_int64_t *f,
	const char *name,
	const h5part_float64_t *data,
	const int l_name
	) {

	H5PartFile *filehandle = (H5PartFile*)(size_t)*f;

	char *name2 =_H5Part_strdupfor2c ( name, l_name );

	h5part_int64_t herr = H5PartReadStepAttrib (
		filehandle, name2, (void*)data);

	free ( name2 );
	return herr;
}

#if ! defined(F77_NO_UNDERSCORE)
#define h5pt_writestepattrib_r4 F77NAME ( \
	h5pt_writestepattrib_r4_, \
	H5PT_WRITESTEPATTRIB_R4 )
#endif

h5part_int64_t
h5pt_writestepattrib_r4 (
	h5part_int64_t *f,
	const char *name,
	const h5part_float32_t *data,
	const h5part_float32_t *nelem,
	const int l_name
	) {

	H5PartFile *filehandle = (H5PartFile*)(size_t)*f;

	char *name2 =_H5Part_strdupfor2c ( name, l_name );

	h5part_int64_t herr = H5PartWriteStepAttrib (
		filehandle, name2, H5PART_FLOAT32, data, *nelem);

	free ( name2 );
	return herr;
}

#if ! defined(F77_NO_UNDERSCORE)
#define h5pt_writestepattrib_r4 F77NAME ( \
	h5pt_writestepattrib_r4_, \
	H5PT_WRITESTEPATTRIB_R4 )
#endif

h5part_int64_t
h5pt_readstepattrib_r4 (
	h5part_int64_t *f,
	const char *name,
	const h5part_float32_t *data,
	const int l_name
	) {

	H5PartFile *filehandle = (H5PartFile*)(size_t)*f;

	char *name2 =_H5Part_strdupfor2c ( name, l_name );

	h5part_int64_t herr = H5PartReadStepAttrib (
		filehandle, name2, (void*)data);

	free ( name2 );
	return herr;
}

#if ! defined(F77_NO_UNDERSCORE)
#define h5pt_writestepattrib_i8 F77NAME ( \
	h5pt_writestepattrib_i8_, \
	H5PT_WRITESTEPATTRIB_I8 )
#endif

h5part_int64_t
h5pt_writestepattrib_i8 (
	h5part_int64_t *f,
	const char *name,
	const h5part_int64_t *data,
	const h5part_int64_t *nelem,
	const int l_name
	) {

	H5PartFile *filehandle = (H5PartFile*)(size_t)*f;

	char *name2 =_H5Part_strdupfor2c ( name, l_name );

	h5part_int64_t herr = H5PartWriteStepAttrib (
		filehandle, name2, H5PART_INT64, data, *nelem);

	free ( name2 );
	return herr;
}

#if ! defined(F77_NO_UNDERSCORE)
#define h5pt_writestepattrib_i8 F77NAME ( \
	h5pt_writestepattrib_i8_, \
	H5PT_WRITESTEPATTRIB_I8 )
#endif

h5part_int64_t
h5pt_readstepattrib_i8 (
	h5part_int64_t *f,
	const char *name,
	const h5part_int64_t *data,
	const int l_name
	) {

	H5PartFile *filehandle = (H5PartFile*)(size_t)*f;

	char *name2 =_H5Part_strdupfor2c ( name, l_name );

	h5part_int64_t herr = H5PartReadStepAttrib (
		filehandle, name2, (void*)data);

	free ( name2 );
	return herr;
}

#if ! defined(F77_NO_UNDERSCORE)
#define h5pt_writestepattrib_i4 F77NAME ( \
	h5pt_writestepattrib_i4_, \
	H5PT_WRITESTEPATTRIB_I4 )
#endif

h5part_int64_t
h5pt_writestepattrib_i4 (
	h5part_int64_t *f,
	const char *name,
	const h5part_int32_t *data,
	const h5part_int32_t *nelem,
	const int l_name
	) {

	H5PartFile *filehandle = (H5PartFile*)(size_t)*f;

	char *name2 =_H5Part_strdupfor2c ( name, l_name );

	h5part_int64_t herr = H5PartWriteStepAttrib (
		filehandle, name2, H5PART_INT32, data, *nelem);

	free ( name2 );
	return herr;
}

#if ! defined(F77_NO_UNDERSCORE)
#define h5pt_writestepattrib_i4 F77NAME ( \
	h5pt_writestepattrib_i4_, \
	H5PT_WRITESTEPATTRIB_I4 )
#endif

h5part_int64_t
h5pt_readstepattrib_i4 (
	h5part_int64_t *f,
	const char *name,
	const h5part_int32_t *data,
	const int l_name
	) {

	H5PartFile *filehandle = (H5PartFile*)(size_t)*f;

	char *name2 =_H5Part_strdupfor2c ( name, l_name );

	h5part_int64_t herr = H5PartReadStepAttrib (
		filehandle, name2, (void*)data);

	free ( name2 );
	return herr;
}

/*** QUERY ***/

h5_int64_t
h5pt_getnstepattribs (
	const h5_int64_t *f
	) {

	h5_file_t *filehandle = (h5_file_t*)(size_t)*f;

	return H5PartGetNumStepAttribs ( filehandle );
}

h5_int64_t
h5pt_getnfileattribs (
	const h5_int64_t *f
	) {

	h5_file_t *filehandle = (h5_file_t*)(size_t)*f;

	return H5PartGetNumFileAttribs ( filehandle );
}

h5_int64_t
h5pt_getstepattribinfo (
	const h5_int64_t *f,
	const h5_int64_t *idx,
	char *name,
	h5_int64_t *nelem,
	const int l_name
	) {

	h5_file_t *filehandle = (h5_file_t*)(size_t)*f;
	h5_int64_t type;

	h5_int64_t herr = H5PartGetStepAttribInfo ( 
		filehandle, *idx, name, l_name, &type, nelem);

	_H5Part_strc2for( name, l_name );
	return herr;
}

h5_int64_t
h5pt_getfileattribinfo (
	const h5_int64_t *f,
	const h5_int64_t *idx,
	char *name,
	h5_int64_t *nelem,
	const int l_name ) {

	h5_file_t *filehandle = (h5_file_t*)(size_t)*f;
	h5_int64_t type;

	h5_int64_t herr = H5PartGetFileAttribInfo ( 
		filehandle, *idx, name, l_name, &type, nelem);

	_H5Part_strc2for( name, l_name );
	return herr;
}


