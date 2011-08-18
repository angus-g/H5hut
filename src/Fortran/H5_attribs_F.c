
#include <stdlib.h>
#include <string.h>

#include "h5core/h5_core.h"
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

#define h5_writefileattrib_string F77NAME (									h5_writefileattrib_string_,						H5_writefileattrib_string )
#define h5_writestepattrib_string F77NAME (									h5_writestepattrib_string_,						H5_WRITESTEPATTRIB_STRING )
#define h5_readstepattrib_string F77NAME (									h5_readstepattrib_string_,						h5_READSTEPATTRIB_STRING )
#define h5_readfileattrib_string F77NAME (									h5_readfileattrib_string_,						h5_READFILEATTRIB_STRING )

#endif

h5_err_t
h5_writefileattrib_string (
	h5_int64_t *const f,
	const char *name,
	const char *buffer,
	const int l_name,
	const int l_buffer
	) {

	h5_file_t *fh = h5_filehandlefor2c(f);
	char *name2 = h5_strdupfor2c ( name, l_name );
	char *buffer2 = h5_strdupfor2c ( buffer, l_buffer );
	H5_API_ENTER (h5_err_t,
		      "f=%p, name='%s', buffer='%s', l_name=%d, l_buffer=%d",
		      fh, name2, buffer2, l_name, l_buffer);

	h5_err_t herr = h5_write_attrib (
		fh, H5_ATTRIB_FILE, name2,
		H5_STRING_T, buffer2, strlen(buffer2)+1 );

	free ( name2 );
	free ( buffer2 );
	H5_API_RETURN(herr);
}

h5_err_t
h5_writestepattrib_string (
	h5_int64_t *const f,
	const char *name,
	const char *buffer,
	const int l_name,
	const int l_buffer
	) {

	h5_file_t *fh = h5_filehandlefor2c(f);
	char *name2 = h5_strdupfor2c ( name, l_name );
	char *buffer2 = h5_strdupfor2c ( buffer, l_buffer );
	H5_API_ENTER (h5_err_t,
		      "f=%p, name='%s', buffer='%s', l_name=%d, l_buffer=%d",
		      fh, name2, buffer2, l_name, l_buffer);

	h5_err_t herr = h5_write_attrib (
		fh, H5_ATTRIB_STEP, name2,
		H5_STRING_T, buffer2, strlen(buffer2)+1 );

	free ( name2 );
	free ( buffer2 );
	H5_API_RETURN(herr);
}

h5_err_t
h5_readfileattrib_string (
	h5_int64_t *const f,
	const char *name,
	char *buffer,
	const int l_name,
	const int l_buffer
	) {
		
	h5_file_t *fh = h5_filehandlefor2c(f);
	char * name2 = h5_strdupfor2c ( name, l_name );
	H5_API_ENTER (h5_err_t,
		      "f=%p, name='%s', buffer=%p, l_name=%d, l_buffer=%d",
		      fh, name2, buffer, l_name, l_buffer);

	h5_err_t herr = h5_read_attrib (
		fh, H5_ATTRIB_FILE, name2, H5_STRING_T, buffer );

	h5_strc2for ( buffer, l_buffer );

	free ( name2 );
	H5_API_RETURN(herr);
}

h5_err_t
h5_readstepeattrib_string (
	h5_int64_t *const f,
	const char *name,
	char *buffer,
	const int l_name,
	const int l_buffer
	) {
		
	h5_file_t *fh = h5_filehandlefor2c(f);
	char * name2 = h5_strdupfor2c ( name, l_name );
	H5_API_ENTER (h5_err_t,
		      "f=%p, name='%s', buffer=%p, l_name=%d, l_buffer=%d",
		      fh, name2, buffer, l_name, l_buffer);

	h5_err_t herr = h5_read_attrib (
		fh, H5_ATTRIB_STEP, name2, H5_STRING_T, buffer );

	h5_strc2for ( buffer, l_buffer );

	free ( name2 );
	H5_API_RETURN(herr);
}

#if ! defined(F77_NO_UNDERSCORE)
#define h5_writefileattrib_r8 F77NAME ( \
	h5_writefileattrib_r8_, \
	H5_WRITEFILEATTRIB_R8 )
#endif

h5_err_t
h5_writefileattrib_r8 (
	h5_int64_t *const f,
	const char *name,
	const h5_float64_t *buffer,
	const h5_int64_t *nelem,
	const int l_name
	) {

	h5_file_t *fh = h5_filehandlefor2c(f);
	char *name2 = h5_strdupfor2c ( name, l_name );
	H5_API_ENTER (h5_err_t,
		      "f=%p, name='%s', buffer=%p, nelem=%lld, l_name=%d",
		      fh, name2, buffer, (long long)*nelem, l_name);

	h5_err_t herr = h5_write_attrib(
		fh, H5_ATTRIB_FILE, name2,
		H5_FLOAT64_T, buffer, (hsize_t)*nelem);

	free ( name2 );
	H5_API_RETURN(herr);
}

#if ! defined(F77_NO_UNDERSCORE)
#define h5_readfileattrib_r8 F77NAME ( \
	h5_readfileattrib_r8_, \
	H5_READFILEATTRIB_R8 )
#endif

h5_err_t
h5bl_readfileattrib_r8 (
	h5_int64_t *const f,
	const char *name,
	h5_float64_t *buffer,
	const int l_name
	) {

	h5_file_t *fh = h5_filehandlefor2c(f);
	char *name2 = h5_strdupfor2c ( name, l_name );
	H5_API_ENTER (h5_err_t,
		      "f=%p, name='%s', buffer=%p, l_name=%d",
		      fh, name2, buffer, l_name);

	h5_err_t herr = h5_read_attrib(
		fh, H5_ATTRIB_FILE, name2, H5_FLOAT64_T, buffer);

	free ( name2 );
	H5_API_RETURN(herr);
}

#if ! defined(F77_NO_UNDERSCORE)
#define h5_writefileattrib_r4 F77NAME ( \
	h5_writefileattrib_r4_, \
	H5_WRITEFILEATTRIB_R4 )
#endif

h5_err_t
h5_writefileattrib_r4 (
	h5_int64_t *const f,
	const char *name,
	const h5_float32_t *buffer,
	const h5_int64_t *nelem,
	const int l_name
	) {

	h5_file_t *fh = h5_filehandlefor2c(f);
	char *name2 = h5_strdupfor2c ( name, l_name );
	H5_API_ENTER (h5_err_t,
		      "f=%p, name='%s', buffer=%p, nelem=%lld, l_name=%d",
		      fh, name2, buffer, (long long)*nelem, l_name);

	h5_err_t herr = h5_write_attrib(
		fh, H5_ATTRIB_FILE, name2,
		H5_FLOAT32_T, buffer, (hsize_t)*nelem);

	free ( name2 );
	H5_API_RETURN(herr);
}

#if ! defined(F77_NO_UNDERSCORE)
#define h5_readfileattrib_r4 F77NAME ( \
	h5_readfileattrib_r4_, \
	H5_READFILEATTRIB_R4 )
#endif

h5_err_t
h5bl_readfileattrib_r4 (
	h5_int64_t *const f,
	const char *name,
	h5_float64_t *buffer,
	const int l_name
	) {

	h5_file_t *fh = h5_filehandlefor2c(f);
	char *name2 = h5_strdupfor2c ( name, l_name );
	H5_API_ENTER (h5_err_t,
		      "f=%p, name='%s', buffer=%p, l_name=%d",
		      fh, name2, buffer, l_name);

	h5_err_t herr = h5_read_attrib(
		fh, H5_ATTRIB_FILE, name2, H5_FLOAT32_T, buffer);

	free ( name2 );
	H5_API_RETURN(herr);
}

#if ! defined(F77_NO_UNDERSCORE)
#define h5_writefileattrib_i8 F77NAME ( \
	h5_writefileattrib_i8_, \
	H5_WRITEFILEATTRIB_I8 )
#endif

h5_err_t
h5_writefileattrib_i8 (
	h5_int64_t *const f,
	const char *name,
	const h5_int64_t *buffer,
	const h5_int64_t *nelem,
	const int l_name
	) {

	h5_file_t *fh = h5_filehandlefor2c(f);
	char *name2 = h5_strdupfor2c ( name, l_name );
	H5_API_ENTER (h5_err_t,
		      "f=%p, name='%s', buffer=%p, nelem=%lld, l_name=%d",
		      fh, name2, buffer, (long long)*nelem, l_name);

	h5_err_t herr = h5_write_attrib(
		fh, H5_ATTRIB_FILE, name2,
		H5_INT64_T, buffer, (hsize_t)*nelem);

	free ( name2 );
	H5_API_RETURN(herr);
}

#if ! defined(F77_NO_UNDERSCORE)
#define h5_readfileattrib_i8 F77NAME ( \
	h5_readfileattrib_i8_, \
	H5_READFILEATTRIB_I8 )
#endif

h5_err_t
h5bl_readfileattrib_i8 (
	h5_int64_t *const f,
	const char *name,
	h5_float64_t *buffer,
	const int l_name
	) {

	h5_file_t *fh = h5_filehandlefor2c(f);
	char *name2 = h5_strdupfor2c ( name, l_name );
	H5_API_ENTER (h5_err_t,
		      "f=%p, name='%s', buffer=%p, l_name=%d",
		      fh, name2, buffer, l_name);

	h5_err_t herr = h5_read_attrib(
		fh, H5_ATTRIB_FILE, name2, H5_INT64_T, buffer);

	free ( name2 );
	H5_API_RETURN(herr);
}

#if ! defined(F77_NO_UNDERSCORE)
#define h5_writefileattrib_i4 F77NAME ( \
	h5_writefileattrib_i4_, \
	H5_WRITEFILEATTRIB_I4 )
#endif

h5_err_t
h5_writefileattrib_i4 (
	h5_int64_t *const f,
	const char *name,
	const h5_int32_t *buffer,
	const h5_int64_t *nelem,
	const int l_name
	) {

	h5_file_t *fh = h5_filehandlefor2c(f);
	char *name2 = h5_strdupfor2c ( name, l_name );
	H5_API_ENTER (h5_err_t,
		      "f=%p, name='%s', buffer=%p, nelem=%lld, l_name=%d",
		      fh, name2, buffer, (long long)*nelem, l_name);

	h5_err_t herr = h5_write_attrib(
		fh, H5_ATTRIB_FILE, name2,
		H5_INT32_T, buffer, (hsize_t)*nelem);

	free ( name2 );
	H5_API_RETURN(herr);
}

#if ! defined(F77_NO_UNDERSCORE)
#define h5_readfileattrib_i4 F77NAME ( \
	h5_readfileattrib_i4_, \
	H5_READFILEATTRIB_I4 )
#endif

h5_err_t
h5bl_readfileattrib_i4 (
	h5_int64_t *const f,
	const char *name,
	h5_float64_t *buffer,
	const int l_name
	) {

	h5_file_t *fh = h5_filehandlefor2c(f);
	char *name2 = h5_strdupfor2c ( name, l_name );
	H5_API_ENTER (h5_err_t,
		      "f=%p, name='%s', buffer=%p, l_name=%d",
		      fh, name2, buffer, l_name);

	h5_err_t herr = h5_read_attrib(
		fh, H5_ATTRIB_FILE, name2, H5_INT32_T, buffer);

	free ( name2 );
	H5_API_RETURN(herr);
}

#if ! defined(F77_NO_UNDERSCORE)
#define h5_writestepattrib_r8 F77NAME ( \
	h5_writestepattrib_r8_, \
	H5_WRITESTEPATTRIB_R8 )
#endif

h5_err_t
h5_writestepattrib_r8 (
	h5_int64_t *const f,
	const char *name,
	const h5_float64_t *buffer,
	const h5_int64_t *nelem,
	const int l_name
	) {

	h5_file_t *fh = h5_filehandlefor2c(f);
	char *name2 = h5_strdupfor2c ( name, l_name );
	H5_API_ENTER (h5_err_t,
		      "f=%p, name='%s', buffer=%p, nelem=%lld, l_name=%d",
		      fh, name2, buffer, (long long)*nelem, l_name);

	h5_err_t herr = h5_write_attrib(
		fh, H5_ATTRIB_STEP, name2,
		H5_FLOAT64_T, buffer, (hsize_t)*nelem);

	free ( name2 );
	H5_API_RETURN(herr);
}

#if ! defined(F77_NO_UNDERSCORE)
#define h5_readstepattrib_r8 F77NAME ( \
	h5_readstepattrib_r8_, \
	H5_READSTEPATTRIB_R8 )
#endif

h5_err_t
h5bl_readstepattrib_r8 (
	h5_int64_t *const f,
	const char *name,
	h5_float64_t *buffer,
	const int l_name
	) {

	h5_file_t *fh = h5_filehandlefor2c(f);
	char *name2 = h5_strdupfor2c ( name, l_name );
	H5_API_ENTER (h5_err_t,
		      "f=%p, name='%s', buffer=%p, l_name=%d",
		      fh, name2, buffer, l_name);

	h5_err_t herr = h5_read_attrib(
		fh, H5_ATTRIB_STEP, name2, H5_FLOAT64_T, buffer);

	free ( name2 );
	H5_API_RETURN(herr);
}

#if ! defined(F77_NO_UNDERSCORE)
#define h5_writestepattrib_r4 F77NAME ( \
	h5_writestepattrib_r4_, \
	H5_WRITESTEPATTRIB_R4 )
#endif

h5_err_t
h5_writestepattrib_r4 (
	h5_int64_t *const f,
	const char *name,
	const h5_float32_t *buffer,
	const h5_int64_t *nelem,
	const int l_name
	) {

	h5_file_t *fh = h5_filehandlefor2c(f);
	char *name2 = h5_strdupfor2c ( name, l_name );
	H5_API_ENTER (h5_err_t,
		      "f=%p, name='%s', buffer=%p, nelem=%lld, l_name=%d",
		      fh, name2, buffer, (long long)*nelem, l_name);

	h5_err_t herr = h5_write_attrib(
		fh, H5_ATTRIB_STEP, name2,
		H5_FLOAT32_T, buffer, (hsize_t)*nelem);

	free ( name2 );
	H5_API_RETURN(herr);
}

#if ! defined(F77_NO_UNDERSCORE)
#define h5_readstepattrib_r4 F77NAME ( \
	h5_readstepattrib_r4_, \
	H5_READSTEPATTRIB_R4 )
#endif

h5_err_t
h5bl_readstepattrib_r4 (
	h5_int64_t *const f,
	const char *name,
	h5_float64_t *buffer,
	const int l_name
	) {

	h5_file_t *fh = h5_filehandlefor2c(f);
	char *name2 = h5_strdupfor2c ( name, l_name );
	H5_API_ENTER (h5_err_t,
		      "f=%p, name='%s', buffer=%p, l_name=%d",
		      fh, name2, buffer, l_name);

	h5_err_t herr = h5_read_attrib(
		fh, H5_ATTRIB_STEP, name2, H5_FLOAT32_T, buffer);

	free ( name2 );
	H5_API_RETURN(herr);
}

#if ! defined(F77_NO_UNDERSCORE)
#define h5_writestepattrib_i8 F77NAME ( \
	h5_writestepattrib_i8_, \
	H5_WRITESTEPATTRIB_I8 )
#endif

h5_err_t
h5_writestepattrib_i8 (
	h5_int64_t *const f,
	const char *name,
	const h5_int64_t *buffer,
	const h5_int64_t *nelem,
	const int l_name
	) {

	h5_file_t *fh = h5_filehandlefor2c(f);
	char *name2 = h5_strdupfor2c ( name, l_name );
	H5_API_ENTER (h5_err_t,
		      "f=%p, name='%s', buffer=%p, nelem=%lld, l_name=%d",
		      fh, name2, buffer, (long long)*nelem, l_name);

	h5_err_t herr = h5_write_attrib(
		fh, H5_ATTRIB_STEP, name2,
		H5_INT64_T, buffer, (hsize_t)*nelem);

	free ( name2 );
	H5_API_RETURN(herr);
}

#if ! defined(F77_NO_UNDERSCORE)
#define h5_readstepattrib_i8 F77NAME ( \
	h5_readstepattrib_i8_, \
	H5_READSTEPATTRIB_I8 )
#endif

h5_err_t
h5bl_readstepattrib_i8 (
	h5_int64_t *const f,
	const char *name,
	h5_float64_t *buffer,
	const int l_name
	) {

	h5_file_t *fh = h5_filehandlefor2c(f);
	char *name2 = h5_strdupfor2c ( name, l_name );
	H5_API_ENTER (h5_err_t,
		      "f=%p, name='%s', buffer=%p, l_name=%d",
		      fh, name2, buffer, l_name);

	h5_err_t herr = h5_read_attrib(
		fh, H5_ATTRIB_STEP, name2, H5_INT64_T, buffer);

	free ( name2 );
	H5_API_RETURN(herr);
}

#if ! defined(F77_NO_UNDERSCORE)
#define h5_writestepattrib_i4 F77NAME ( \
	h5_writestepattrib_i4_, \
	H5_WRITESTEPATTRIB_I4 )
#endif

h5_err_t
h5_writestepattrib_i4 (
	h5_int64_t *const f,
	const char *name,
	const h5_int32_t *buffer,
	const h5_int64_t *nelem,
	const int l_name
	) {

	h5_file_t *fh = h5_filehandlefor2c(f);
	char *name2 = h5_strdupfor2c ( name, l_name );
	H5_API_ENTER (h5_err_t,
		      "f=%p, name='%s', buffer=%p, nelem=%lld, l_name=%d",
		      fh, name2, buffer, (long long)*nelem, l_name);

	h5_err_t herr = h5_write_attrib(
		fh, H5_ATTRIB_STEP, name2,
		H5_INT32_T, buffer, (hsize_t)*nelem);

	free ( name2 );
	H5_API_RETURN(herr);
}

#if ! defined(F77_NO_UNDERSCORE)
#define h5_readstepattrib_i4 F77NAME ( \
	h5_readstepattrib_i4_, \
	H5_READSTEPATTRIB_I4 )
#endif

h5_err_t
h5bl_readstepattrib_i4 (
	h5_int64_t *const f,
	const char *name,
	h5_float64_t *buffer,
	const int l_name
	) {

	h5_file_t *fh = h5_filehandlefor2c(f);
	char *name2 = h5_strdupfor2c ( name, l_name );
	H5_API_ENTER (h5_err_t,
		      "f=%p, name='%s', buffer=%p, l_name=%d",
		      fh, name2, buffer, l_name);

	h5_err_t herr = h5_read_attrib(
		fh, H5_ATTRIB_STEP, name2, H5_INT32_T, buffer);

	free ( name2 );
	H5_API_RETURN(herr);
}
