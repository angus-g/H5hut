
#include <stdlib.h>

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

#if !defined(F77_NO_UNDERSCORE)
#define h5bl_3d_write_scalar_field_r8 F77NAME ( \
                h5bl_3d_write_scalar_field_r8_, \
                H5BL_3D_WRITE_SCALAR_FIELD_R8 )
#endif

h5_err_t
h5bl_3d_write_scalar_field_r8 (
        h5_int64_t *const f,
        const char *name,
        const h5_float64_t *buffer,
        const int l_name
        ) {

	h5_file_t *fh = h5_filehandlefor2c(f);
	H5_API_ENTER (h5_err_t,
	              "f=%p, name='%s', buffer=%p, l_name=%d",
	              fh, name, buffer, l_name);
	char *name2 =  h5_strdupfor2c ( name, l_name );
	h5_err_t herr = h5b_write_scalar_data (
	        fh, name2, (void*)buffer, H5T_NATIVE_DOUBLE );
	free ( name2 );
	H5_API_RETURN(herr);
}

#if !defined(F77_NO_UNDERSCORE)
#define h5bl_3d_read_scalar_field_r8 F77NAME ( \
                h5bl_3d_read_scalar_field_r8_, \
                H5BL_3D_READ_SCALAR_FIELD_R8 )
#endif

h5_err_t
h5bl_3d_read_scalar_field_r8 (
        h5_int64_t *const f,
        const char *name,
        h5_float64_t *buffer,
        const int l_name
        ) {

	h5_file_t *fh = h5_filehandlefor2c(f);
	H5_API_ENTER (h5_err_t,
	              "f=%p, name='%s', buffer=%p, l_name=%d",
	              fh, name, buffer, l_name);
	char *name2 =  h5_strdupfor2c ( name,  l_name );
	h5_err_t herr = h5b_read_scalar_data (
	        fh, name2, buffer, H5T_NATIVE_DOUBLE );
	free ( name2 );
	H5_API_RETURN(herr);
}

#if !defined(F77_NO_UNDERSCORE)
#define h5bl_3d_write_vector3d_field_r8 F77NAME ( \
                h5bl_3d_write_vector3d_field_r8_, \
                H5BL_3D_WRITE_VECTOR3D_FIELD_R8 )
#endif

h5_err_t
h5bl_3d_write_vector3d_field_r8 (
        h5_int64_t *const f,
        const char *name,
        const h5_float64_t *x_buf,
        const h5_float64_t *y_buf,
        const h5_float64_t *z_buf,
        const int l_name
        ) {

	h5_file_t *fh = h5_filehandlefor2c(f);
	H5_API_ENTER (h5_err_t,
	              "f=%p, name='%s', x_buf=%p, y_buf=%p, z_buf=%p, l_name=%d",
	              fh, name, x_buf, y_buf, z_buf, l_name);
	char *name2 =  h5_strdupfor2c ( name,  l_name );
	h5_err_t herr = h5b_write_vector3d_data (
	        fh, name2,
	        (void*)x_buf, (void*)y_buf, (void*)z_buf, H5T_NATIVE_DOUBLE );
	free ( name2 );
	H5_API_RETURN(herr);
}

#if !defined(F77_NO_UNDERSCORE)
#define h5bl_3d_read_vector3d_field_r8 F77NAME ( \
                h5bl_3d_read_vector3d_field_r8_, \
                H5BL_3D_READ_VECTOR3D_FIELD_R8 )
#endif

h5_err_t
h5bl_3d_read_vector3d_field_r8 (
        h5_int64_t *const f,
        const char *name,
        h5_float64_t *x_buf,
        h5_float64_t *y_buf,
        h5_float64_t *z_buf,
        const int l_name
        ) {
	h5_file_t *fh = h5_filehandlefor2c(f);
	H5_API_ENTER (h5_err_t,
	              "f=%p, name='%s', x_buf=%p, y_buf=%p, z_buf=%p, l_name=%d",
	              fh, name, x_buf, y_buf, z_buf, l_name);
	char *name2 =  h5_strdupfor2c ( name,  l_name );
	h5_err_t herr = h5b_read_vector3d_data (
	        fh, name2,
	        (void*)x_buf, (void*)y_buf, (void*)z_buf, H5T_NATIVE_DOUBLE );
	free ( name2 );
	H5_API_RETURN(herr);
}

#if !defined(F77_NO_UNDERSCORE)
#define h5bl_3d_write_scalar_field_r4 F77NAME ( \
                h5bl_3d_write_scalar_field_r4_, \
                H5BL_3D_WRITE_SCALAR_FIELD_R4 )
#endif

h5_err_t
h5bl_3d_write_scalar_field_r4 (
        h5_int64_t *const f,
        const char *name,
        const h5_float32_t *buffer,
        const int l_name
        ) {
	h5_file_t *fh = h5_filehandlefor2c(f);
	H5_API_ENTER (h5_err_t,
	              "f=%p, name='%s', buffer=%p, l_name=%d",
	              fh, name, buffer, l_name);
	char *name2 =  h5_strdupfor2c ( name, l_name );
	h5_err_t herr = h5b_write_scalar_data (
	        fh, name2, (void*)buffer, H5T_NATIVE_FLOAT );
	free ( name2 );
	H5_API_RETURN(herr);
}

#if !defined(F77_NO_UNDERSCORE)
#define h5bl_3d_read_scalar_field_r4 F77NAME ( \
                h5bl_3d_read_scalar_field_r4_, \
                H5BL_3D_READ_SCALAR_FIELD_R4 )
#endif

h5_err_t
h5bl_3d_read_scalar_field_r4 (
        h5_int64_t *const f,
        const char *name,
        h5_float32_t *buffer,
        const int l_name
        ) {
	h5_file_t *fh = h5_filehandlefor2c(f);
	H5_API_ENTER (h5_err_t,
	              "f=%p, name='%s', buffer=%p, l_name=%d",
	              fh, name, buffer, l_name);
	char *name2 =  h5_strdupfor2c ( name,  l_name );
	h5_err_t herr = h5b_read_scalar_data (
	        fh, name2, buffer, H5T_NATIVE_FLOAT );
	free ( name2 );
	H5_API_RETURN(herr);
}

#if !defined(F77_NO_UNDERSCORE)
#define h5bl_3d_write_vector3d_field_r4 F77NAME ( \
                h5bl_3d_write_vector3d_field_r4_, \
                H5BL_3D_WRITE_VECTOR3D_FIELD_R4 )
#endif

h5_err_t
h5bl_3d_write_vector3d_field_r4 (
        h5_int64_t *const f,
        const char *name,
        const h5_float32_t *x_buf,
        const h5_float32_t *y_buf,
        const h5_float32_t *z_buf,
        const int l_name
        ) {
	h5_file_t *fh = h5_filehandlefor2c(f);
	H5_API_ENTER (h5_err_t,
	              "f=%p, name='%s', x_buf=%p, y_buf=%p, z_buf=%p, l_name=%d",
	              fh, name, x_buf, y_buf, z_buf, l_name);
	char *name2 =  h5_strdupfor2c ( name,  l_name );
	h5_err_t herr = h5b_write_vector3d_data (
	        fh, name2,
	        (void*)x_buf, (void*)y_buf, (void*)z_buf, H5T_NATIVE_FLOAT );
	free ( name2 );
	H5_API_RETURN(herr);
}

#if !defined(F77_NO_UNDERSCORE)
#define h5bl_3d_read_vector3d_field_r4 F77NAME ( \
                h5bl_3d_read_vector3d_field_r4_, \
                H5BL_3D_READ_VECTOR3D_FIELD_R4 )
#endif

h5_err_t
h5bl_3d_read_vector3d_field_r4 (
        h5_int64_t *const f,
        const char *name,
        h5_float32_t *x_buf,
        h5_float32_t *y_buf,
        h5_float32_t *z_buf,
        const int l_name
        ) {
	h5_file_t *fh = h5_filehandlefor2c(f);
	H5_API_ENTER (h5_err_t,
	              "f=%p, name='%s', x_buf=%p, y_buf=%p, z_buf=%p, l_name=%d",
	              fh, name, x_buf, y_buf, z_buf, l_name);
	char *name2 =  h5_strdupfor2c ( name,  l_name );
	h5_err_t herr = h5b_read_vector3d_data (
	        fh, name2,
	        (void*)x_buf, (void*)y_buf, (void*)z_buf, H5T_NATIVE_FLOAT );
	free ( name2 );
	H5_API_RETURN(herr);
}

#if !defined(F77_NO_UNDERSCORE)
#define h5bl_3d_write_scalar_field_i8 F77NAME ( \
                h5bl_3d_write_scalar_field_i8_, \
                H5BL_3D_WRITE_SCALAR_FIELD_I8 )
#endif

h5_err_t
h5bl_3d_write_scalar_field_i8 (
        h5_int64_t *const f,
        const char *name,
        const h5_int64_t *buffer,
        const int l_name
        ) {
	h5_file_t *fh = h5_filehandlefor2c(f);
	H5_API_ENTER (h5_err_t,
	              "f=%p, name='%s', buffer=%p, l_name=%d",
	              fh, name, buffer, l_name);
	char *name2 =  h5_strdupfor2c ( name, l_name );
	h5_err_t herr = h5b_write_scalar_data (
	        fh, name2, (void*)buffer, H5T_NATIVE_INT64 );
	free ( name2 );
	H5_API_RETURN(herr);
}

#if !defined(F77_NO_UNDERSCORE)
#define h5bl_3d_read_scalar_field_i8 F77NAME ( \
                h5bl_3d_read_scalar_field_i8_, \
                H5BL_3D_READ_SCALAR_FIELD_I8 )
#endif

h5_err_t
h5bl_3d_read_scalar_field_i8 (
        h5_int64_t *const f,
        const char *name,
        h5_int64_t *buffer,
        const int l_name
        ) {
	h5_file_t *fh = h5_filehandlefor2c(f);
	H5_API_ENTER (h5_err_t,
	              "f=%p, name='%s', buffer=%p, l_name=%d",
	              fh, name, buffer, l_name);
	char *name2 =  h5_strdupfor2c ( name,  l_name );
	h5_err_t herr = h5b_read_scalar_data (
	        fh, name2, buffer, H5T_NATIVE_INT64 );
	free ( name2 );
	H5_API_RETURN(herr);
}

#if !defined(F77_NO_UNDERSCORE)
#define h5bl_3d_write_vector3d_field_i8 F77NAME ( \
                h5bl_3d_write_vector3d_field_i8_, \
                H5BL_3D_WRITE_VECTOR3D_FIELD_I8 )
#endif

h5_err_t
h5bl_3d_write_vector3d_field_i8 (
        h5_int64_t *const f,
        const char *name,
        const h5_int64_t *x_buf,
        const h5_int64_t *y_buf,
        const h5_int64_t *z_buf,
        const int l_name
        ) {
	h5_file_t *fh = h5_filehandlefor2c(f);
	H5_API_ENTER (h5_err_t,
	              "f=%p, name='%s', x_buf=%p, y_buf=%p, z_buf=%p, l_name=%d",
	              fh, name, x_buf, y_buf, z_buf, l_name);
	char *name2 =  h5_strdupfor2c ( name,  l_name );
	h5_err_t herr = h5b_write_vector3d_data (
	        fh, name2,
	        (void*)x_buf, (void*)y_buf, (void*)z_buf, H5T_NATIVE_INT64 );
	free ( name2 );
	H5_API_RETURN(herr);
}

#if !defined(F77_NO_UNDERSCORE)
#define h5bl_3d_read_vector3d_field_i8 F77NAME ( \
                h5bl_3d_read_vector3d_field_i8_, \
                H5BL_3D_READ_VECTOR3D_FIELD_I8 )
#endif

h5_err_t
h5bl_3d_read_vector3d_field_i8 (
        h5_int64_t *const f,
        const char *name,
        h5_int64_t *x_buf,
        h5_int64_t *y_buf,
        h5_int64_t *z_buf,
        const int l_name
        ) {
	h5_file_t *fh = h5_filehandlefor2c(f);
	H5_API_ENTER (h5_err_t,
	              "f=%p, name='%s', x_buf=%p, y_buf=%p, z_buf=%p, l_name=%d",
	              fh, name, x_buf, y_buf, z_buf, l_name);
	char *name2 =  h5_strdupfor2c ( name,  l_name );
	h5_err_t herr = h5b_read_vector3d_data (
	        fh, name2,
	        (void*)x_buf, (void*)y_buf, (void*)z_buf, H5T_NATIVE_INT64 );
	free ( name2 );
	H5_API_RETURN(herr);
}

#if !defined(F77_NO_UNDERSCORE)
#define h5bl_3d_write_scalar_field_i4 F77NAME ( \
                h5bl_3d_write_scalar_field_i4_, \
                H5BL_3D_WRITE_SCALAR_FIELD_I4 )
#endif

h5_err_t
h5bl_3d_write_scalar_field_i4 (
        h5_int64_t *const f,
        const char *name,
        const h5_int32_t *buffer,
        const int l_name
        ) {
	h5_file_t *fh = h5_filehandlefor2c(f);
	H5_API_ENTER (h5_err_t,
	              "f=%p, name='%s', buffer=%p, l_name=%d",
	              fh, name, buffer, l_name);
	char *name2 =  h5_strdupfor2c ( name, l_name );
	h5_err_t herr = h5b_write_scalar_data (
	        fh, name2, (void*)buffer, H5T_NATIVE_INT32 );
	free ( name2 );
	H5_API_RETURN(herr);
}

#if !defined(F77_NO_UNDERSCORE)
#define h5bl_3d_read_scalar_field_i4 F77NAME ( \
                h5bl_3d_read_scalar_field_i4_, \
                H5BL_3D_READ_SCALAR_FIELD_I4 )
#endif

h5_err_t
h5bl_3d_read_scalar_field_i4 (
        h5_int64_t *const f,
        const char *name,
        h5_int32_t *buffer,
        const int l_name
        ) {
	h5_file_t *fh = h5_filehandlefor2c(f);
	H5_API_ENTER (h5_err_t,
	              "f=%p, name='%s', buffer=%p, l_name=%d",
	              fh, name, buffer, l_name);
	char *name2 =  h5_strdupfor2c ( name,  l_name );
	h5_err_t herr = h5b_read_scalar_data (
	        fh, name2, buffer, H5T_NATIVE_INT32 );
	free ( name2 );
	H5_API_RETURN(herr);
}

#if !defined(F77_NO_UNDERSCORE)
#define h5bl_3d_write_vector3d_field_i4 F77NAME ( \
                h5bl_3d_write_vector3d_field_i4_, \
                H5BL_3D_WRITE_VECTOR3D_FIELD_I4 )
#endif

h5_err_t
h5bl_3d_write_vector3d_field_i4 (
        h5_int64_t *const f,
        const char *name,
        const h5_int32_t *x_buf,
        const h5_int32_t *y_buf,
        const h5_int32_t *z_buf,
        const int l_name
        ) {
	h5_file_t *fh = h5_filehandlefor2c(f);
	H5_API_ENTER (h5_err_t,
	              "f=%p, name='%s', x_buf=%p, y_buf=%p, z_buf=%p, l_name=%d",
	              fh, name, x_buf, y_buf, z_buf, l_name);
	char *name2 =  h5_strdupfor2c ( name,  l_name );
	h5_err_t herr = h5b_write_vector3d_data (
	        fh, name2,
	        (void*)x_buf, (void*)y_buf, (void*)z_buf, H5T_NATIVE_INT32 );
	free ( name2 );
	H5_API_RETURN(herr);
}

#if !defined(F77_NO_UNDERSCORE)
#define h5bl_3d_read_vector3d_field_i4 F77NAME ( \
                h5bl_3d_read_vector3d_field_i4_, \
                H5BL_3D_READ_VECTOR3D_FIELD_I4 )
#endif

h5_err_t
h5bl_3d_read_vector3d_field_i4 (
        h5_int64_t *const f,
        const char *name,
        h5_int32_t *x_buf,
        h5_int32_t *y_buf,
        h5_int32_t *z_buf,
        const int l_name
        ) {
	h5_file_t *fh = h5_filehandlefor2c(f);
	H5_API_ENTER (h5_err_t,
	              "f=%p, name='%s', x_buf=%p, y_buf=%p, z_buf=%p, l_name=%d",
	              fh, name, x_buf, y_buf, z_buf, l_name);
	char *name2 =  h5_strdupfor2c ( name,  l_name );
	h5_err_t herr = h5b_read_vector3d_data (
	        fh, name2,
	        (void*)x_buf, (void*)y_buf, (void*)z_buf, H5T_NATIVE_INT32 );
	free ( name2 );
	H5_API_RETURN(herr);
}

#if !defined(F77_NO_UNDERSCORE)
#define h5bl_writefieldattrib_r8 F77NAME ( \
                h5bl_writefieldattrib_r8_, \
                H5BL_WRITEFIELDATTRIB_R8 )
#endif

h5_err_t
h5bl_writefieldattrib_r8 (
        h5_int64_t *const f,
        const char *field_name,
        const char *attrib_name,
        const h5_float64_t *buffer,
        const h5_size_t *nelems,
        const int l_field_name,
        const int l_attrib_name
        ) {
	h5_file_t *fh = h5_filehandlefor2c(f);
	H5_API_ENTER (h5_err_t,
	              "f=%p, field_name='%s', attrib_name='%s', "
	              "buffer=%p, nelems=%lld, l_field_name=%d, l_attrib_name=%d",
	              fh, field_name, attrib_name, buffer, (long long)*nelems,
	              l_field_name, l_attrib_name);
	char *field_name2 = h5_strdupfor2c ( field_name,  l_field_name );
	char *attrib_name2 = h5_strdupfor2c ( attrib_name, l_attrib_name );
	h5_err_t herr = h5_write_field_attrib (
	        fh, field_name2, attrib_name2,
	        H5T_NATIVE_DOUBLE, buffer, *nelems );
	free ( field_name2 );
	free ( attrib_name2 );
	H5_API_RETURN(herr);
}

#if !defined(F77_NO_UNDERSCORE)
#define h5bl_readfieldattrib_r8 F77NAME ( \
                h5bl_readfieldattrib_r8_, \
                H5BL_READFIELDATTRIB_R8 )
#endif

h5_err_t
h5bl_readfieldattrib_r8 (
        h5_int64_t *const f,
        const char *field_name,
        const char *attrib_name,
        h5_float64_t *buffer,
        const int l_field_name,
        const int l_attrib_name
        ) {

	h5_file_t *fh = h5_filehandlefor2c(f);
	H5_API_ENTER (h5_err_t,
	              "f=%p, field_name='%s', attrib_name='%s', "
	              "values=%p, l_field_name=%d, l_attrib_name=%d",
	              fh, field_name, attrib_name, buffer,
	              l_field_name, l_attrib_name);
	char *field_name2 = h5_strdupfor2c ( field_name,  l_field_name );
	char *attrib_name2 = h5_strdupfor2c ( attrib_name, l_attrib_name );
	h5_err_t herr = h5_read_field_attrib (
	        fh, field_name2, attrib_name2, H5T_NATIVE_DOUBLE, buffer );
	free ( field_name2 );
	free ( attrib_name2 );
	H5_API_RETURN(herr);
}

#if !defined(F77_NO_UNDERSCORE)
#define h5bl_writefieldattrib_r4 F77NAME ( \
                h5bl_writefieldattrib_r4_, \
                H5BL_WRITEFIELDATTRIB_R4 )
#endif

h5_err_t
h5bl_writefieldattrib_r4 (
        h5_int64_t *const f,
        const char *field_name,
        const char *attrib_name,
        const h5_float32_t *buffer,
        const h5_size_t *nelems,
        const int l_field_name,
        const int l_attrib_name
        ) {

	h5_file_t *fh = h5_filehandlefor2c(f);
	H5_API_ENTER (h5_err_t,
	              "f=%p, field_name='%s', attrib_name='%s', "
	              "buffer=%p, nelems=%lld, l_field_name=%d, l_attrib_name=%d",
	              fh, field_name, attrib_name, buffer, (long long)*nelems,
	              l_field_name, l_attrib_name);
	char *field_name2 = h5_strdupfor2c ( field_name,  l_field_name );
	char *attrib_name2 = h5_strdupfor2c ( attrib_name, l_attrib_name );
	h5_err_t herr = h5_write_field_attrib (
	        fh, field_name2, attrib_name2,
	        H5T_NATIVE_FLOAT, buffer, *nelems );
	free ( field_name2 );
	free ( attrib_name2 );
	H5_API_RETURN(herr);
}

#if !defined(F77_NO_UNDERSCORE)
#define h5bl_readfieldattrib_r4 F77NAME ( \
                h5bl_readfieldattrib_r4_, \
                H5BL_READFIELDATTRIB_R4 )
#endif

h5_err_t
h5bl_readfieldattrib_r4 (
        h5_int64_t *const f,
        const char *field_name,
        const char *attrib_name,
        h5_float32_t *buffer,
        const int l_field_name,
        const int l_attrib_name
        ) {

	h5_file_t *fh = h5_filehandlefor2c(f);
	H5_API_ENTER (h5_err_t,
	              "f=%p, field_name='%s', attrib_name='%s', "
	              "values=%p, l_field_name=%d, l_attrib_name=%d",
	              fh, field_name, attrib_name, buffer,
	              l_field_name, l_attrib_name);
	char *field_name2 = h5_strdupfor2c ( field_name,  l_field_name );
	char *attrib_name2 = h5_strdupfor2c ( attrib_name, l_attrib_name );
	h5_err_t herr = h5_read_field_attrib (
	        fh, field_name2, attrib_name2, H5T_NATIVE_FLOAT, buffer );
	free ( field_name2 );
	free ( attrib_name2 );
	H5_API_RETURN(herr);
}

#if !defined(F77_NO_UNDERSCORE)
#define h5bl_writefieldattrib_i8 F77NAME ( \
                h5bl_writefieldattrib_i8_, \
                H5BL_WRITEFIELDATTRIB_I8 )
#endif

h5_err_t
h5bl_writefieldattrib_i8 (
        h5_int64_t *const f,
        const char *field_name,
        const char *attrib_name,
        const h5_int64_t *buffer,
        const h5_size_t *nelems,
        const int l_field_name,
        const int l_attrib_name
        ) {

	h5_file_t *fh = h5_filehandlefor2c(f);
	H5_API_ENTER (h5_err_t,
	              "f=%p, field_name='%s', attrib_name='%s', "
	              "buffer=%p, nelems=%lld, l_field_name=%d, l_attrib_name=%d",
	              fh, field_name, attrib_name, buffer, (long long)*nelems,
	              l_field_name, l_attrib_name);
	char *field_name2 = h5_strdupfor2c ( field_name,  l_field_name );
	char *attrib_name2 = h5_strdupfor2c ( attrib_name, l_attrib_name );
	h5_err_t herr = h5_write_field_attrib (
	        fh, field_name2, attrib_name2,
	        H5T_NATIVE_INT64, buffer, *nelems );
	free ( field_name2 );
	free ( attrib_name2 );
	H5_API_RETURN(herr);
}

#if !defined(F77_NO_UNDERSCORE)
#define h5bl_readfieldattrib_i8 F77NAME ( \
                h5bl_readfieldattrib_i8_, \
                H5BL_READFIELDATTRIB_I8 )
#endif

h5_err_t
h5bl_readfieldattrib_i8 (
        h5_int64_t *const f,
        const char *field_name,
        const char *attrib_name,
        h5_int64_t *buffer,
        const int l_field_name,
        const int l_attrib_name
        ) {
	h5_file_t *fh = h5_filehandlefor2c(f);
	H5_API_ENTER (h5_err_t,
	              "f=%p, field_name='%s', attrib_name='%s', "
	              "values=%p, l_field_name=%d, l_attrib_name=%d",
	              fh, field_name, attrib_name, buffer,
	              l_field_name, l_attrib_name);
	char *field_name2 = h5_strdupfor2c ( field_name,  l_field_name );
	char *attrib_name2 = h5_strdupfor2c ( attrib_name, l_attrib_name );
	h5_err_t herr = h5_read_field_attrib (
	        fh, field_name2, attrib_name2, H5T_NATIVE_INT64, buffer );
	free ( field_name2 );
	free ( attrib_name2 );
	H5_API_RETURN(herr);
}

#if !defined(F77_NO_UNDERSCORE)
#define h5bl_writefieldattrib_i4 F77NAME ( \
                h5bl_writefieldattrib_i4_, \
                H5BL_WRITEFIELDATTRIB_I4 )
#endif

h5_err_t
h5bl_writefieldattrib_i4 (
        h5_int64_t *const f,
        const char *field_name,
        const char *attrib_name,
        const h5_int32_t *buffer,
        const h5_size_t *nelems,
        const int l_field_name,
        const int l_attrib_name
        ) {
	h5_file_t *fh = h5_filehandlefor2c(f);
	H5_API_ENTER (h5_err_t,
	              "f=%p, field_name='%s', attrib_name='%s', "
	              "buffer=%p, nelems=%lld, l_field_name=%d, l_attrib_name=%d",
	              fh, field_name, attrib_name, buffer, (long long)*nelems,
	              l_field_name, l_attrib_name);
	char *field_name2 = h5_strdupfor2c ( field_name,  l_field_name );
	char *attrib_name2 = h5_strdupfor2c ( attrib_name, l_attrib_name );
	h5_err_t herr = h5_write_field_attrib (
	        fh, field_name2, attrib_name2,
	        H5T_NATIVE_INT32, buffer, *nelems );
	free ( field_name2 );
	free ( attrib_name2 );
	H5_API_RETURN(herr);
}

#if !defined(F77_NO_UNDERSCORE)
#define h5bl_readfieldattrib_i4 F77NAME ( \
                h5bl_readfieldattrib_i4_, \
                H5BL_READFIELDATTRIB_I4 )
#endif

h5_err_t
h5bl_readfieldattrib_i4 (
        h5_int64_t *const f,
        const char *field_name,
        const char *attrib_name,
        h5_int32_t *buffer,
        const int l_field_name,
        const int l_attrib_name
        ) {
	h5_file_t *fh = h5_filehandlefor2c(f);
	H5_API_ENTER (h5_err_t,
	              "f=%p, field_name='%s', attrib_name='%s', "
	              "values=%p, l_field_name=%d, l_attrib_name=%d",
	              fh, field_name, attrib_name, buffer,
	              l_field_name, l_attrib_name);
	char *field_name2 = h5_strdupfor2c ( field_name,  l_field_name );
	char *attrib_name2 = h5_strdupfor2c ( attrib_name, l_attrib_name );
	h5_err_t herr = h5_read_field_attrib (
	        fh, field_name2, attrib_name2, H5T_NATIVE_INT32, buffer );
	free ( field_name2 );
	free ( attrib_name2 );
	H5_API_RETURN(herr);
}
