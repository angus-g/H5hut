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
	h5_int64_t *n
	) {

	h5_file_t *fh = h5_filehandlefor2c(f);
	H5_API_ENTER2(h5_err_t, "f=0x%p, n=%lld", fh, (long long)*n);
	H5_API_RETURN(h5u_set_num_particles ( fh, *n, 1 ));
}

h5_err_t
h5pt_setnpoints_strided (
	const h5_int64_t *f,
	h5_int64_t *n,
	h5_int64_t *stride
	) {

	h5_file_t *fh = h5_filehandlefor2c(f);
	H5_API_ENTER3(h5_err_t, "f=0x%p, n=%lld, stride=%lld",
				 fh, (long long)*n, (long long)*stride);
	H5_API_RETURN(h5u_set_num_particles ( fh, *n, *stride ));
}

/*==============Reading Data Characteristics============*/

h5_int64_t
h5pt_getndatasets (
	const h5_int64_t *f
	) {

	h5_file_t *fh = h5_filehandlefor2c(f);
	H5_API_ENTER1(h5_int64_t, "f=0x%p", fh);
	H5_API_RETURN(h5u_get_num_datasets ( fh ));
}

h5_int64_t
h5pt_getnpoints (
	const h5_int64_t *f
	) {

	h5_file_t *fh = h5_filehandlefor2c(f);
	H5_API_ENTER1(h5_int64_t, "f=0x%p", fh);
	H5_API_RETURN(h5u_get_num_particles ( fh ));
}

h5_err_t
h5pt_getdatasetname ( 
	const h5_int64_t *f,
	const h5_int64_t *index,
	char *name,
	const int l_name
	) {

	h5_file_t *fh = h5_filehandlefor2c(f);
	H5_API_ENTER4(h5_err_t, "f=0x%p, index=%lld, name=\"%s\", l_name=%d",
				fh, (long long)*index, name, l_name);
	h5_err_t herr =  h5u_get_dataset_info (
		fh, *index, name, l_name, NULL, NULL );
	h5_strc2for ( name, l_name );
	H5_API_RETURN(herr);
}

/*=============Setting and getting views================*/

h5_err_t
h5pt_setview (
	const h5_int64_t *f,
	const h5_int64_t *start,
	const h5_int64_t *end
	) {

	h5_file_t *fh = h5_filehandlefor2c(f);
	H5_API_ENTER3(h5_err_t, "f=0x%p, start=%lld, end=%lld",
				fh, (long long)*start, (long long)*end);
	H5_API_RETURN(h5u_set_view ( fh, (*start)-1, (*end)-1 ));
}

h5_err_t
h5pt_setview_indices (
	const h5_int64_t *f,
	const h5_int64_t *indices,
	const h5_int64_t *nelem
	) {

	h5_file_t *fh = h5_filehandlefor2c(f);
	H5_API_ENTER3(h5_err_t, "f=0x%p, indices=0x%p, nelem=%lld",
				fh, indices, (long long)*nelem);
	H5_API_RETURN(h5u_set_view_indices ( fh, indices, *nelem ));
}

h5_err_t
h5pt_resetview (
	const h5_int64_t *f
	) {

	h5_file_t *fh = h5_filehandlefor2c(f);
	H5_API_ENTER1(h5_err_t, "f=0x%p", fh);
	H5_API_RETURN(h5u_reset_view ( fh ));
}

h5_err_t
h5pt_hasview (
	const h5_int64_t *f
	) {

	h5_file_t *fh = h5_filehandlefor2c(f);
	H5_API_ENTER1(h5_err_t, "f=0x%p", fh);
	H5_API_RETURN(h5u_has_view ( fh ));
}

h5_err_t
h5pt_getview (
	const h5_int64_t *f,
	h5_int64_t *start,
	h5_int64_t *end
	) {

	h5_file_t *fh = h5_filehandlefor2c(f);
	H5_API_ENTER3(h5_err_t, "f=0x%p, start=0x%p, end=0x%p",
				fh, start, end);
	H5_API_RETURN(h5u_get_view ( fh, start, end));
}


/*==================Writing data ============*/
h5_err_t
h5pt_writedata_r8 (
	const h5_int64_t *f,
	const char *name,
	const h5_float64_t *data,
	const int l_name ) {

	h5_file_t *fh = h5_filehandlefor2c(f);
	H5_API_ENTER4(h5_err_t, "f=0x%p, name=\"%s\", data=0x%p, l_name=%d",
				fh, name, data, l_name);
	char *name2 = h5_strdupfor2c ( name, l_name );
	h5_err_t herr = h5u_write_data (
		fh, name2, (void*)data, H5T_NATIVE_DOUBLE );
	free ( name2 );
	H5_API_RETURN(herr);
}

h5_err_t
h5pt_writedata_r4 (
	const h5_int64_t *f,
	const char *name,
	const h5_float32_t *data,
	const int l_name ) {

	h5_file_t *fh = h5_filehandlefor2c(f);
	H5_API_ENTER4(h5_err_t, "f=0x%p, name=\"%s\", data=0x%p, l_name=%d",
				fh, name, data, l_name);
	char *name2 = h5_strdupfor2c ( name, l_name );
	h5_err_t herr = h5u_write_data (
		fh, name2, (void*)data, H5T_NATIVE_FLOAT );
	free ( name2 );
	H5_API_RETURN(herr);
}

h5_err_t
h5pt_writedata_i8 (
	const h5_int64_t *f,
	const char *name,
	const h5_int64_t *data,
	const int l_name ) {

	h5_file_t *fh = h5_filehandlefor2c(f);
	H5_API_ENTER4(h5_err_t, "f=0x%p, name=\"%s\", data=0x%p, l_name=%d",
				fh, name, data, l_name);
	char *name2 = h5_strdupfor2c ( name, l_name );
	h5_err_t herr = h5u_write_data (
		fh, name2, (void*)data, H5T_NATIVE_INT64 );
	free ( name2 );
	H5_API_RETURN(herr);
}

h5_err_t
h5pt_writedata_i4 (
	const h5_int64_t *f,
	const char *name,
	const h5_int32_t *data,
	const int l_name ) {

	h5_file_t *fh = h5_filehandlefor2c(f);
	H5_API_ENTER4(h5_err_t, "f=0x%p, name=\"%s\", data=0x%p, l_name=%d",
				fh, name, data, l_name);
	char *name2 = h5_strdupfor2c ( name, l_name );
	h5_err_t herr = h5u_write_data (
		fh, name2, (void*)data, H5T_NATIVE_INT32 );
	free ( name2 );
	H5_API_RETURN(herr);
}


/*==================Reading data ============*/
h5_err_t
h5pt_readdata_r8 (
	const h5_int64_t *f,
	const char *name,
	h5_float64_t *data,
	const int l_name
	) {

	h5_file_t *fh = h5_filehandlefor2c(f);
	H5_API_ENTER4(h5_err_t, "f=0x%p, name=\"%s\", data=0x%p, l_name=%d",
				fh, name, data, l_name);
	char *name2 = h5_strdupfor2c ( name, l_name );
	h5_err_t herr = h5u_read_data (
		fh, name2, data, H5T_NATIVE_DOUBLE );
	free ( name2 );
	H5_API_RETURN(herr);
}

h5_err_t
h5pt_readdata_r4 (
	const h5_int64_t *f,
	const char *name,
	h5_float32_t *data,
	const int l_name
	) {

	h5_file_t *fh = h5_filehandlefor2c(f);
	H5_API_ENTER4(h5_err_t, "f=0x%p, name=\"%s\", data=0x%p, l_name=%d",
				fh, name, data, l_name);
	char *name2 = h5_strdupfor2c ( name, l_name );
	h5_err_t herr = h5u_read_data (
		fh, name2, data, H5T_NATIVE_FLOAT );
	free ( name2 );
	H5_API_RETURN(herr);
}

h5_err_t
h5pt_readdata_i8 (
	const h5_int64_t *f,
	const char *name,
	h5_int64_t *data,
	const int l_name
	) {

	h5_file_t *fh = h5_filehandlefor2c(f);
	H5_API_ENTER4(h5_err_t, "f=0x%p, name=\"%s\", data=0x%p, l_name=%d",
				fh, name, data, l_name);
	char *name2 = h5_strdupfor2c ( name, l_name );
	h5_err_t herr = h5u_read_data (
		fh, name2, data, H5T_NATIVE_INT64 );

	free ( name2 );
	H5_API_RETURN(herr);
}

h5_err_t
h5pt_readdata_i4 (
	const h5_int64_t *f,
	const char *name,
	h5_int32_t *data,
	const int l_name
	) {

	h5_file_t *fh = h5_filehandlefor2c(f);
	H5_API_ENTER4(h5_err_t, "f=0x%p, name=\"%s\", data=0x%p, l_name=%d",
				fh, name, data, l_name);
	char *name2 = h5_strdupfor2c ( name, l_name );
	h5_err_t herr = h5u_read_data (
		fh, name2, data, H5T_NATIVE_INT32 );
	free ( name2 );
	H5_API_RETURN(herr);
}

