#include "H5Part.h"
#include "H5Block.h"
#include "Underscore.h"
#include <hdf5.h>


#ifdef F77_SINGLE_UNDERSCORE
#define F77NAME(a,b,c) a
#elif defined(F77_NO_UNDERSCORE)
#define F77NAME(a,b,c) b
#elif defined(F77_CRAY_UNDERSCORE)
#define F77NAME(a,b,c) c
#else
#error Error, no way to determine how to construct fortran bindings
#endif


#define f_h5bl_define3dlayout F77NAME (					\
					h5bl_define3dlayout_,		\
					h5bl_define3dlayout,		\
					H5BL_DEFINE3DLAYOUT )
#define f_h5bl_get_partition_of_proc F77NAME (				\
					h5bl_get_partition_of_proc_,	\
					h5bl_get_partition_of_proc,	\
					H5BL_GET_PARTITION_OF_PROC )
#define f_h5bl_get_reduced_partition_of_proc F77NAME (			\
					h5bl_get_reduced_partition_of_proc_,\
					h5bl_get_reduced_partition_of_proc,\
					H5BL_GET_REDUCED_PARTITION_OF_PROC )
#define f_h5bl_get_proc_of F77NAME (					\
					h5bl_get_proc_of_,		\
					h5bl_get_proc_of,		\
					H5BL_GET_PROC_OF )
#define f_h5bl_3d_read_scalar_field F77NAME (				\
					h5bl_3d_read_scalar_field_,	\
					h5bl_3d_read_scalar_field,	\
					H5BL_3D_READ_SCALAR_FIELD )
#define f_h5bl_3d_write_scalar_field F77NAME (				\
					h5bl_3d_write_scalar_field_,	\
					h5bl_3d_write_scalar_field,	\
					H5BL_3D_WRITE_SCALAR_FIELD )
#define f_h5bl_3d_read_3dvector_field F77NAME (				\
					h5bl_3d_read_3dvector_field_,	\
					h5bl_3d_read_3dvector_field,	\
					H5BL_3D_READ_3DVECTOR_FIELD )
#define f_h5bl_3d_write_3dvector_field F77NAME (			\
					h5bl_3d_write_3dvector_field_,	\
					h5bl_3d_write_3dvector_field,	\
					H5BL_3D_WRITE_3DVECTOR_FIELD )

#define f_h5bl_getnumfields F77NAME (					\
					h5bl_getnumfields_,		\
					h5bl_getnumfields,		\
					H5BL_GETNUMFIELDS )
#define f_h5bl_getfieldinfo F77NAME (					\
					h5bl_getfieldinfo_,		\
					h5bl_getfieldinfo,		\
					H5BL_GETFIELDINFO )

#define f_h5bl_writefieldattrib_r8 F77NAME (				\
					h5bl_writefieldattrib_r8_,	\
					h5bl_writefieldattrib_r8,	\
					H5BL_WRITEFIELDATTRIB_R8 )
#define f_h5bl_writefieldattrib_i8 F77NAME (				\
					h5bl_writefieldattrib_i8_,	\
					h5bl_writefieldattrib_i8,	\
					H5BL_WRITEFIELDATTRIB_I8 )
#define f_h5bl_writefieldattrib_string F77NAME (			\
					h5bl_writefieldattrib_string_,	\
					h5bl_writefieldattrib_string,	\
					H5BL_WRITEFIELDATTRIB_STRING )
#define f_h5bl_getnfieldattribs F77NAME (				\
					h5bl_getnfieldattribs_,		\
					h5bl_getnfieldattribs,		\
					H5BL_GETNFIELDATTRIBS )
#define f_h5bl_getfieldattribinfo F77NAME (				\
					h5bl_getfieldattribinfo_,	\
					h5bl_getfieldattribinfo,	\
					h5bl_getfieldattribinfo )
#define f_h5bl_readfieldattrib_i8 F77NAME (				\
					h5bl_readfieldattrib_i8_,	\
					h5bl_readfieldattrib_i8,	\
					H5BL_READFIELDATTRIB_I8 )
#define f_h5bl_readfieldattrib_r8 F77NAME (				\
					h5bl_readfieldattrib_r8_,	\
					h5bl_readfieldattrib_r8,	\
					H5BL_READFIELDATTRIB_R8 )
#define f_h5bl_readfieldattrib_string F77NAME (				\
					h5bl_readfieldattrib_string_,	\
					h5bl_readfieldattrib_string,	\
					H5BL_READFIELDATTRIB_STRING )
#define f_h5bl_has_fielddata F77NAME (					\
					h5bl_has_fielddata_,		\
					h5bl_has_fielddata,		\
					H5BL_HAS_FIELDDATA )



h5part_int64_t
f_h5bl_define3dlayout (
	h5part_int64_t *f,
	const h5part_int64_t *i_start,	/*!< start index of i */
	const h5part_int64_t *i_end,	/*!< end index of i */
	const h5part_int64_t *j_start,	/*!< start index of j */
	const h5part_int64_t *j_end,	/*!< end index of j */
	const h5part_int64_t *k_start,	/*!< start index of k */
	const h5part_int64_t *k_end	/*!< end index of k */
	) {

	H5PartFile *filehandle = (H5PartFile*)(size_t)*f;

	return H5BlockDefine3DFieldLayout (
		filehandle,
		*i_start-1, *i_end-1,
		*j_start-1, *j_end-1,
		*k_start-1, *k_end-1 );
}

h5part_int64_t
f_h5bl_get_partition_of_proc (
	h5part_int64_t *f,			/*!< file handle */
	const h5part_int64_t *proc,
	h5part_int64_t *i_start,	/*!< start index of i */
	h5part_int64_t *i_end,		/*!< end index of i */
	h5part_int64_t *j_start,	/*!< start index of j */
	h5part_int64_t *j_end,		/*!< end index of j */
	h5part_int64_t *k_start,	/*!< start index of k */
	h5part_int64_t *k_end		/*!< end index of k */
	) {

	H5PartFile *filehandle = (H5PartFile*)(size_t)*f;

	return H5Block3dGetPartitionOfProc (
		filehandle,
		*proc,
		i_start, i_end, j_start, j_end, k_start, k_end );
}

h5part_int64_t
f_h5bl_get_reduced_partition_of_proc (
	h5part_int64_t *f,
	const h5part_int64_t *proc,
	h5part_int64_t *i_start, 
	h5part_int64_t *i_end,
	h5part_int64_t *j_start,
	h5part_int64_t *j_end,
	h5part_int64_t *k_start,
	h5part_int64_t *k_end
	) {

	H5PartFile *filehandle = (H5PartFile*)(size_t)*f;

	return H5Block3dGetReducedPartitionOfProc (
		filehandle,
		*proc,
		i_start, i_end, j_start, j_end, k_start, k_end );
}

h5part_int64_t
f_h5bl_get_proc_of (
	h5part_int64_t *f,
	const h5part_int64_t *i,
	const h5part_int64_t *j,
	const h5part_int64_t *k
	) {

	H5PartFile *filehandle = (H5PartFile*)(size_t)*f;

	return H5Block3dGetProcOf ( filehandle, *i, *j, *k );
}

h5part_int64_t
f_h5bl_3d_read_scalar_field (
	h5part_int64_t *f,
	const char *field_name,
	h5part_float64_t *data,
	const int l_field_name
	) {

	H5PartFile *filehandle = (H5PartFile*)(size_t)*f;

	char *field_name2 = (char*)malloc(l_field_name+1);
	strncpy ( field_name2, field_name, l_field_name );
	field_name2[l_field_name] = '\0';

	h5part_int64_t herr = H5Block3dReadScalarField (
		filehandle, field_name2, data );

	free ( field_name2 );
	return herr;
}

h5part_int64_t
f_h5bl_3d_write_scalar_field (
	h5part_int64_t *f,
	const char *field_name,
	const h5part_float64_t *data,
	const int l_field_name
	) {

	H5PartFile *filehandle = (H5PartFile*)(size_t)*f;

	char *field_name2 = (char*)malloc(l_field_name+1);
	strncpy ( field_name2, field_name, l_field_name );
	field_name2[l_field_name] = '\0';

	h5part_int64_t herr = H5Block3dWriteScalarField (
		filehandle, field_name2, data );

	free ( field_name2 );
	return herr;
}

h5part_int64_t
f_h5bl_3d_read_3dvector_field (
	h5part_int64_t *f,		/*!< file handle */
	const char *field_name,		/*!< name of the data set */
	h5part_float64_t *xval,		/*!< array of x component data */
	h5part_float64_t *yval,		/*!< array of y component data */
	h5part_float64_t *zval,		/*!< array of z component data */
	const int l_field_name
	) {

	H5PartFile *filehandle = (H5PartFile*)(size_t)*f;

	char *field_name2 = (char*)malloc(l_field_name+1);
	strncpy ( field_name2, field_name, l_field_name );
	field_name2[l_field_name] = '\0';

	h5part_int64_t herr = H5Block3dRead3dVectorField (
		filehandle, field_name2, xval, yval, zval );

	free ( field_name2 );
	return herr;
}

h5part_int64_t
f_h5bl_3d_write_3dvector_field (
	h5part_int64_t *f,			/*!< file handle */
	const char *field_name,		/*!< name of the data set */
	const h5part_float64_t *xval,	/*!< array of x component data */
	const h5part_float64_t *yval,	/*!< array of y component data */
	const h5part_float64_t *zval,	/*!< array of z component data */
	const int l_field_name
	) {

	H5PartFile *filehandle = (H5PartFile*)(size_t)*f;

	char *field_name2 = (char*)malloc(l_field_name+1);
	strncpy ( field_name2, field_name, l_field_name );
	field_name2[l_field_name] = '\0';

	h5part_int64_t herr = H5Block3dWrite3dVectorField (
		filehandle, field_name2, xval, yval, zval );

	free ( field_name2 );
	return herr;
}

h5part_int64_t
f_h5bl_getnumfields (
	h5part_int64_t *f			/*!< file handle */
	) {

	H5PartFile *filehandle = (H5PartFile*)(size_t)*f;

	return H5BlockGetNumFields ( filehandle );
}

h5part_int64_t
f_h5bl_getfieldinfo (
	h5part_int64_t *f,
	const h5part_int64_t *idx,
	char *field_name,
	h5part_int64_t *grid_rank,
	h5part_int64_t *grid_dims,
	h5part_int64_t *field_dims,
	const int l_field_name
	) {

	H5PartFile *filehandle = (H5PartFile*)(size_t)*f;

	return  H5BlockGetFieldInfo (
		filehandle, *idx, field_name, l_field_name, grid_rank, grid_dims, field_dims );
}

h5part_int64_t
f_h5bl_writefieldattrib_r8 (
	h5part_int64_t *f,
	const char *field_name,
	const char *attrib_name,
	const h5part_float64_t *attrib_value,
	const h5part_int64_t *attrib_nelem,
	const int l_field_name,
	const int l_attrib_name
	) {

	H5PartFile *filehandle = (H5PartFile*)(size_t)*f;

	char *field_name2 = (char*)malloc(l_field_name+1);
	strncpy ( field_name2, field_name, l_field_name );
	field_name2[l_field_name] = '\0';

	char *attrib_name2 = (char*)malloc(l_attrib_name+1);
	strncpy ( attrib_name2, attrib_name, l_attrib_name );
	attrib_name2[l_attrib_name] = '\0';

	h5part_int64_t herr = H5BlockWriteFieldAttrib (
		filehandle, field_name2, attrib_name2, H5PART_FLOAT64,
		attrib_value, *attrib_nelem );

	free ( field_name2 );
	free ( attrib_name2 );
	return herr;
}


h5part_int64_t
f_h5bl_writefieldattrib_i8 (
	h5part_int64_t *f,
	const char *field_name,
	const char *attrib_name,
	const h5part_int64_t *attrib_value,
	const h5part_int64_t *attrib_nelem,
	const int l_field_name,
	const int l_attrib_name
	) {

	H5PartFile *filehandle = (H5PartFile*)(size_t)*f;

	char *field_name2 = (char*)malloc(l_field_name+1);
	strncpy ( field_name2, field_name, l_field_name );
	field_name2[l_field_name] = '\0';

	char *attrib_name2 = (char*)malloc(l_attrib_name+1);
	strncpy ( attrib_name2, attrib_name, l_attrib_name );
	attrib_name2[l_attrib_name] = '\0';

	h5part_int64_t herr = H5BlockWriteFieldAttrib (
		filehandle, field_name2, attrib_name2, H5PART_INT64,
		attrib_value, *attrib_nelem );

	free ( field_name2 );
	free ( attrib_name2 );
	return herr;
}

h5part_int64_t
f_h5bl_writefieldattrib_string (
	h5part_int64_t *f,
	const char *field_name,
	const char *attrib_name,
	const char *attrib_value,
	const int l_field_name,
	const int l_attrib_name,
	const int l_attrib_value
	) {

	H5PartFile *filehandle = (H5PartFile*)(size_t)*f;

	char *field_name2 = (char*)malloc(l_field_name+1);
	strncpy ( field_name2, field_name, l_field_name );
	field_name2[l_field_name] = '\0';

	char *attrib_name2 = (char*)malloc(l_attrib_name+1);
	strncpy ( attrib_name2, attrib_name, l_attrib_name );
	attrib_name2[l_attrib_name] = '\0';

	char *attrib_value2 = (char*)malloc(l_attrib_value+1);
	strncpy ( attrib_value2, attrib_value, l_attrib_value );
	attrib_value2[l_attrib_value] = '\0';

	h5part_int64_t herr = H5BlockWriteFieldAttribString (
		filehandle, field_name2, attrib_name2, attrib_value2 );

	free ( field_name2 );
	free ( attrib_name2 );
	free ( attrib_value2 );
	return herr;
}

h5part_int64_t
f_h5bl_getnfieldattribs (
	h5part_int64_t *f,
	const char *field_name,
	const int l_field_name
	) {

	H5PartFile *filehandle = (H5PartFile*)(size_t)*f;

	char *field_name2 = (char*)malloc(l_field_name+1);
	strncpy ( field_name2, field_name, l_field_name );
	field_name2[l_field_name] = '\0';

	h5part_int64_t herr = H5BlockGetNumFieldAttribs (
		filehandle, field_name2 );
	
	free ( field_name2 );
	return herr;
}

h5part_int64_t
f_h5bl_getfieldattribinfo (
	h5part_int64_t *f,
	const char *field_name,
	const h5part_int64_t *attrib_idx,
	char *attrib_name,
	h5part_int64_t *attrib_nelem,
	const int l_field_name,
	const int l_attrib_name
	) {

	H5PartFile *filehandle = (H5PartFile*)(size_t)*f;

	h5part_int64_t	attrib_type;

	char *field_name2 = (char*)malloc(l_field_name+1);
	strncpy ( field_name2, field_name, l_field_name );
	field_name2[l_field_name] = '\0';

	h5part_int64_t herr = H5BlockGetFieldAttribInfo (
		filehandle, field_name2, *attrib_idx,
		attrib_name, l_attrib_name+1,
		&attrib_type,
		attrib_nelem );

	free ( field_name2 );
	return herr;
}


h5part_int64_t
f_h5bl_readfieldattrib_i8 (
	h5part_int64_t *f,
	const char *field_name,
	const char *attrib_name,
	h5part_int64_t *attrib_value,
	const int l_field_name,
	const int l_attrib_name
	) {

	H5PartFile *filehandle = (H5PartFile*)(size_t)*f;

	char *field_name2 = (char*)malloc(l_field_name+1);
	strncpy ( field_name2, field_name, l_field_name );
	field_name2[l_field_name] = '\0';

	char *attrib_name2 = (char*)malloc(l_attrib_name+1);
	strncpy ( attrib_name2, attrib_name, l_attrib_name );
	attrib_name2[l_attrib_name] = '\0';

	h5part_int64_t herr = H5BlockReadFieldAttrib (
		filehandle, field_name2, attrib_name2, attrib_value );

	free ( field_name2 );
	free ( attrib_name2 );
	return herr;
}

h5part_int64_t
f_h5bl_readfieldattrib_r8 (
	h5part_int64_t *f,
	const char *field_name,
	const char *attrib_name,
	h5part_float64_t *attrib_value,
	const int l_field_name,
	const int l_attrib_name
	) {

	H5PartFile *filehandle = (H5PartFile*)(size_t)*f;

	char *field_name2 = (char*)malloc(l_field_name+1);
	strncpy ( field_name2, field_name, l_field_name );
	field_name2[l_field_name] = '\0';

	char *attrib_name2 = (char*)malloc(l_attrib_name+1);
	strncpy ( attrib_name2, attrib_name, l_attrib_name );
	attrib_name2[l_attrib_name] = '\0';

	h5part_int64_t herr = H5BlockReadFieldAttrib (
		filehandle, field_name2, attrib_name2, attrib_value );

	free ( field_name2 );
	free ( attrib_name2 );
	return herr;
}

h5part_int64_t
f_h5bl_readfieldattrib_string (
	h5part_int64_t *f,
	const char *field_name,
	const char *attrib_name,
	char *attrib_value,
	const int l_field_name,
	const int l_attrib_name,
	const int l_attrib_value
	) {

	H5PartFile *filehandle = (H5PartFile*)(size_t)*f;

	char *field_name2 = (char*)malloc(l_field_name+1);
	strncpy ( field_name2, field_name, l_field_name );
	field_name2[l_field_name] = '\0';

	char *attrib_name2 = (char*)malloc(l_attrib_name+1);
	strncpy ( attrib_name2, attrib_name, l_attrib_name );
	attrib_name2[l_attrib_name] = '\0';

	h5part_int64_t herr = H5BlockReadFieldAttrib (
		filehandle, field_name2, attrib_name2, attrib_value );

	free ( field_name2 );
	free ( attrib_name2 );
	return herr;
}

h5part_int64_t
f_h5bl_has_fielddata (
	h5part_int64_t *f
	) {

	H5PartFile *filehandle = (H5PartFile*)(size_t)*f;

	return H5BlockHasFieldData ( filehandle );
}
