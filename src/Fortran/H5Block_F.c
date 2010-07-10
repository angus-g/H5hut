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

#define h5bl_3d_setview F77NAME (					\
					h5bl_3d_setview_,		\
					H5BL_3D_SETVIEW )
#define h5bl_3d_getview F77NAME (					\
					h5bl_3d_getview_,		\
					H5BL_3D_GETVIEW )
#define h5bl_3d_setchunk F77NAME (				\
					h5bl_3d_setchunk_,	\
					H5BL_3D_SETCHUNK )
#define h5bl_3d_getreducedview F77NAME (			\
					h5bl_3d_getreducedview_,\
					H5BL_3D_GETREDUCEDVIEW )
#define h5bl_3d_getproc F77NAME (					\
					h5bl_getproc_,		\
					H5BL_GETPROC )
#define h5bl_getnumfields F77NAME (					\
					h5bl_getnumfields_,		\
					H5BL_GETNUMFIELDS )
#define h5bl_getfieldinfo F77NAME (					\
					h5bl_getfieldinfo_,		\
					H5BL_GETFIELDINFO )
#define h5bl_writefieldattrib_string F77NAME (			\
					h5bl_writefieldattrib_string_,	\
					H5BL_WRITEFIELDATTRIB_STRING )
#define h5bl_getnfieldattribs F77NAME (				\
					h5bl_getnfieldattribs_,		\
					H5BL_GETNFIELDATTRIBS )
#define h5bl_getfieldattribinfo F77NAME (				\
					h5bl_getfieldattribinfo_,	\
					h5bl_getfieldattribinfo )
#define h5bl_readfieldattrib_string F77NAME (				\
					h5bl_readfieldattrib_string_,	\
					H5BL_READFIELDATTRIB_STRING )
#endif

h5_err_t
h5bl_3d_setview (
	h5_int64_t *f,
	const h5_int64_t *i_start,	/*!< start index of i */
	const h5_int64_t *i_end,	/*!< end index of i */
	const h5_int64_t *j_start,	/*!< start index of j */
	const h5_int64_t *j_end,	/*!< end index of j */
	const h5_int64_t *k_start,	/*!< start index of k */
	const h5_int64_t *k_end	/*!< end index of k */
	) {

	h5_file_t *filehandle = (h5_file_t*)(size_t)*f;

	return H5Block3dSetView (
		filehandle,
		*i_start-1, *i_end-1,
		*j_start-1, *j_end-1,
		*k_start-1, *k_end-1 );
}

h5_err_t
h5bl_3d_setchunk (
	h5_int64_t *f,
	const h5_int64_t *i,
	const h5_int64_t *j,
	const h5_int64_t *k
	) {

	h5_file_t *filehandle = (h5_file_t*)(size_t)*f;

	return H5Block3dSetChunk ( filehandle, *i, *j, *k );
}

h5_err_t
h5bl_3d_getview (
	h5_int64_t *f,		/*!< file handle */
	const h5_int64_t *proc,
	h5_int64_t *i_start,	/*!< start index of i */
	h5_int64_t *i_end,		/*!< end index of i */
	h5_int64_t *j_start,	/*!< start index of j */
	h5_int64_t *j_end,		/*!< end index of j */
	h5_int64_t *k_start,	/*!< start index of k */
	h5_int64_t *k_end		/*!< end index of k */
	) {

	h5_file_t *filehandle = (h5_file_t*)(size_t)*f;

	h5_err_t herr = H5Block3dGetView (
		filehandle,
		*proc,
		i_start, i_end, j_start, j_end, k_start, k_end );
	if ( herr < 0 ) return herr;

	(*i_start)++;
	(*i_end)++;
	(*j_start)++;
	(*j_end)++;
	(*k_start)++;
	(*k_end)++;

	return H5_SUCCESS;
}

h5_err_t
h5bl_3d_getreducedview (
	h5_int64_t *f,
	const h5_int64_t *proc,
	h5_int64_t *i_start, 
	h5_int64_t *i_end,
	h5_int64_t *j_start,
	h5_int64_t *j_end,
	h5_int64_t *k_start,
	h5_int64_t *k_end
	) {

	h5_file_t *filehandle = (h5_file_t*)(size_t)*f;

	h5_err_t herr = H5Block3dGetReducedView (
		filehandle,
		*proc,
		i_start, i_end, j_start, j_end, k_start, k_end );
	if ( herr < 0 ) return herr;

	(*i_start)++;
	(*i_end)++;
	(*j_start)++;
	(*j_end)++;
	(*k_start)++;
	(*k_end)++;

	return H5_SUCCESS;
}

h5_err_t
h5bl_3d_getproc (
	h5_int64_t *f,
	const h5_int64_t *i,
	const h5_int64_t *j,
	const h5_int64_t *k
	) {

	h5_file_t *filehandle = (h5_file_t*)(size_t)*f;

	return H5Block3dGetProc ( filehandle, (*i)-1, (*j)-1, (*k)-1 );
}

h5_err_t
h5bl_getnumfields (
	h5_int64_t *f			/*!< file handle */
	) {

	h5_file_t *filehandle = (h5_file_t*)(size_t)*f;

	return H5BlockGetNumFields ( filehandle );
}

h5_err_t
h5bl_getfieldinfo (
	h5_int64_t *f,
	const h5_int64_t *idx,
	char *field_name,
	h5_size_t *grid_rank,
	h5_size_t *grid_dims,
	h5_size_t *field_dims,
	h5_int64_t *type,
	const int l_field_name
	) {

	h5_file_t *filehandle = (h5_file_t*)(size_t)*f;

	h5_err_t herr = H5BlockGetFieldInfo (
		filehandle, *idx, field_name, l_field_name,
		grid_rank, grid_dims, field_dims, type );
	h5_strc2for ( field_name, l_field_name );
	return herr;
}

h5_err_t
h5bl_writefieldattrib_string (
	h5_int64_t *f,
	const char *field_name,
	const char *attrib_name,
	const char *attrib_value,
	const int l_field_name,
	const int l_attrib_name,
	const int l_attrib_value
	) {

	h5_file_t *filehandle = (h5_file_t*)(size_t)*f;

	char *field_name2 = h5_strdupfor2c ( field_name, l_field_name );
	char *attrib_name2 = h5_strdupfor2c ( attrib_name, l_attrib_name );
	char *attrib_value2 = h5_strdupfor2c ( attrib_value, l_attrib_value );

	h5_err_t herr = H5BlockWriteFieldAttribString (
		filehandle, field_name2, attrib_name2, attrib_value2 );

	free ( field_name2 );
	free ( attrib_name2 );
	free ( attrib_value2 );
	return herr;
}


h5_err_t
h5bl_getnfieldattribs (
	h5_int64_t *f,
	const char *field_name,
	const int l_field_name
	) {

	h5_file_t *filehandle = (h5_file_t*)(size_t)*f;

	char *field_name2 = h5_strdupfor2c ( field_name, l_field_name );

	h5_err_t herr = H5BlockGetNumFieldAttribs (
		filehandle, field_name2 );
	
	free ( field_name2 );
	return herr;
}

h5_err_t
h5bl_getfieldattribinfo (
	h5_int64_t *f,
	const char *field_name,
	const h5_int64_t *attrib_idx,
	char *attrib_name,
	h5_size_t *attrib_nelem,
	const int l_field_name,
	const int l_attrib_name
	) {

	h5_file_t *filehandle = (h5_file_t*)(size_t)*f;

	h5_int64_t	attrib_type;

	char *field_name2 = h5_strdupfor2c ( field_name,   l_field_name );

	h5_err_t herr = H5BlockGetFieldAttribInfo (
		filehandle, field_name2, *attrib_idx,
		attrib_name, l_attrib_name,
		&attrib_type,
		attrib_nelem );

	h5_strc2for ( attrib_name, l_attrib_name );

	free ( field_name2 );
	return herr;
}


h5_err_t
h5bl_readfieldattrib_string (
	h5_int64_t *f,
	const char *field_name,
	const char *attrib_name,
	char *attrib_value,
	const int l_field_name,
	const int l_attrib_name,
	const int l_attrib_value
	) {

	h5_file_t *filehandle = (h5_file_t*)(size_t)*f;

	char *field_name2 = h5_strdupfor2c ( field_name, l_field_name );
	char *attrib_name2 = h5_strdupfor2c ( attrib_name, l_attrib_name );

	h5_err_t herr = H5BlockReadFieldAttrib (
		filehandle, field_name2, attrib_name2, attrib_value );

	h5_strc2for ( attrib_value, l_attrib_value );

	free ( field_name2 );
	free ( attrib_name2 );
	return herr;
}

