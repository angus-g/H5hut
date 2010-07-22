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
#define h5bl_3d_hasview F77NAME (					\
					h5bl_hasview_,		\
					H5BL_HASVIEW )
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
	h5_set_funcname( filehandle, __func__ );
	return h5b_3d_set_view (
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
	h5_set_funcname( filehandle, __func__ );
	return h5b_3d_set_chunk ( filehandle, *i, *j, *k );
}

h5_err_t
h5bl_3d_getview (
	h5_int64_t *f,
	h5_int64_t *i_start,
	h5_int64_t *i_end,
	h5_int64_t *j_start,
	h5_int64_t *j_end,
	h5_int64_t *k_start,
	h5_int64_t *k_end
	) {

	h5_file_t *filehandle = (h5_file_t*)(size_t)*f;
	h5_set_funcname( filehandle, __func__ );
        h5_size_t view[6];

	h5_err_t herr = h5b_3d_get_view (
		filehandle,
                view+0, view+1, view+2, view+3, view+4, view+5 );
	if ( herr < 0 ) return herr;

	*i_start = view[0]+1;
	*i_end   = view[1]+1;
	*j_start = view[2]+1;
	*j_end   = view[3]+1;
	*k_start = view[4]+1;
	*k_end   = view[5]+1;

	return H5_SUCCESS;
}

h5_err_t
h5bl_3d_getreducedview (
	h5_int64_t *f,
	h5_int64_t *i_start, 
	h5_int64_t *i_end,
	h5_int64_t *j_start,
	h5_int64_t *j_end,
	h5_int64_t *k_start,
	h5_int64_t *k_end
	) {

	h5_file_t *filehandle = (h5_file_t*)(size_t)*f;
	h5_set_funcname( filehandle, __func__ );
        h5_size_t view[6];

	h5_err_t herr = h5b_3d_get_reduced_view (
		filehandle,
                view+0, view+1, view+2, view+3, view+4, view+5 );
	if ( herr < 0 ) return herr;

	*i_start = view[0]+1;
	*i_end   = view[1]+1;
	*j_start = view[2]+1;
	*j_end   = view[3]+1;
	*k_start = view[4]+1;
	*k_end   = view[5]+1;

	return H5_SUCCESS;
}

h5_int64_t
h5bl_3d_hasview (
	h5_int64_t *f
	) {

	h5_file_t *filehandle = (h5_file_t*)(size_t)*f;
	h5_set_funcname( filehandle, __func__ );
	return h5b_3d_has_view ( filehandle );
}

h5_err_t
h5bl_getnumfields (
	h5_int64_t *f			/*!< file handle */
	) {

	h5_file_t *filehandle = (h5_file_t*)(size_t)*f;
	h5_set_funcname( filehandle, __func__ );
	return h5b_get_num_fields ( filehandle );
}

h5_err_t
h5bl_getfieldinfo (
	h5_int64_t *f,
	const h5_int64_t *idx,
	char *field_name,
	h5_size_t *field_rank,
	h5_size_t *field_dims,
	h5_size_t *elem_rank,
	h5_int64_t *type,
	const int l_field_name
	) {

	h5_file_t *filehandle = (h5_file_t*)(size_t)*f;
	h5_set_funcname( filehandle, __func__ );
	h5_err_t herr = h5b_get_field_info (
		filehandle, *idx, field_name, l_field_name,
		field_rank, field_dims, elem_rank, type );
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
	h5_set_funcname( filehandle, __func__ );

	char *field_name2 = h5_strdupfor2c ( field_name, l_field_name );
	char *attrib_name2 = h5_strdupfor2c ( attrib_name, l_attrib_name );
	char *attrib_value2 = h5_strdupfor2c ( attrib_value, l_attrib_value );

	h5_err_t herr = h5_write_field_attrib (
		filehandle, field_name2, attrib_name2,
		H5T_NATIVE_CHAR, attrib_value2, strlen(attrib_value2)+1 );

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
	h5_set_funcname( filehandle, __func__ );

	char *field_name2 = h5_strdupfor2c ( field_name, l_field_name );

	h5_err_t herr = h5b_get_num_field_attribs (
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
	h5_set_funcname( filehandle, __func__ );

	h5_int64_t	attrib_type;

	char *field_name2 = h5_strdupfor2c ( field_name,   l_field_name );

	h5_err_t herr = h5b_get_field_attrib_info (
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
	h5_set_funcname( filehandle, __func__ );

	char *field_name2 = h5_strdupfor2c ( field_name, l_field_name );
	char *attrib_name2 = h5_strdupfor2c ( attrib_name, l_attrib_name );

	h5_err_t herr = h5_read_field_attrib (
		filehandle, field_name2, attrib_name2,
		H5_STRING_T, attrib_value );

	h5_strc2for ( attrib_value, l_attrib_value );

	free ( field_name2 );
	free ( attrib_name2 );
	return herr;
}

