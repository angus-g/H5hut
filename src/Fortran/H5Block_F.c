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
	h5_int64_t *const f,
	const h5_int64_t *i_start,	/*!< start index of i */
	const h5_int64_t *i_end,	/*!< end index of i */
	const h5_int64_t *j_start,	/*!< start index of j */
	const h5_int64_t *j_end,	/*!< end index of j */
	const h5_int64_t *k_start,	/*!< start index of k */
	const h5_int64_t *k_end		/*!< end index of k */
	) {

	h5_file_t *fh = h5_filehandlefor2c(f);
	H5_API_ENTER (h5_err_t,
		      "f=%p, "
		      "i_start=%lld, i_end=%lld, "
		      "j_start=%lld, j_end=%lld, "
		       "k_start=%lld, k_end=%lld",
		      fh,
		      (long long)i_start, (long long)i_end,
		      (long long)j_start, (long long)j_end,
		      (long long)k_start, (long long)k_end);
	H5_API_RETURN(h5b_3d_set_view (
		fh,
		*i_start-1, *i_end-1,
		*j_start-1, *j_end-1,
		*k_start-1, *k_end-1 ));
}

h5_err_t
h5bl_3d_setchunk (
	h5_int64_t *const f,
	const h5_int64_t *i,
	const h5_int64_t *j,
	const h5_int64_t *k
	) {

	h5_file_t *fh = h5_filehandlefor2c(f);
	H5_API_ENTER (h5_err_t,
		      "f=%p, i=%lld, j=%lld, k=%lld",
		      fh, (long long)i, (long long)j, (long long)k);
	H5_API_RETURN(h5b_3d_set_chunk ( fh, *i, *j, *k ));
}

h5_err_t
h5bl_3d_getview (
	h5_int64_t *const f,
	h5_int64_t *const i_start,
	h5_int64_t *const i_end,
	h5_int64_t *const j_start,
	h5_int64_t *const j_end,
	h5_int64_t *const k_start,
	h5_int64_t *const k_end
	) {

	h5_file_t *fh = h5_filehandlefor2c(f);
	H5_API_ENTER (h5_err_t,
		      "f=%p, "
		      "i_start=%p, i_end=%p, "
		      "j_start=%p, j_end=%p, "
		      "k_start=%p, k_end=%p",
		      fh,
		      i_start, i_end,
		      j_start, j_end,
		      k_start, k_end);
	H5_API_RETURN (h5b_3d_get_view (
			       fh,
			       (h5_size_t*)i_start, (h5_size_t*)i_end,
			       (h5_size_t*)j_start, (h5_size_t*)j_end,
			       (h5_size_t*)k_start, (h5_size_t*)k_end ));
}

h5_err_t
h5bl_3d_getreducedview (
	h5_int64_t *const f,
	h5_int64_t *const i_start, 
	h5_int64_t *const i_end,
	h5_int64_t *const j_start,
	h5_int64_t *const j_end,
	h5_int64_t *const k_start,
	h5_int64_t *const k_end
	) {

	h5_file_t *fh = h5_filehandlefor2c(f);
	H5_API_ENTER (h5_err_t,
		      "f=%p, "
		      "i_start=%p, i_end=%p, "
		      "j_start=%p, j_end=%p, "
		      "k_start=%p, k_end=%p",
		      fh,
		      i_start, i_end,
		      j_start, j_end,
		      k_start, k_end);
	H5_API_RETURN(h5b_3d_get_reduced_view (
		fh,
		(h5_size_t*)i_start, (h5_size_t*)i_end,
		(h5_size_t*)j_start, (h5_size_t*)j_end,
		(h5_size_t*)k_start, (h5_size_t*)k_end ));
}

h5_int64_t
h5bl_3d_hasview (
	h5_int64_t *const f
	) {

	h5_file_t *fh = h5_filehandlefor2c(f);
	H5_API_ENTER (h5_int64_t, "f=%p", fh);
	H5_API_RETURN (h5b_3d_has_view ( fh ));
}

h5_err_t
h5bl_getnumfields (
	h5_int64_t *const f
	) {

	h5_file_t *fh = h5_filehandlefor2c(f);
	H5_API_ENTER (h5_int64_t, "f=%p", fh);
	H5_API_RETURN (h5b_get_num_fields ( fh ));
}

h5_err_t
h5bl_getfieldinfo (
	h5_int64_t *const f,
	const h5_int64_t *idx,
	char *name,
	h5_size_t *field_rank,
	h5_size_t *field_dims,
	h5_size_t *elem_rank,
	h5_int64_t *type,
	const int l_name
	) {

	h5_file_t *fh = h5_filehandlefor2c(f);
	H5_API_ENTER (h5_err_t,
		      "f=%p, idx=%lld, "
		      "name=%p,"
		      "field_rank=%p, field_dims=%p, elem_rank=%p, type=%p, l_name=%d",
		      fh, (long long)*idx, name,
		      field_rank, field_dims, elem_rank, type, l_name);
	h5_err_t herr = h5b_get_field_info (
		fh, *idx, name, (h5_size_t)l_name,
		field_rank, field_dims, elem_rank, type );
	h5_strc2for ( name, l_name );
	H5_API_RETURN(herr);
}

h5_err_t
h5bl_writefieldattrib_string (
	h5_int64_t *const f,
	const char *field_name,
	const char *attrib_name,
	const char *buffer,
	const int l_field_name,
	const int l_attrib_name,
	const int l_buffer
	) {

	h5_file_t *fh = h5_filehandlefor2c(f);
	char *field_name2 = h5_strdupfor2c ( field_name, l_field_name );
	char *attrib_name2 = h5_strdupfor2c ( attrib_name, l_attrib_name );
	char *buffer2 = h5_strdupfor2c ( buffer, l_buffer );
	H5_API_ENTER (h5_err_t,
		      "f=%p, "
		      "field_name='%s', "
		      "attrib_name='%s', "
		      "buffer='%s', "
		      "l_field_name=%d, "
		      "l_attrib_name=%d, "
		      "l_buffer=%d",
		      fh,
		      field_name2,
		      attrib_name2,
		      buffer2, l_field_name, l_attrib_name, l_buffer);
	h5_err_t herr = h5_write_field_attrib (
		fh, field_name2, attrib_name2,
		H5_STRING_T, buffer2, strlen(buffer2)+1 );

	free ( field_name2 );
	free ( attrib_name2 );
	free ( buffer2 );
	H5_API_RETURN(herr);
}


h5_err_t
h5bl_getnfieldattribs (
	h5_int64_t *const f,
	const char *name,
	const int l_name
	) {

	h5_file_t *fh = h5_filehandlefor2c(f);
	char *name2 = h5_strdupfor2c ( name, l_name );
	H5_API_ENTER (h5_err_t,
		      "f=%p, name='%s', l_name=%d", fh, name2, l_name);
	h5_err_t herr = h5b_get_num_field_attribs ( fh, name2 );
	free ( name2 );
	H5_API_RETURN(herr);
}

h5_err_t
h5bl_getfieldattribinfo (
	h5_int64_t *const f,
	const char *field_name,
	const h5_int64_t *attrib_idx,
	char *attrib_name,
	h5_size_t *attrib_nelem,
	const int l_field_name,
	const int l_attrib_name
	) {

	h5_file_t *fh = h5_filehandlefor2c(f);
	char *field_name2 = h5_strdupfor2c ( field_name, l_field_name );
	H5_API_ENTER (h5_err_t,
		      "f=%p field_name='%s', "
		      "attrib_idx=%lld, "
		      "attrib_name=%p, "
		      "attrib_nelem=%p, l_field_name=%d, l_attrib_name=%d",
		      fh,
		      field_name2,
		      (long long)*attrib_idx,
		      attrib_name, attrib_nelem, l_field_name, l_attrib_name);

	h5_int64_t attrib_type;
	h5_err_t herr = h5b_get_field_attrib_info (
		fh, field_name2, *attrib_idx,
		attrib_name, l_attrib_name,
		&attrib_type,
		attrib_nelem );

	h5_strc2for ( attrib_name, l_attrib_name );

	free ( field_name2 );
	H5_API_RETURN(herr);
}


h5_err_t
h5bl_readfieldattrib_string (
	h5_int64_t *const f,
	const char *field_name,
	const char *attrib_name,
	char *buffer,
	const int l_field_name,
	const int l_attrib_name,
	const int l_buffer
	) {

	h5_file_t *fh = h5_filehandlefor2c(f);
	char *field_name2 = h5_strdupfor2c ( field_name, l_field_name );
	char *attrib_name2 = h5_strdupfor2c ( attrib_name, l_attrib_name );
	H5_API_ENTER (h5_err_t,
		      "f=%p, "
		      "field_name='%s', "
		      "attrib_name='%s', "
		      "buffer=%p, l_field_name=%d, l_attrib_name=%d, l_buffer=%d",
		      fh,
		      field_name,
		      attrib_name,
		      buffer, l_field_name, l_attrib_name, l_buffer);

	h5_err_t herr = h5_read_field_attrib (
		fh, field_name2, attrib_name2,
		H5_STRING_T, buffer );

	h5_strc2for ( buffer, l_buffer );

	free ( field_name2 );
	free ( attrib_name2 );

	H5_API_RETURN(herr);
}

