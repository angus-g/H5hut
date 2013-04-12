/*
  Copyright (c) 2006-2012, The Regents of the University of California,
  through Lawrence Berkeley National Laboratory (subject to receipt of any
  required approvals from the U.S. Dept. of Energy) and the Paul Scherrer
  Institut (Switzerland).  All rights reserved.

  License: see file COPYING in top level of source distribution.
*/

#include "h5_private.h"
#include "h5core/h5b_model.h"

#define h5bl_3d_setview F77_NAME (					\
                h5bl_3d_setview,                                        \
                h5bl_3d_setview_,                                       \
                H5BL_3D_SETVIEW )
h5_int64_t
h5bl_3d_setview (
	const h5_int64_t* const fh,
	const h5_int64_t* const i_start,	/*!< start index of i */
	const h5_int64_t* const i_end,  	/*!< end index of i */
	const h5_int64_t* const j_start,	/*!< start index of j */
	const h5_int64_t* const j_end,          /*!< end index of j */
	const h5_int64_t* const k_start,	/*!< start index of k */
	const h5_int64_t* const k_end		/*!< end index of k */
	) {
	h5_file_t f = h5_filehandlefor2c (fh);
	H5_API_ENTER (h5_int64_t,
		      "fh=%p, "
		      "i_start=%lld, i_end=%lld, "
		      "j_start=%lld, j_end=%lld, "
		       "k_start=%lld, k_end=%lld",
		      (h5_file_p)f,
		      *i_start, (long long)i_end,
		      (long long)j_start, (long long)j_end,
		      (long long)k_start, (long long)k_end);
	H5_API_RETURN(h5b_3d_set_view (
		f,
		*i_start-1, *i_end-1,
		*j_start-1, *j_end-1,
		*k_start-1, *k_end-1 ));
}

#define h5bl_3d_getview F77_NAME (					\
                h5bl_3d_getview,                                        \
                h5bl_3d_getview_,                                       \
                H5BL_3D_GETVIEW )
h5_int64_t
h5bl_3d_getview (
	const h5_int64_t* const fh,
	h5_int64_t* const i_start,
	h5_int64_t* const i_end,
	h5_int64_t* const j_start,
	h5_int64_t* const j_end,
	h5_int64_t* const k_start,
	h5_int64_t* const k_end
	) {
	h5_file_t f = h5_filehandlefor2c (fh);
	H5_API_ENTER (h5_int64_t,
		      "fh=%p, "
		      "i_start=%p, i_end=%p, "
		      "j_start=%p, j_end=%p, "
		      "k_start=%p, k_end=%p",
		      (h5_file_p)f,
		      i_start, i_end,
		      j_start, j_end,
		      k_start, k_end);
	TRY (h5b_3d_get_view (
                     f,
                     (h5_size_t*)i_start, (h5_size_t*)i_end,
                     (h5_size_t*)j_start, (h5_size_t*)j_end,
                     (h5_size_t*)k_start, (h5_size_t*)k_end ));
        *i_start += 1;
        *i_end +=   1;
        *j_start += 1;
        *j_end +=   1;
        *k_start += 1;
        *k_end +=   1;
        H5_API_RETURN (H5_SUCCESS);
}



#define h5bl_3d_getreducedview F77_NAME (                               \
                h5bl_3d_getreducedview,                                 \
                h5bl_3d_getreducedview_,                                \
                H5BL_3D_GETREDUCEDVIEW )
h5_int64_t
h5bl_3d_getreducedview (
	const h5_int64_t* const fh,
	h5_int64_t* const i_start, 
	h5_int64_t* const i_end,
	h5_int64_t* const j_start,
	h5_int64_t* const j_end,
	h5_int64_t* const k_start,
	h5_int64_t* const k_end
	) {
	h5_file_t f = h5_filehandlefor2c (fh);
	H5_API_ENTER (h5_int64_t,
		      "fh=%p, "
		      "i_start=%p, i_end=%p, "
		      "j_start=%p, j_end=%p, "
		      "k_start=%p, k_end=%p",
		      (h5_file_p)f,
		      i_start, i_end,
		      j_start, j_end,
		      k_start, k_end);
	TRY (h5b_3d_get_reduced_view (
                     f,
                     (h5_size_t*)i_start, (h5_size_t*)i_end,
                     (h5_size_t*)j_start, (h5_size_t*)j_end,
                     (h5_size_t*)k_start, (h5_size_t*)k_end));
        *i_start += 1;
        *i_end +=   1;
        *j_start += 1;
        *j_end +=   1;
        *k_start += 1;
        *k_end +=   1;
        H5_API_RETURN (H5_SUCCESS);
}

#define h5bl_3d_hasview F77_NAME (                              \
                h5bl_hasview,                                   \
                h5bl_hasview_,                                  \
                H5BL_HASVIEW )
h5_int64_t
h5bl_3d_hasview (
	const h5_int64_t* const fh
	) {
	h5_file_t f = h5_filehandlefor2c (fh);
	H5_API_ENTER (h5_int64_t,
                      "fh=%p",
                      (h5_file_p)f);
	H5_API_RETURN (h5b_3d_has_view ( f ));
}

#define h5bl_3d_setchunk F77_NAME (				\
                h5bl_3d_setchunk,                               \
                h5bl_3d_setchunk_,                              \
                H5BL_3D_SETCHUNK )
h5_int64_t
h5bl_3d_setchunk (
	const h5_int64_t* const fh,
	const h5_int64_t* const i,
	const h5_int64_t* const j,
	const h5_int64_t* const k
	) {
	h5_file_t f = h5_filehandlefor2c (fh);
	H5_API_ENTER (h5_int64_t,
		      "fh=%p, i=%lld, j=%lld, k=%lld",
		      (h5_file_p)f, (long long)i, (long long)j, (long long)k);
	H5_API_RETURN(h5b_3d_set_chunk ( f, *i, *j, *k ));
}

#define h5bl_getnumfields F77_NAME (					\
                h5bl_getnumfields,                                      \
                h5bl_getnumfields_,                                     \
                H5BL_GETNUMFIELDS )
h5_int64_t
h5bl_getnumfields (
	const h5_int64_t* const fh
	) {

	h5_file_t f = h5_filehandlefor2c (fh);
	H5_API_ENTER (h5_int64_t,
                      "fh=%p",
                      (h5_file_p)f);
	H5_API_RETURN (h5b_get_num_fields (f));
}

#define h5bl_getfieldinfo F77_NAME (					\
                h5bl_getfieldinfo,                                      \
                h5bl_getfieldinfo_,                                     \
                H5BL_GETFIELDINFO )
h5_int64_t
h5bl_getfieldinfo (
	const h5_int64_t* const fh,
	const h5_int64_t *idx,
	char* const name,
	h5_size_t* const field_rank,
	h5_size_t* const field_dims,
	h5_size_t* const elem_rank,
	h5_int64_t* const type,
	const int l_name
	) {
	h5_file_t f = h5_filehandlefor2c (fh);
	H5_API_ENTER (h5_int64_t,
		      "fh=%p, idx=%lld, "
		      "name=%*s,"
		      "field_rank=%p, field_dims=%p, elem_rank=%p, type=%p",
		      (h5_file_p)f, (long long)*idx, l_name, name,
		      field_rank, field_dims, elem_rank, type);
	h5_int64_t herr = h5b_get_field_info (
		f, *idx - 1, name, (h5_size_t)l_name,
		field_rank, field_dims, elem_rank, type );
	h5_strc2for ( name, l_name );
	H5_API_RETURN(herr);
}

