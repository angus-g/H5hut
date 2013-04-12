/*
  Copyright (c) 2006-2012, The Regents of the University of California,
  through Lawrence Berkeley National Laboratory (subject to receipt of any
  required approvals from the U.S. Dept. of Energy) and the Paul Scherrer
  Institut (Switzerland).  All rights reserved.

  License: see file COPYING in top level of source distribution.
*/
#include "h5core/h5.h"

#include "h5_debug_private.h"
#include "h5_va_macros.h"

#include "h5_private.h"
#include "h5_hdf5_private.h"

#include "h5_attribs_private.h"
#include "h5_readwrite_private.h"
#include "h5b_types_private.h"
#include "h5b_model_private.h"
h5_err_t
h5b_write_field_attrib (
	const h5_file_t fh,			/*!< IN: file handle */
	const char *field_name,			/*!< IN: field name */
	const char *attrib_name,		/*!< IN: attribute name */
	const hid_t attrib_type,		/*!< IN: attribute type */
	const void *attrib_value,		/*!< IN: attribute value */
	const h5_int64_t attrib_nelem		/*!< IN: number of elements */
	) {
        h5_file_p f = (h5_file_p)fh;
	H5_CORE_API_ENTER (h5_err_t,
	                   "f=%p, field_name='%s', "
	                   "attrib_name='%s', attrib_type=%d, "
	                   "attrib_value=%p, attrib_nelem=%lld",
	                   f,
	                   field_name,
	                   attrib_name,
	                   attrib_type,
	                   attrib_value,
	                   (long long)attrib_nelem);
	CHECK_FILEHANDLE (f);
	CHECK_WRITABLE_MODE (f);
	CHECK_TIMEGROUP (f);

	TRY( h5bpriv_create_field_group(f, field_name) );

	TRY( h5priv_write_attrib (
		f->b->field_gid,
		attrib_name,
		attrib_type,
		attrib_value,
		attrib_nelem,
		1) );

	H5_CORE_API_RETURN (H5_SUCCESS);
}

h5_err_t
h5b_read_field_attrib (
	const h5_file_t fh,			/*!< IN: file handle */
	const char *field_name,			/*!< IN: field name */
	const char *attrib_name,		/*!< IN: attribute name */
	const h5_int64_t attrib_type,		/*!< IN: attribute type */
	void *buffer		                /*!< OUT: attribute value */
	) {
        h5_file_p f = (h5_file_p)fh;
	H5_CORE_API_ENTER (h5_err_t,
	                   "f=%p, field_name='%s', "
	                   "attrib_name='%s', attrib_type=%lld, "
	                   "attrib_value=%p",
	                   f,
	                   field_name,
	                   attrib_name,
	                   (long long)attrib_type,
	                   buffer);
	CHECK_FILEHANDLE (f);
	CHECK_TIMEGROUP (f);

	TRY( h5bpriv_open_field_group(f, field_name) );

	TRY( h5priv_read_attrib (
		f->b->field_gid,
		attrib_name,
		attrib_type,
		buffer) );

	H5_CORE_API_RETURN (H5_SUCCESS);
}

h5_ssize_t
h5b_get_num_field_attribs (
	const h5_file_t fh,			/*<! IN: file handle */
	const char *field_name			/*<! IN: field name */
	) {
        h5_file_p f = (h5_file_p)fh;
	H5_CORE_API_ENTER (h5_err_t, "f=%p field_name='%s'", f, field_name);
	CHECK_FILEHANDLE (f);
	CHECK_TIMEGROUP (f);

	TRY (h5bpriv_open_field_group(f, field_name));

	H5_CORE_API_RETURN (hdf5_get_num_attribute (f->b->field_gid));
}

h5_err_t
h5b_get_field_attrib_info (
	const h5_file_t fh,			/*!< IN: file handle */
	const char *field_name,			/*!< IN: field name */
	const h5_size_t attrib_idx,		/*!< IN: attribute index */
	char *attrib_name,			/*!< OUT: attribute name */
	const h5_size_t len_attrib_name,	/*!< IN: buffer size */
	h5_int64_t *attrib_type,		/*!< OUT: attribute type */
	h5_size_t *attrib_nelem			/*!< OUT: number of elements */
	) {
        h5_file_p f = (h5_file_p)fh;
	H5_CORE_API_ENTER (h5_err_t,
	                   "f=%p, "
	                   "field_name='%s', "
	                   "attrib_idx=%llu, "
	                   "attrib_name=%p, len_attrib_name=%llu, "
	                   "attrib_type=%p, "
	                   "attrib_nelem=%p",
	                   f,
	                   field_name,
	                   (long long unsigned)attrib_idx,
	                   attrib_name, (long long unsigned)len_attrib_name,
	                   attrib_type,
	                   attrib_nelem);
	CHECK_FILEHANDLE (f);
	CHECK_TIMEGROUP (f);

	TRY (h5bpriv_open_field_group(f, field_name));

	H5_CORE_API_RETURN (
		h5priv_get_attrib_info (
			f->b->field_gid,
			attrib_idx,
			attrib_name,
			len_attrib_name,
			attrib_type,
			attrib_nelem));
}

static inline h5_err_t
check_coords (
        const h5_file_p f,
        int rank,
	const h5_int64_t n_coords
        ) {
	h5b_fdata_t *b = f->b;
        switch (rank) {
        case 0: {
                if (n_coords != b->i_max + 1)
                        h5_warn ("Coordinate array length (%lld) "
                                 "does not match X dimension (%lld)",
                                 (long long)n_coords,
                                 (long long)b->i_max + 1);
                break;
        }

        case 1:
                if (n_coords != b->j_max + 1)
                        h5_warn ("Coordinate array length (%lld) "
                                 "does not match Y dimension (%lld)",
                                 (long long)n_coords,
                                 (long long)b->j_max + 1);
                break;
        case 2:
                if (n_coords != b->k_max + 1)
                        h5_warn ("Coordinate array length (%lld) "
                                 "does not match Z dimension (%lld)",
                                 (long long)n_coords,
                                 (long long)b->k_max + 1);
                break;
        default:
		return h5_error_internal ();
        }
        return H5_SUCCESS;
}

h5_err_t
h5b_set_3d_field_coords (
        const h5_file_t fh,
        int rank,
	const char* field_name,
        const char* attrib_name,
	const h5_float64_t* coords,
	const h5_int64_t n_coords
	) {
        h5_file_p f = (h5_file_p)fh;
	H5_CORE_API_ENTER (h5_err_t,
			   "f=%p, "
                           "rank=%d, "
			   "field_name='%s', "
			   "attrib_name='%s', "
			   "coords=%p, n_coords=%llu",
			   f,
                           rank,
			   field_name,
                           attrib_name,
			   coords, (long long unsigned)n_coords);
	CHECK_FILEHANDLE (f);
	CHECK_TIMEGROUP (f);

        TRY (check_coords (f, rank, n_coords));
	TRY (h5b_write_field_attrib (
                     (h5_file_t)f,
                     field_name,
                     attrib_name,
                     H5_FLOAT64_T,
                     coords,
                     n_coords));

        H5_CORE_API_RETURN (H5_SUCCESS);
}

h5_err_t
h5b_get_3d_field_coords (
        const h5_file_t fh,
        int rank,
	const char* const field_name,
        const char* const attrib_name,
	h5_float64_t* const coords,
	const h5_int64_t n_coords
	) {
        h5_file_p f = (h5_file_p)fh;
	H5_CORE_API_ENTER (h5_err_t,
			   "f=%p, "
                           "rank=%d, "
			   "field_name='%s', "
			   "attrib_name='%s', "
			   "coords=%p, n_coords=%llu",
			   f,
                           rank,
			   field_name,
                           attrib_name,
			   coords, (long long unsigned)n_coords);
	CHECK_FILEHANDLE (f);
	CHECK_TIMEGROUP (f);

        TRY (check_coords (f, rank, n_coords));
	TRY (h5b_read_field_attrib (
                     (h5_file_t)f,
                     field_name,
                     attrib_name,
                     H5_FLOAT64_T,
                     coords));

        H5_CORE_API_RETURN (H5_SUCCESS);
}


