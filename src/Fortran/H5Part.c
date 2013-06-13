/*
  Copyright (c) 2006-2012, The Regents of the University of California,
  through Lawrence Berkeley National Laboratory (subject to receipt of any
  required approvals from the U.S. Dept. of Energy) and the Paul Scherrer
  Institut (Switzerland).  All rights reserved.

  License: see file COPYING in top level of source distribution.
*/

#include "h5_private.h"
#include "h5core/h5u_model.h"

/*==============Reading Data Characteristics============*/

#define h5pt_getndatasets F77_NAME (					\
                h5pt_getndatasets,                                      \
                h5pt_getndatasets_,                                     \
                H5PT_GETNDATASETS )
h5_int64_t
h5pt_getndatasets (
	const h5_int64_t* const fh
	) {
	h5_file_t* f = h5_filehandlefor2c (fh);
	H5_API_ENTER (h5_int64_t, "fh=%p", (h5_file_p)f);
	H5_API_RETURN(h5u_get_num_datasets (f));
}

#define h5pt_getnpoints F77_NAME (					\
                h5pt_getnpoints,                                        \
                h5pt_getnpoints_,                                       \
                H5PT_GETNPOINTS )
h5_int64_t
h5pt_getnpoints (
	const h5_int64_t* const fh
	) {
	h5_file_t* f = h5_filehandlefor2c (fh);
	H5_API_ENTER (h5_int64_t, "fh=%p", (h5_file_p)f);
	H5_API_RETURN (h5u_get_num_particles (f));
}

#define h5pt_getdatasetname F77_NAME (					\
                h5pt_getdatasetname,                                    \
                h5pt_getdatasetname_,                                   \
                H5PT_GETDATASETNAME )
h5_int64_t
h5pt_getdatasetname (
	const h5_int64_t* const fh,
	const h5_int64_t* index,
	char* name,
	const int l_name
	) {
	h5_file_t* f = h5_filehandlefor2c (fh);
	H5_API_ENTER (h5_int64_t,
                      "fh=%p, index=%lld, name='%s', l_name=%d",
                      (h5_file_p)f, (long long)*index, name, l_name);
	h5_int64_t herr =  h5u_get_dataset_info (
		f, *index - 1, name, l_name, NULL, NULL );
	h5_strc2for (name, l_name);
	H5_API_RETURN (herr);
}

#define h5pt_getdatasetinfo F77_NAME(            \
                h5pt_getdatasetinfo,             \
                h5pt_getdatasetinfo_,            \
                H5PT_GETDATASETINFO)
h5_int64_t
h5pt_getdatasetinfo (
	const h5_int64_t* const fh,
	const h5_int64_t* dataset_idx,
	char* dataset_name,
        h5_int64_t* dataset_type,
	h5_int64_t* dataset_nelem,
	const int l_dataset_name
        ) {
	h5_file_t* f = h5_filehandlefor2c (fh);
	H5_API_ENTER (h5_int64_t,
		      "fh=%p, "
		      "dataset_idx=%lld, "
		      "dataset_name=%p, "
		      "dataset_type=%p, "
		      "dataset_nelem=%p",
		      (h5_file_p)f,
		      (long long)*dataset_idx,
		      dataset_name, dataset_type, dataset_nelem);
        h5_int64_t h5err = h5u_get_dataset_info (
                f,
                *dataset_idx - 1,
                dataset_name, l_dataset_name,
                dataset_type,
                (h5_size_t*)dataset_nelem);
	h5_strc2for (dataset_name, l_dataset_name);
        convert_type2for (dataset_type);
	H5_API_RETURN (h5err);
}

/*=============Setting and getting views================*/

#define h5pt_setview F77_NAME (						\
                h5pt_setview,                                           \
                h5pt_setview_,                                          \
                H5PT_SETVIEW )
h5_int64_t
h5pt_setview (
	const h5_int64_t* const fh,
	const h5_int64_t* const start,
	const h5_int64_t* const end
	) {
	h5_file_t* f = h5_filehandlefor2c (fh);
	H5_API_ENTER (h5_int64_t,
                      "fh=%p, start=%lld, end=%lld",
                      (h5_file_p)f, (long long)*start, (long long)*end);
	H5_API_RETURN (h5u_set_view (f, (*start)-1, (*end)-1));
}

#define h5pt_setview_indices F77_NAME (					\
                h5pt_setview_indices,                                   \
                h5pt_setview_indices_,                                  \
                H5PT_SETVIEW_INDICES )
h5_int64_t
h5pt_setview_indices (
	const h5_int64_t* const fh,
	const h5_int64_t* const indices,
	const h5_int64_t* const nelem
	) {
	h5_file_t* f = h5_filehandlefor2c (fh);
	H5_API_ENTER (h5_int64_t,
                      "fh=%p, indices=%p, nelem=%lld",
                      (h5_file_p)f, indices, (long long)*nelem);
        h5_size_t* findices;
        TRY (findices = h5_calloc (*nelem, sizeof (*indices)));
        for (size_t i = 0; i < *nelem; i++) 
                findices[i] = indices[i] - 1;
        TRY (h5u_set_view_indices (f, findices, *nelem));
        TRY (h5_free (findices));
	H5_API_RETURN (H5_SUCCESS);
}

#define h5pt_setnpoints F77_NAME (					\
                h5pt_setnpoints,                                        \
                h5pt_setnpoints_,                                       \
                H5PT_SETNPOINTS )
h5_int64_t
h5pt_setnpoints (
	const h5_int64_t* const fh,
	const h5_int64_t* const npoints
	) {
	h5_file_t* f = h5_filehandlefor2c (fh);
	H5_API_ENTER (h5_int64_t,
                      "fh=%p, npoints=%lld",
                      (h5_file_p)f, (long long)*npoints);
	H5_API_RETURN (h5u_set_num_particles (f, *npoints, 1));
}
                
#define h5pt_setnpoints_strided F77_NAME (                              \
                        h5pt_setnpoints_strided,                        \
                        h5pt_setnpoints_strided_,                       \
                        H5PT_SETNPOINTS_STRIDED )
h5_int64_t
h5pt_setnpoints_strided (
	const h5_int64_t* const fh,
	const h5_int64_t* const npoints,
        const h5_int64_t* const stride
	) {
	h5_file_t* f = h5_filehandlefor2c (fh);
	H5_API_ENTER (h5_int64_t,
                      "fh=%p, npoints=%lld, stride=%lld",
                      (h5_file_p)f, (long long)*npoints, (long long)*stride);
	H5_API_RETURN (h5u_set_num_particles (f, *npoints, *stride));
}

#define h5pt_resetview F77_NAME (					\
                h5pt_resetview,                                         \
                h5pt_resetview_,                                        \
                H5PT_RESETVIEW )
h5_int64_t
h5pt_resetview (
	const h5_int64_t* const fh
	) {
	h5_file_t* f = h5_filehandlefor2c (fh);
	H5_API_ENTER (h5_int64_t,
                      "fh=%p",
                      (h5_file_p)f);
	H5_API_RETURN (h5u_reset_view (f));
}

#define h5pt_hasview F77_NAME (						\
                h5pt_hasview,                                           \
                h5pt_hasview_,                                          \
                H5PT_HASVIEW )
h5_int64_t
h5pt_hasview (
	const h5_int64_t* const fh
	) {
	h5_file_t* f = h5_filehandlefor2c (fh);
	H5_API_ENTER (h5_int64_t,
                      "fh=%p",
                      (h5_file_p)f);
	H5_API_RETURN (h5u_has_view (f));
}

#define h5pt_getview F77_NAME (						\
                h5pt_getview,                                           \
                h5pt_getview_,                                          \
                H5PT_GETVIEW )
h5_int64_t
h5pt_getview (
	const h5_int64_t* const fh,
	h5_int64_t* const start,
	h5_int64_t* const end
	) {
	h5_file_t* f = h5_filehandlefor2c (fh);
	H5_API_ENTER (h5_int64_t,
                      "fh=%p, start=%p, end=%p",
                      (h5_file_p)f, start, end);
        TRY (h5u_get_view (f, start, end));
        *start += 1;
        *end += 1;
        H5_API_RETURN (H5_SUCCESS);
}
