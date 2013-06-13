/*
  Copyright (c) 2006-2012, The Regents of the University of California,
  through Lawrence Berkeley National Laboratory (subject to receipt of any
  required approvals from the U.S. Dept. of Energy) and the Paul Scherrer
  Institut (Switzerland).  All rights reserved.

  License: see file COPYING in top level of source distribution.
*/

#include "h5_private.h"
#include "h5core/h5b_attribs.h"

#define H5_R8_T H5_FLOAT64_T
#define H5_R4_T H5_FLOAT32_T
#define H5_I8_T H5_INT64_T
#define H5_I4_T H5_INT32_T

/*
   __ _ _   _  ___ _ __ _   _ 
  / _` | | | |/ _ \ '__| | | |
 | (_| | |_| |  __/ |  | |_| |
  \__, |\__,_|\___|_|   \__, |
     |_|                |___/
*/

#define h5bl_getnfieldattribs F77_NAME (				\
                h5bl_getnfieldattribs,                                  \
                h5bl_getnfieldattribs_,                                 \
                H5BL_GETNFIELDATTRIBS)
h5_int64_t
h5bl_getnfieldattribs (
	const h5_int64_t* const fh,
	const char* const name,
	const int l_name
	) {
	h5_file_t* f = h5_filehandlefor2c (fh);
	H5_API_ENTER (h5_int64_t,
		      "fh=%p, name='%*s'",
                      (h5_file_p)f, l_name, name);
	char* name2 = h5_strdupfor2c ( name, l_name );
	h5_int64_t herr = h5b_get_num_field_attribs (f, name2);
	free (name2);
	H5_API_RETURN (herr);
}

#define h5bl_getfieldattribinfo F77_NAME (				\
                h5bl_getfieldattribinfo,                                \
                h5bl_getfieldattribinfo_,                               \
                h5bl_getfieldattribinfo)
h5_int64_t
h5bl_getfieldattribinfo (
	const h5_int64_t* const fh,
	const char* const field_name,
	const h5_int64_t* const attrib_idx,
	char* const attrib_name,
	h5_int64_t* const attrib_nelem,
	const int l_field_name,
	const int l_attrib_name
	) {
	h5_file_t* f = h5_filehandlefor2c (fh);
	H5_API_ENTER (h5_err_t,
		      "fh=%p, field_name='%*s', attrib_idx=%lld, "
                      "attrib_name=%p, attrib_nelem=%p",
		      (h5_file_p)f,
		      l_field_name, field_name,
		      (long long)*attrib_idx, 
		      attrib_name, attrib_nelem);

	char *field_name2 = h5_strdupfor2c ( field_name, l_field_name );
	h5_int64_t attrib_type;
	h5_int64_t herr = h5b_get_field_attrib_info (
                f,
                field_name2, *attrib_idx - 1,
		attrib_name, l_attrib_name,
		&attrib_type,
		(h5_size_t*)attrib_nelem );

	h5_strc2for ( attrib_name, l_attrib_name );

	free (field_name2);
	H5_API_RETURN (herr);
}

/*
  _    __    
 (_)  / /__  
 | | / / _ \ 
 | |/ / (_) |
 |_/_/ \___/ 
*/
static inline h5_int64_t
write_field_attrib (
        h5_file_t* const fh,
	const char* field_name,
	const int l_field_name,
	const char* attrib_name,
	const int l_attrib_name,
        const h5_int64_t attrib_type,
	const void* attrib_value,
	const hsize_t attrib_nelems
        ) {
	char *field_name2 = h5_strdupfor2c (field_name, l_field_name);
	char *attrib_name2 = h5_strdupfor2c (attrib_name, l_attrib_name);
	h5_int64_t h5err = h5_write_field_attrib (
                fh, field_name2,
                attrib_name2, attrib_type,
                attrib_value, attrib_nelems);
	free (field_name2);
	free (attrib_name2);
        return h5err;
}

static inline h5_int64_t
read_field_attrib (
        h5_file_t* const fh,
	const char* field_name,
	const int l_field_name,
	const char* attrib_name,
	const int l_attrib_name,
        const hid_t attrib_type,
	void* attrib_value
	) {
	char *field_name2 = h5_strdupfor2c (field_name, l_field_name);
	char *attrib_name2 = h5_strdupfor2c (attrib_name, l_attrib_name);
	h5_int64_t h5err = h5_read_field_attrib (
                fh, field_name2,
                attrib_name2, attrib_type, attrib_value);
	free (field_name2);
	free (attrib_name2);
	return h5err;
}

/*
      _        _             
  ___| |_ _ __(_)_ __   __ _ 
 / __| __| '__| | '_ \ / _` |
 \__ \ |_| |  | | | | | (_| |
 |___/\__|_|  |_|_| |_|\__, |
                       |___/ 
*/
#define h5bl_writefieldattrib_string F77_NAME (                         \
                h5bl_writefieldattrib_string,                           \
                h5bl_writefieldattrib_string_,                          \
                H5BL_WRITEFIELDATTRIB_STRING)
h5_int64_t
h5bl_writefieldattrib_string (
	const h5_int64_t* const fh,
	const char* const field_name,
	const char* const attrib_name,
	const char* const attrib_value,
	const int l_field_name,
	const int l_attrib_name,
	const int l_attrib_value
	) {
	h5_file_t* f = h5_filehandlefor2c (fh);
	H5_API_ENTER (h5_int64_t,
		      "fh=%p, field_name='%.*s', "
                      "attrib_name='%.*s' attrib_value='%.*s'",
		      (h5_file_p)f,
                      l_field_name, field_name,
                      l_attrib_name, attrib_name,
                      l_attrib_value, attrib_value);
	char* attrib_value2 = h5_strdupfor2c (attrib_value, l_attrib_value);
	h5_int64_t h5err = write_field_attrib (
                f,
                field_name, l_field_name,
                attrib_name, l_attrib_name,
                H5_STRING_T,
                attrib_value2, strlen(attrib_value2)+1 );
	free (attrib_value2);
	H5_API_RETURN (h5err);
}

#define h5bl_readfieldattrib_string F77_NAME (				\
                h5bl_readfieldattrib_string,                            \
                h5bl_readfieldattrib_string_,                           \
                H5BL_READFIELDATTRIB_STRING)
h5_err_t
h5bl_readfieldattrib_string (
	const h5_int64_t* const fh,
	const char* const field_name,
	const char* const attrib_name,
	char* const attrib_value,
	const int l_field_name,
	const int l_attrib_name,
	const int l_attrib_value
	) {
	h5_file_t* f = h5_filehandlefor2c (fh);
	H5_API_ENTER (h5_int64_t,
		      "f=%p, field_name='%.*s', attrib_name='%.*s' attrib_value='%p'",
		      (h5_file_p)f,
                      l_field_name, field_name,
                      l_attrib_name, attrib_name,
                      attrib_value);
        h5_int64_t h5err = read_field_attrib (
                f,
		field_name, l_field_name,
                attrib_name, l_attrib_name,
		H5_STRING_T, attrib_value);

	h5_strc2for (attrib_value, l_attrib_value);
	H5_API_RETURN (h5err);
}

/*
                 _ 
  _ __ ___  __ _| |
 | '__/ _ \/ _` | |
 | | |  __/ (_| | |
 |_|  \___|\__,_|_|
*/
#define h5bl_writefieldattrib_r8 F77_NAME (                         \
                h5bl_writefieldattrib_r8,                           \
                h5bl_writefieldattrib_r8_,                          \
                H5BL_WRITEFIELDATTRIB_R8)
h5_int64_t
h5bl_writefieldattrib_r8 (
	const h5_int64_t* const fh,
	const char* const field_name,
	const char* const attrib_name,
	const h5_float64_t* const attrib_value,
	const h5_int64_t* const attrib_nelems,
	const int l_field_name,
	const int l_attrib_name
	) {
	h5_file_t* f = h5_filehandlefor2c (fh);
	H5_API_ENTER (h5_int64_t,
		      "fh=%p, field_name='%.*s', attrib_name='%.*s', "
                      "attrib_value=%p, attrib_nelems=%lld",
		      (h5_file_p)f,
                      l_field_name, field_name,
                      l_attrib_name, attrib_name,
                      attrib_value, *attrib_nelems);
	H5_API_RETURN (write_field_attrib (
                               f,
                               field_name, l_field_name,
                               attrib_name, l_attrib_name,
                               H5_R8_T,
                               attrib_value, *attrib_nelems));
}

#define h5bl_readfieldattrib_r8 F77_NAME (                          \
                h5bl_readfieldattrib_r8,                            \
                h5bl_readfieldattrib_r8_,                           \
                H5BL_READFIELDATTRIB_R8)
h5_err_t
h5bl_readfieldattrib_r8 (
	const h5_int64_t* const fh,
	const char* const field_name,
	const char* const  attrib_name,
	h5_float64_t* const attrib_value,
	const int l_field_name,
	const int l_attrib_name
	) {
	h5_file_t* f = h5_filehandlefor2c (fh);
	H5_API_ENTER (h5_int64_t,
		      "fh=%p, field_name='%.*s', attrib_name='%.*s', "
                      "attrib_value=%p",
		      (h5_file_p)f,
                      l_field_name, field_name,
                      l_attrib_name, attrib_name,
                      attrib_value);
        H5_API_RETURN (read_field_attrib (
                               f,
                               field_name, l_field_name,
                               attrib_name, l_attrib_name,
                               H5_R8_T,
                               attrib_value));
}

#define h5bl_writefieldattrib_r4 F77_NAME (                         \
                h5bl_writefieldattrib_r4,                           \
                h5bl_writefieldattrib_r4_,                          \
                H5BL_WRITEFIELDATTRIB_R4)
h5_int64_t
h5bl_writefieldattrib_r4 (
	const h5_int64_t* const fh,
	const char* const field_name,
	const char* const attrib_name,
	const h5_float32_t* const attrib_value,
	const h5_int64_t* const attrib_nelems,
	const int l_field_name,
	const int l_attrib_name
	) {
	h5_file_t* f = h5_filehandlefor2c (fh);
	H5_API_ENTER (h5_int64_t,
		      "fh=%p, field_name='%.*s', attrib_name='%.*s', "
                      "attrib_value=%p, attrib_nelems=%lld",
		      (h5_file_p)f,
                      l_field_name, field_name,
                      l_attrib_name, attrib_name,
                      attrib_value, *attrib_nelems);
	H5_API_RETURN (write_field_attrib (
                               f,
                               field_name, l_field_name,
                               attrib_name, l_attrib_name,
                               H5_R4_T,
                               attrib_value, *attrib_nelems));
}

#define h5bl_readfieldattrib_r4 F77_NAME (                          \
                h5bl_readfieldattrib_r4,                            \
                h5bl_readfieldattrib_r4_,                           \
                H5BL_READFIELDATTRIB_R4)
h5_err_t
h5bl_readfieldattrib_r4 (
	const h5_int64_t* const fh,
	const char* const field_name,
	const char* const attrib_name,
	h5_float32_t* const attrib_value,
	const int l_field_name,
	const int l_attrib_name
	) {
	h5_file_t* f = h5_filehandlefor2c (fh);
	H5_API_ENTER (h5_int64_t,
		      "fh=%p, field_name='%.*s', attrib_name='%.*s', "
                      "attrib_value=%p",
		      (h5_file_p)f,
                      l_field_name, field_name,
                      l_attrib_name, attrib_name,
                      attrib_value);
        H5_API_RETURN (read_field_attrib (
                               f,
                               field_name, l_field_name,
                               attrib_name, l_attrib_name,
                               H5_R4_T,
                               attrib_value));
}

/*
  _       _                       
 (_)_ __ | |_ ___  __ _  ___ _ __ 
 | | '_ \| __/ _ \/ _` |/ _ \ '__|
 | | | | | ||  __/ (_| |  __/ |   
 |_|_| |_|\__\___|\__, |\___|_|   
                  |___/
*/
#define h5bl_writefieldattrib_i8 F77_NAME (                         \
                h5bl_writefieldattrib_i8,                           \
                h5bl_writefieldattrib_i8_,                          \
                H5BL_WRITEFIELDATTRIB_I8)
h5_int64_t
h5bl_writefieldattrib_i8 (
	const h5_int64_t* const fh,
	const char* const field_name,
	const char* const attrib_name,
	const h5_int64_t* const attrib_value,
	const h5_int64_t* const attrib_nelems,
	const int l_field_name,
	const int l_attrib_name
	) {
	h5_file_t* f = h5_filehandlefor2c (fh);
	H5_API_ENTER (h5_int64_t,
		      "fh=%p, field_name='%.*s', attrib_name='%.*s', "
                      "attrib_value=%p, attrib_nelems=%lld",
		      (h5_file_p)f,
                      l_field_name, field_name,
                      l_attrib_name, attrib_name,
                      attrib_value, *attrib_nelems);
	H5_API_RETURN (write_field_attrib (
                               f,
                               field_name, l_field_name,
                               attrib_name, l_attrib_name,
                               H5_I8_T,
                               attrib_value, *attrib_nelems));
}

#define h5bl_readfieldattrib_i8 F77_NAME (                          \
                h5bl_readfieldattrib_i8,                            \
                h5bl_readfieldattrib_i8_,                           \
                H5BL_READFIELDATTRIB_I8)
h5_err_t
h5bl_readfieldattrib_i8 (
	const h5_int64_t* const fh,
	const char* const field_name,
	const char* const attrib_name,
	h5_int64_t* const attrib_value,
	const int l_field_name,
	const int l_attrib_name,
	const int l_attrib_value
	) {
	h5_file_t* f = h5_filehandlefor2c (fh);
	H5_API_ENTER (h5_int64_t,
		      "fh=%p, field_name='%.*s', attrib_name='%.*s', "
                      "attrib_value=%p",
		      (h5_file_p)f,
                      l_field_name, field_name,
                      l_attrib_name, attrib_name,
                      attrib_value);
        H5_API_RETURN (read_field_attrib (
                               f,
                               field_name, l_field_name,
                               attrib_name, l_attrib_name,
                               H5_I8_T,
                               attrib_value));
}

#define h5bl_writefieldattrib_i4 F77_NAME (                         \
                h5bl_writefieldattrib_i4,                           \
                h5bl_writefieldattrib_i4_,                          \
                H5BL_WRITEFIELDATTRIB_I4 )
h5_int64_t
h5bl_writefieldattrib_i4 (
	const h5_int64_t* const fh,
	const char* const field_name,
	const char* const attrib_name,
	const h5_int32_t* const attrib_value,
	const h5_int64_t* const attrib_nelems,
	const int l_field_name,
	const int l_attrib_name
	) {
	h5_file_t* f = h5_filehandlefor2c (fh);
	H5_API_ENTER (h5_int64_t,
		      "fh=%p, field_name='%.*s', attrib_name='%.*s', "
                      "attrib_value=%p, attrib_nelems=%lld",
		      (h5_file_p)f,
                      l_field_name, field_name,
                      l_attrib_name, attrib_name,
                      attrib_value, *attrib_nelems);
	H5_API_RETURN (write_field_attrib (
                               f,
                               field_name, l_field_name,
                               attrib_name, l_attrib_name,
                               H5_I4_T,
                               attrib_value, *attrib_nelems));
}

#define h5bl_readfieldattrib_i4 F77_NAME (                          \
                h5bl_readfieldattrib_i4,                            \
                h5bl_readfieldattrib_i4_,                           \
                H5BL_READFIELDATTRIB_I4)
h5_err_t
h5bl_readfieldattrib_i4 (
	const h5_int64_t* const fh,
	const char* const field_name,
	const char* const attrib_name,
	h5_int32_t* const attrib_value,
	const int l_field_name,
	const int l_attrib_name
	) {
	h5_file_t* f = h5_filehandlefor2c (fh);
	H5_API_ENTER (h5_int64_t,
		      "fh=%p, field_name='%.*s', attrib_name='%.*s', "
                      "attrib_value=%p",
		      (h5_file_p)f,
                      l_field_name, field_name,
                      l_attrib_name, attrib_name,
                      attrib_value);
        H5_API_RETURN (read_field_attrib (
                               f,
                               field_name, l_field_name,
                               attrib_name, l_attrib_name,
                               H5_I4_T,
                               attrib_value));
}
