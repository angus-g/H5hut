/*
  Copyright (c) 2006-2016, The Regents of the University of California,
  through Lawrence Berkeley National Laboratory (subject to receipt of any
  required approvals from the U.S. Dept. of Energy) and the Paul Scherrer
  Institut (Switzerland).  All rights reserved.

  License: see file COPYING in top level of source distribution.
*/

#ifndef __H5BLOCK_ATTRIB
#define __H5BLOCK_ATTRIB

#include <string.h>

#include "h5core/h5_types.h"
#include "h5core/h5.h"
#include "h5core/h5_debug.h"
#include "h5core/h5b_attribs.h"

/**
   \addtogroup h5block_attrib
   @{
*/

/*
  !   __ _      _     _         _   _        _ _           _            
  !  / _(_) ___| | __| |   __ _| |_| |_ _ __(_) |__  _   _| |_ ___  ___ 
  ! | |_| |/ _ \ |/ _` |  / _` | __| __| '__| | '_ \| | | | __/ _ \/ __|
  ! |  _| |  __/ | (_| | | (_| | |_| |_| |  | | |_) | |_| | ||  __/\__ \
  ! |_| |_|\___|_|\__,_|  \__,_|\__|\__|_|  |_|_.__/ \__,_|\__\___||___/
  !
  !
  !   __ _ _   _  ___ _ __ _   _ 
  !  / _` | | | |/ _ \ '__| | | |
  ! | (_| | |_| |  __/ |  | |_| |
  !  \__, |\__,_|\___|_|   \__, |
  !     |_|                |___/ 
 */
/**
  Query the number of attributes of field \c field_name.

  \return number of attributes
  \return \c H5_FAILURE on error
*/
static inline h5_ssize_t
H5BlockGetNumFieldAttribs (
	const h5_file_t f,		///< [in]  file handle.
	const char* field_name		///< [in]  field name.
	) {
	H5_API_ENTER (h5_ssize_t,
                      "f=%p, field_name='%s'",
                      (h5_file_p)f, field_name);
	H5_API_RETURN (h5b_get_num_field_attribs (f, field_name));
}

/**
  Gets the name, type and number of elements of the field attribute
  specified by its index.

  This function can be used to retrieve all attributes bound to the
  specified field by looping from \c 0 to the number of attribute
  minus one.  The number of attributes bound to the
  field can be queried by calling \ref H5BlockGetNumFieldAttribs.

  \return \c H5_SUCCESS on success
  \return \c H5_FAILURE on error
*/
static inline h5_err_t
H5BlockGetFieldAttribInfo (
	const h5_file_t f,		///< [in]  file handle.
	const char* field_name,		///< [in]  field name.
	const h5_size_t attrib_idx,	///< [in]  index of attribute to query
	char* attrib_name,		///< [out] name of attribute.
	const h5_size_t len_attrib_name,///< [in]  length of buffer \c name.
	h5_int64_t* attrib_type,	///< [out] type of value.
	h5_size_t* attrib_nelem         ///< [out] number of elements.
	) {
	H5_API_ENTER (h5_err_t,
		      "f=%p field_name='%s', "
		      "attrib_idx=%llu, "
		      "attrib_name=%p, len_attrib_name=%llu, "
		      "attrib_type=%p, "
		      "attrib_nelem=%p",
		      (h5_file_p)f,
		      field_name,
		      (long long unsigned)attrib_idx,
		      attrib_name, (long long unsigned)len_attrib_name,
		      attrib_type,
		      attrib_nelem);
	H5_API_RETURN (
		h5b_get_field_attrib_info_by_idx (
			f,
			field_name,
			attrib_idx,
			attrib_name,
			len_attrib_name,
			attrib_type,
			attrib_nelem));
}

/*
  !  _    __          _        _             
  ! (_)  / /__    ___| |_ _ __(_)_ __   __ _ 
  ! | | / / _ \  / __| __| '__| | '_ \ / _` |
  ! | |/ / (_) | \__ \ |_| |  | | | | | (_| |
  ! |_/_/ \___/  |___/\__|_|  |_|_| |_|\__, |
  !                                    |___/
*/

/**
  Write the string in \c buffer as attribute \c attrib_name of field
  \c field_name.

  \return \c H5_SUCCESS on success
  \return \c H5_FAILURE on error
*/
static inline h5_err_t
H5BlockWriteFieldAttribString (
	const h5_file_t f,		///< [in]  file handle.
	const char* field_name,		///< [in]  field name.
	const char* attrib_name,	///< [in]  attribute name.
	const char* buffer		///< [in]  attribute value.
	) {
	H5_API_ENTER (h5_err_t,
		      "f=%p, "
		      "field_name='%s', "
		      "attrib_name='%s', "
		      "buffer='%s'",
		      (h5_file_p)f,
		      field_name,
		      attrib_name,
		      buffer);
	H5_API_RETURN (
		h5b_write_field_attrib (
			f,
			field_name,
			attrib_name,
			H5T_NATIVE_CHAR,
			buffer,
			strlen(buffer) + 1));
}

/**
  Read the string value from attribute \c attrib_name of field
  \c field_name into \c buffer.

  \return \c H5_SUCCESS on success
  \return \c H5_FAILURE on error
*/
static inline h5_err_t
H5BlockReadFieldAttribString (
	const h5_file_t f,		///< [in]  file handle.
	const char* field_name,		///< [in]  field name.
	const char* attrib_name,	///< [in]  attribute name.
	char* buffer			///< [out] attribute value.
	) {
	H5_API_ENTER (h5_err_t,
		      "f=%p, "
		      "field_name='%s', "
		      "attrib_name='%s', "
		      "buffer=%p",
		      (h5_file_p)f,
		      field_name,
		      attrib_name,
		      buffer);
	H5_API_RETURN (
		h5b_read_field_attrib (
			f,
			field_name,
			attrib_name,
			H5_STRING_T,
			(void*)buffer));
}

/*
  !  _    __                     _ 
  ! (_)  / /__    _ __ ___  __ _| |
  ! | | / / _ \  | '__/ _ \/ _` | |
  ! | |/ / (_) | | | |  __/ (_| | |
  ! |_/_/ \___/  |_|  \___|\__,_|_|
 */

/**
  Write float64 \c values as attribute \c attrib_name of field
  \c field_name.

  \return \c H5_SUCCESS on success
  \return \c H5_FAILURE on error
*/
static inline h5_err_t
H5BlockWriteFieldAttribFloat64 (
	const h5_file_t f,		///< [in]  file handle.
	const char* field_name,		///< [in]  field name.
	const char* attrib_name,	///< [in]  attribute name.
	const h5_float64_t* buffer,	///< [in]  attribute values.
	const h5_size_t nelems		///< [in]  number of elements.
	) {

	H5_API_ENTER (h5_err_t,
		      "f=%p, field_name='%s', attrib_name='%s', "
		      "buffer=%p, nelems=%lld",
		      (h5_file_p)f, field_name, attrib_name, buffer, (long long)nelems);
	H5_API_RETURN (h5b_write_field_attrib (
			       f,
			       field_name,
			       attrib_name,
			       H5T_NATIVE_DOUBLE,
			       buffer,
			       nelems ));
}

/**
  Read float64 values from attribute \c attrib_name of field
  \c field_name into a \c buffer.

  \return \c H5_SUCCESS on success
  \return \c H5_FAILURE on error
*/
static inline h5_err_t
H5BlockReadFieldAttribFloat64 (
	const h5_file_t f,		///< [in]  file handle.
	const char* field_name,		///< [in]  field name.
	const char* attrib_name,	///< [in]  attribute name.
	h5_float64_t* buffer		///< [out] attribute values.
	) {

        H5_API_ENTER (h5_err_t,
                      "f=%p, field_name='%s', attrib_name='%s', buffer=%p",
		      (h5_file_p)f, field_name, attrib_name, buffer);
	H5_API_RETURN (h5b_read_field_attrib (
			       f,
			       field_name,
			       attrib_name,
			       H5T_NATIVE_DOUBLE,
			       (void*)buffer ));
}


/**
  Write float32 \c values as attribute \c attrib_name of field
  \c field_name.

  \return \c H5_SUCCESS on success
  \return \c H5_FAILURE on error
*/
static inline h5_err_t
H5BlockWriteFieldAttribFloat32 (
	const h5_file_t f,		///< [in]  file handle.
	const char* field_name,		///< [in]  field name.
	const char* attrib_name,	///< [in]  attribute name.
	const h5_float32_t* buffer,	///< [in]  attribute values.
	const h5_size_t nelems		///< [in]  number of elements.
	) {

	H5_API_ENTER (h5_err_t,
		      "f=%p, field_name='%s', attrib_name='%s', "
		      "buffer=%p, nelems=%lld",
		      (h5_file_p)f, field_name, attrib_name, buffer, (long long)nelems);
	H5_API_RETURN (h5b_write_field_attrib (
			       f,
			       field_name,
			       attrib_name,
			       H5T_NATIVE_FLOAT,
			       buffer,
			       nelems ));
}

/**
  Read float32 values from attribute \c attrib_name of field
  \c field_name into a \c buffer.

  \return \c H5_SUCCESS on success
  \return \c H5_FAILURE on error
*/
static inline h5_err_t
H5BlockReadFieldAttribFloat32 (
	const h5_file_t f,		///< [in]  file handle.
	const char* field_name,		///< [in]  field name.
	const char* attrib_name,	///< [in]  attribute name.
	h5_float32_t* const buffer	///< [out] attribute values.
	) {

        H5_API_ENTER (h5_err_t,
                      "f=%p, field_name='%s', attrib_name='%s', buffer=%p",
		      (h5_file_p)f, field_name, attrib_name, buffer);
	H5_API_RETURN (h5b_read_field_attrib (
			       f,
			       field_name,
			       attrib_name,
			       H5T_NATIVE_FLOAT,
			       buffer ));
}

/*
  !  _    __      _       _                       
  ! (_)  / /__   (_)_ __ | |_ ___  __ _  ___ _ __ 
  ! | | / / _ \  | | '_ \| __/ _ \/ _` |/ _ \ '__|
  ! | |/ / (_) | | | | | | ||  __/ (_| |  __/ |   
  ! |_/_/ \___/  |_|_| |_|\__\___|\__, |\___|_|   
  !                               |___/
*/

/**
  Write int64 \c values as attribute \c attrib_name of field
  \c field_name.

  \return \c H5_SUCCESS on success
  \return \c H5_FAILURE on error
*/
static inline h5_err_t
H5BlockWriteFieldAttribInt64 (
	const h5_file_t f,		///< [in]  file handle.
	const char* field_name,		///< [in]  field name.
	const char* attrib_name,	///< [in]  attribute name.
	const h5_int64_t* buffer,	///< [in]  attribute values.
	const h5_size_t nelems		///< [in]  number of elements.
	) {

	H5_API_ENTER (h5_err_t,
		      "f=%p, field_name='%s', attrib_name='%s', buffer=%p, nelems=%lld",
		      (h5_file_p)f, field_name, attrib_name, buffer, (long long)nelems);
	H5_API_RETURN (h5b_write_field_attrib (
			       f,
			       field_name,
			       attrib_name,
			       H5T_NATIVE_INT64,
			       buffer,
			       nelems ));
}

/**
  Read int64 values from attribute \c attrib_name of field
  \c field_name into a \c buffer.

  \return \c H5_SUCCESS on success
  \return \c H5_FAILURE on error
*/
static inline h5_err_t
H5BlockReadFieldAttribInt64 (
	const h5_file_t f,		///< [in]  file handle.
	const char* field_name,		///< [in]  field name.
	const char* attrib_name,	///< [in]  attribute name.
	h5_int64_t* const buffer	///< [out] attribute values.
	) {

        H5_API_ENTER (h5_err_t,
                      "f=%p, field_name='%s', attrib_name='%s', buffer=%p",
		      (h5_file_p)f, field_name, attrib_name, buffer);
	H5_API_RETURN (h5b_read_field_attrib (
			       f,
			       field_name,
			       attrib_name,
			       H5T_NATIVE_INT64,
			       buffer ));
}


/**
  Write int32 \c values as attribute \c attrib_name of field
  \c field_name.

  \return \c H5_SUCCESS on success
  \return \c H5_FAILURE on error
*/
static inline h5_err_t
H5BlockWriteFieldAttribInt32 (
	const h5_file_t f,		///< [in]  file handle.
	const char* field_name,		///< [in]  field name.
	const char* attrib_name,	///< [in]  attribute name.
	const h5_int32_t* buffer,	///< [in]  attribute values.
	const h5_size_t nelems		///< [in]  number of elements.
	) {

	H5_API_ENTER (h5_err_t,
		      "f=%p, field_name='%s', attrib_name='%s', "
		      "buffer=%p, nelems=%lld",
		      (h5_file_p)f, field_name, attrib_name, buffer, (long long)nelems);
	H5_API_RETURN (h5b_write_field_attrib (
			       f,
			       field_name,
			       attrib_name,
			       H5T_NATIVE_INT32,
			       buffer,
			       nelems ));
}

/**
  Read int32 values from attribute \c attrib_name of field
  \c field_name into a \c buffer.

  \return \c H5_SUCCESS on success
  \return \c H5_FAILURE on error
*/
static inline h5_err_t
H5BlockReadFieldAttribInt32 (
	const h5_file_t f,		///< [in]  file handle.
	const char* field_name,		///< [in]  field name.
	const char* attrib_name,	///< [in]  attribute name.
	h5_int32_t* buffer		///< [out] attribute values.
	) {
        H5_API_ENTER (h5_err_t,
                      "f=%p, field_name='%s', attrib_name='%s', buffer=%p",
		      (h5_file_p)f, field_name, attrib_name, buffer);
	H5_API_RETURN (h5b_read_field_attrib (
			       f,
			       field_name,
			       attrib_name,
			       H5T_NATIVE_INT32,
			       (void*)buffer ));
}

/*
  !   __ _      _     _              _       _       
  !  / _(_) ___| | __| |   ___  _ __(_) __ _(_)_ __  
  ! | |_| |/ _ \ |/ _` |  / _ \| '__| |/ _` | | '_ \ 
  ! |  _| |  __/ | (_| | | (_) | |  | | (_| | | | | |
  ! |_| |_|\___|_|\__,_|  \___/|_|  |_|\__, |_|_| |_|
  !                                    |___/
*/
/**
  Get field origin.

  \return \c H5_SUCCESS on success
  \return \c H5_FAILURE on error
*/
static inline h5_err_t
H5Block3dGetFieldOrigin (
	const h5_file_t f,		///< [in]  file handle.
	const char* field_name,		///< [in]  field name.
	h5_float64_t* x_origin,		///< [out] X origin.
	h5_float64_t* y_origin,		///< [out] Y origin.
	h5_float64_t* z_origin		///< [out] Z origin.
	) {
	H5_API_ENTER (h5_err_t,
		      "f=%p, field_name='%s', x_origin=%p, y_origin=%p, z_origin=%p",
		      (h5_file_p)f, field_name, x_origin, y_origin, z_origin);
	h5_float64_t origin[3];

	TRY (h5b_read_field_attrib (
		     f,
		     field_name,
		     H5BLOCK_FIELD_ORIGIN_NAME,
		     H5_FLOAT64_T,
		     origin));

	*x_origin = origin[0];
	*y_origin = origin[1];
	*z_origin = origin[2];

	H5_API_RETURN (H5_SUCCESS);
}

/**
  Set field origin.

  \return \c H5_SUCCESS on success
  \return \c H5_FAILURE on error
*/
static inline h5_err_t
H5Block3dSetFieldOrigin (
	const h5_file_t f,			///< [in]  file handle.
	const char* field_name,		///< [in]  field name.
	const h5_float64_t x_origin,	///< [in]  X origin.
	const h5_float64_t y_origin,	///< [in]  Y origin.
	const h5_float64_t z_origin	///< [in]  Z origin.
	) {
	H5_API_ENTER (h5_err_t,
		      "f=%p, field_name='%s', x_origin=%g, y_origin=%g, z_origin=%g",
		      (h5_file_p)f, field_name, x_origin, y_origin, z_origin);
	h5_float64_t origin[3] = { x_origin, y_origin, z_origin };
	H5_API_RETURN (h5b_write_field_attrib (
			       f,
			       field_name,
			       H5BLOCK_FIELD_ORIGIN_NAME,
			       (hid_t)H5_FLOAT64_T, 
			       origin,
			       3));
}

/*
  !   __ _      _     _                        _             
  !  / _(_) ___| | __| |  ___ _ __   __ _  ___(_)_ __   __ _ 
  ! | |_| |/ _ \ |/ _` | / __| '_ \ / _` |/ __| | '_ \ / _` |
  ! |  _| |  __/ | (_| | \__ \ |_) | (_| | (__| | | | | (_| |
  ! |_| |_|\___|_|\__,_| |___/ .__/ \__,_|\___|_|_| |_|\__, |
  !                          |_|                       |___/ 
*/

/**
  Get field spacing for field \c field_name in the current time step.

  \return \c H5_SUCCESS on success
  \return \c H5_FAILURE on error
*/
static inline h5_err_t
H5Block3dGetFieldSpacing (
	const h5_file_t f,		///< [in]  file handle.
	const char* field_name,		///< [in]  field name.
	h5_float64_t* x_spacing,	///< [out] X spacing.
	h5_float64_t* y_spacing,	///< [out] Y spacing.
	h5_float64_t* z_spacing		///< [out] Z spacing.
	) {
	H5_API_ENTER (h5_err_t,
		      "f=%p, field_name='%s', "
                      "x_spacing=%p, y_spacing=%p, z_spacing=%p",
		      (h5_file_p)f, field_name, x_spacing, y_spacing, z_spacing);
	h5_float64_t spacing[3];
	TRY (h5b_read_field_attrib (
		     f,
		     field_name,
		     H5BLOCK_FIELD_SPACING_NAME,
		     H5_FLOAT64_T,
		     spacing));
	*x_spacing = spacing[0];
	*y_spacing = spacing[1];
	*z_spacing = spacing[2];
	H5_API_RETURN (H5_SUCCESS);
}

/**
  Set field spacing for field \c field_name in the current time step.

  \return \c H5_SUCCESS on success
  \return \c H5_FAILURE on error
*/
static inline h5_err_t
H5Block3dSetFieldSpacing (
	const h5_file_t f,		///< [in]  file handle.
	const char* field_name,		///< [in]  field name.
	const h5_float64_t x_spacing,	///< [in]  X spacing.
	const h5_float64_t y_spacing,	///< [in]  Y spacing.
	const h5_float64_t z_spacing	///< [in]  Z spacing.
	) {
	H5_API_ENTER (h5_err_t,
		      "f=%p, field_name='%s', x_spacing=%g, y_spacing=%g, z_spacing=%g",
		      (h5_file_p)f, field_name, x_spacing, y_spacing, z_spacing);
	h5_float64_t spacing[3] = { x_spacing, y_spacing, z_spacing };
	H5_API_RETURN (h5b_write_field_attrib (
			       f,
			       field_name,
			       H5BLOCK_FIELD_SPACING_NAME,
			       (hid_t)H5_FLOAT64_T, 
			       spacing,
			       3));
}

/*
  !   __ _      _     _                           _     
  !  / _(_) ___| | __| |   ___ ___   ___  _ __ __| |___ 
  ! | |_| |/ _ \ |/ _` |  / __/ _ \ / _ \| '__/ _` / __|
  ! |  _| |  __/ | (_| | | (_| (_) | (_) | | | (_| \__ \
  ! |_| |_|\___|_|\__,_|  \___\___/ \___/|_|  \__,_|___/
*/
/**
  Set an explicit list of X coordinates for field \c field_name in the current
  time step. The coordinates are a 1D array of floating point values with
  dimension \c n_coords.

  By convention, the \c coords array should have the same length as the X
  dimension of the field, and a warning will be printed if not.

  \return \c H5_SUCCESS on success
  \return \c H5_FAILURE on error
*/
static inline h5_err_t
H5Block3dSetFieldXCoords (
	const h5_file_t f,			///< [in] file handle 
	const char* field_name,		///< [in] field name
	const h5_float64_t* const coords,///< [in] X coordinates
	const h5_int64_t n_coords	///< [in] number of coordinates
	) {
	H5_API_ENTER (h5_err_t,
                      "f=%p, "
                      "field_name='%s', "
                      "coords=%p, n_coords=%llu",
                      (h5_file_p)f,
                      field_name,
                      coords, (long long unsigned)n_coords);
        H5_API_RETURN (h5b_set_3d_field_coords (
                               f, 0, field_name, H5BLOCK_FIELD_XCOORD_NAME,
                               coords, n_coords));
}


/**
  Get the explicit list of X coordinates for field \c field_name in the current
  time step. The coordinates are read into the 1D array \c coords which has
  length \c n_coords.

  By convention, the \c coords array should have the same length as the X
  dimension of the field, and a warning will be printed if they differ.

  \return \c H5_SUCCESS on success
  \return \c H5_FAILURE on error
*/
static inline h5_err_t
H5Block3dGetFieldXCoords (
	const h5_file_t f,			///< [in] file handle
	const char* field_name,		///< [in] field name
	h5_float64_t* const coords,	///< [in] X coordinates
	const h5_int64_t n_coords	///< [in] number of coordinates
	) {
	H5_API_ENTER (h5_err_t,
                      "f=%p, "
                      "field_name='%s', "
                      "coords=%p, n_coords=%llu",
                      (h5_file_p)f,
                      field_name,
                      coords, (long long unsigned)n_coords);
        H5_API_RETURN (h5b_get_3d_field_coords (
                               f, 0, field_name, H5BLOCK_FIELD_XCOORD_NAME,
                               coords, n_coords));
}

/**
  Set an explicit list of Y coordinates for field \c field_name in the current
  time step.

  \see H5Block3dSetFieldXCoords()

  \return \c H5_SUCCESS on success
  \return \c H5_FAILURE on error
*/
static inline h5_err_t
H5Block3dSetFieldYCoords (
	const h5_file_t f,			///< [in] file handle 
	const char* field_name,		///< [in] field name
	const h5_float64_t* const coords,///< [in] X coordinates
	const h5_int64_t n_coords	///< [in] number of coordinates
	) {
	H5_API_ENTER (h5_err_t,
                      "f=%p, "
                      "field_name='%s', "
                      "coords=%p, n_coords=%llu",
                      (h5_file_p)f,
                      field_name,
                      coords, (long long unsigned)n_coords);
        H5_API_RETURN (h5b_set_3d_field_coords (
                               f, 1, field_name, H5BLOCK_FIELD_YCOORD_NAME,
                               coords, n_coords));
}


/**
  Get the explicit list of Y coordinates for field \c field_name in the current
  time step.

  \see H5Block3dGetFieldXCoords()

  \return \c H5_SUCCESS on success
  \return \c H5_FAILURE on error
*/
static inline h5_err_t
H5Block3dGetFieldYCoords (
	const h5_file_t f,			///< [in] file handle
	const char* field_name,		///< [in] field name
	h5_float64_t* const coords,	///< [in] Y coordinates
	const h5_int64_t n_coords	///< [in] number of coordinates
	) {
	H5_API_ENTER (h5_err_t,
                      "f=%p, "
                      "field_name='%s', "
                      "coords=%p, n_coords=%llu",
                      (h5_file_p)f,
                      field_name,
                      coords, (long long unsigned)n_coords);
        H5_API_RETURN (h5b_get_3d_field_coords (
                               f, 1, field_name, H5BLOCK_FIELD_YCOORD_NAME,
                               coords, n_coords));
}

/**
  Set an explicit list of Z coordinates for field \c field_name in the current
  time step.

  \see H5Block3dSetFieldXCoords()

  \return \c H5_SUCCESS on success
  \return \c H5_FAILURE on error
*/
static inline h5_err_t
H5Block3dSetFieldZCoords (
	const h5_file_t f,			///< [in] file handle 
	const char* field_name,		///< [in] field name
	const h5_float64_t* const coords,///< [in] Z coordinates
	const h5_int64_t n_coords	///< [in] number of coordinates
	) {
	H5_API_ENTER (h5_err_t,
                      "f=%p, "
                      "field_name='%s', "
                      "coords=%p, n_coords=%llu",
                      (h5_file_p)f,
                      field_name,
                      coords, (long long unsigned)n_coords);
        H5_API_RETURN (h5b_set_3d_field_coords (
                               f, 2, field_name, H5BLOCK_FIELD_ZCOORD_NAME,
                               coords, n_coords));
}


/**
  Get the explicit list of Y coordinates for field \c field_name in the current
  time step.

  \see H5Block3dGetFieldXCoords()

  \return \c H5_SUCCESS on success
  \return \c H5_FAILURE on error
*/
static inline h5_err_t
H5Block3dGetFieldZCoords (
	const h5_file_t f,			///< [in] file handle
	const char* field_name,		///< [in] field name
	h5_float64_t* const coords,	///< [in] Z coordinates
	const h5_int64_t n_coords	///< [in] number of coordinates
	) {
	H5_API_ENTER (h5_err_t,
                      "f=%p, "
                      "field_name='%s', "
                      "coords=%p, n_coords=%llu",
                      (h5_file_p)f,
                      field_name,
                      coords, (long long unsigned)n_coords);
        H5_API_RETURN (h5b_get_3d_field_coords (
                               f, 2, field_name, H5BLOCK_FIELD_ZCOORD_NAME,
                               coords, n_coords));
}
///< @}
#endif
