/*
  Copyright 2007-2009
 	Paul Scherrer Institut, Villigen, Switzerland;
 	Achim Gsell
 	All rights reserved.
 
  Authors
 	Achim Gsell
  
  Warning
	This code is under development.
 
 */
#include <hdf5.h>
#include "h5core/h5_core.h"
#include "H5Fed.h"

/*!
  Add a tagset to the current mesh.

  \param[in]	f	file handle
  \param[in]	name	name of tagset
  \param[in]	type	data type of tagset

  \return	H5_SUCCESS or error code
 */
h5_err_t
H5FedAddMTagset (
	h5_file_t * const f,
	char * name,
	h5_id_t type
	) {
	SET_FNAME ( f, __func__ );
	return h5t_add_mtagset ( f, name, type );
}

/*!
  Remove a tagset from the current mesh.

  \param[in]	f	file handle
  \param[in]	name	name of tagset to remove

  \return	H5_SUCCESS or error code
 */
h5_err_t
H5FedRemoveMTagset (
	h5_file_t *const f,
	char name[]
	) {
	SET_FNAME ( f, __func__ );
	return h5t_remove_mtagset (f, name );
}

/*!
  Get available tagsets in current mesh.

  \param[in]	f	file handle
  \param[out]	names	names of available tagsets

  \return	Number of tagsets or error code
 */
h5_size_t
H5FedGetMTagsets (
	h5_file_t *const f,
	char **names[]
	) {
	SET_FNAME ( f, __func__ );
	return h5t_get_mtagsets ( f, names );
}

/*!
  Get type of tagset in current mesh.

  \param[in]	f	file handle
  \param[in]	name	name of tagset

  \return	H5_SUCCESS or error code
 */
h5_id_t
H5FedGetTypeOfMTagset (
	h5_file_t *const f,
	char name[]
	) {
	SET_FNAME ( f, __func__ );
	return h5t_get_mtagset_type_by_name ( f, name );
}

/*!
  Set tag for entity in current mesh.

  \param[in]	f	file handle
  \param[in]	name	names of tagset
  \param[in]	id	id of entity
  \param[in]	dim	dimension of value
  \param[in]	val	tag value

  \return	H5_SUCCESS or error code
 */
h5_err_t
H5FedSetMTag (
	h5_file_t *const f,
	char name[],
	h5_id_t id,
	const size_t dims,
	void *val 
	) {
	SET_FNAME ( f, __func__ );
	return h5t_set_mtag_by_name ( f, name, id, dims, val );
}

h5_err_t
H5FedSetMTagToVertex (
	h5_file_t *const f,
	char name[],
	h5_id_t id,
	const size_t dims,
	void *val 
	) {
	SET_FNAME ( f, __func__ );
	id = h5tpriv_set_entity_type ( H5T_ETYPE_VERTEX, id );
	return h5t_set_mtag_by_name ( f, name, id, dims, val );
}

h5_err_t
H5FedSetMTagToEdge (
	h5_file_t *const f,
	char name[],
	h5_id_t id,
	const size_t dims,
	void *val 
	) {
	SET_FNAME ( f, __func__ );
	id = h5tpriv_set_entity_type ( H5T_ETYPE_EDGE, id );
	return h5t_set_mtag_by_name ( f, name, id, dims, val );
}

h5_err_t
H5FedSetMTagToTriangle (
	h5_file_t *const f,
	char name[],
	h5_id_t id,
	const size_t dims,
	void *val 
	) {
	SET_FNAME ( f, __func__ );
	id = h5tpriv_set_entity_type ( H5T_ETYPE_TRIANGLE, id );
	return h5t_set_mtag_by_name ( f, name, id, dims, val );
}

h5_err_t
H5FedSetMTagToTet (
	h5_file_t *const f,
	char name[],
	h5_id_t id,
	const size_t dims,
	void *val 
	) {
	SET_FNAME ( f, __func__ );
	id = h5tpriv_set_entity_type ( H5T_ETYPE_TET, id );
	return h5t_set_mtag_by_name ( f, name, id, dims, val );
}

/*!
  Set tag for entity in current mesh.

  \param[in]	f	file handle
  \param[in]	name	names of tagset
  \param[in]	id	id of entity
  \param[out]	dim	dimension of value
  \param[out]	val	tag value

  \return	H5_SUCCESS or error code
 */
h5_err_t
H5FedGetMTag (
	h5_file_t* const f,
	const char name[],
	const h5_id_t id,
	size_t* dim,
	void* vals
	) {
	SET_FNAME ( f, __func__ );
	return h5t_get_mtag_by_name ( f, name, id, dim, vals );
}

/*!
  Remove tag for entity in current mesh.

  \param[in]	f	file handle
  \param[in]	name	names of tagset
  \param[in]	id	id of entity
*/
h5_err_t
H5FedRemoveMTag (
	h5_file_t *const f,
	const char name[],
	const h5_id_t id
	) {
	SET_FNAME ( f, __func__ );
	return h5t_remove_mtag_by_name ( f, name, id );
}
