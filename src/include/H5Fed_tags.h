#ifndef __H5FED_TAGS_H
#define __H5FED_TAGS_H

#ifdef __cplusplus
extern "C" {
#endif

/*!
  Add a tagset to the current mesh.

  \param[in]	f	file handle
  \param[in]	name	name of tagset
  \param[in]	type	data type of tagset

  \return	H5_SUCCESS or error code
 */
static inline h5_err_t
H5FedAddMTagset (
	h5_file_t* const f,
	char* name,
	h5_id_t type
	) {
	H5_API_ENTER3 (h5_err_t, "f=0x%p, name=\"%s\", type=%lld",
		       f, name, (long long)type);
	H5_API_RETURN (h5t_add_mtagset (f, name, type));
}

/*!
  Remove a tagset from the current mesh.

  \param[in]	f	file handle
  \param[in]	name	name of tagset to remove

  \return	H5_SUCCESS or error code
 */
static inline h5_err_t
H5FedRemoveMTagset (
	h5_file_t* const f,
	char name[]
	) {
	H5_API_ENTER2 (h5_err_t, "f=0x%p, name=\"%s\"", f, name);
	H5_API_RETURN (h5t_remove_mtagset (f, name));
}

/*!
  Get available tagsets in current mesh.

  \param[in]	f	file handle
  \param[out]	names	names of available tagsets

  \return	Number of tagsets or error code
 */
static inline h5_ssize_t
H5FedGetMTagsets (
	h5_file_t* const f,
	char** names[]
	) {
	H5_API_ENTER2 (h5_ssize_t, "f=0x%p, names=0x%p", f, names);
	H5_API_RETURN (h5t_get_mtagsets (f, names));
}

/*!
  Get type of tagset in current mesh.

  \param[in]	f	file handle
  \param[in]	name	name of tagset

  \return	H5_SUCCESS or error code
 */
static inline h5_id_t
H5FedGetTypeOfMTagset (
	h5_file_t* const f,
	char name[]
	) {
	H5_API_ENTER2 (h5_err_t, "f=0x%p, name=\"%s\"", f, name);
	H5_API_RETURN (h5t_get_mtagset_type_by_name (f, name));
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
static inline h5_err_t
H5FedSetMTag (
	h5_file_t* const f,
	char name[],
	h5_loc_id_t id,
	const size_t dim,
	void* buffer
	) {
	H5_API_ENTER5 (h5_err_t,
		       "f=0x%p, name=\"%s\", id=%lld, dim=%lld, val=0x%p",
		       f, name, (long long)id, (long long)dim, buffer);
	H5_API_RETURN (h5t_set_mtag_by_name (f, name, id, dim, buffer));
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
static inline h5_err_t
H5FedGetMTag (
	h5_file_t* const f,
	const char name[],
	const h5_loc_id_t id,
	size_t* dim,
	void* buffer
	) {
	H5_API_ENTER5 (h5_err_t,
		       "f=0x%p, name=\"%s\", id=%lld, dim=0x%p, val=0x%p",
		       f, name, (long long)id, dim, buffer);
	H5_API_RETURN (h5t_get_mtag_by_name (f, name, id, dim, buffer));
}

/*!
  Remove tag for entity in current mesh.

  \param[in]	f	file handle
  \param[in]	name	names of tagset
  \param[in]	id	id of entity
*/
static inline h5_err_t
H5FedRemoveMTag (
	h5_file_t* const f,
	const char name[],
	const h5_loc_id_t id
	) {
	H5_API_ENTER3 (h5_err_t, "f=0x%p, name=\"%s\", id=%lld",
		       f, name, (long long)id);
	H5_API_RETURN (h5t_remove_mtag_by_name (f, name, id));
}

#ifdef __cplusplus
}
#endif

#endif
