
#include <string.h>

#include "h5core/h5_core.h"
#include "h5_core_private.h"

/*
  TODO:
  - read tagsets on demand
 */

/*
  Release a tag-set
 */
static inline h5_err_t
release_tagset (
	h5t_tagset_t* tagset
	) {
	H5_PRIV_FUNC_ENTER (h5_err_t);
	unsigned int i;
	// release per element structures
	for (i = 0; i < tagset->num_elems; i++) {
		if (tagset->elems[i] != NULL) {
			TRY (h5_free (tagset->elems[i]));
		}
	}
	// release other memory
	TRY (h5_free (tagset->name));
	TRY (h5_free (tagset->values));
	TRY (h5_free (tagset));
	H5_PRIV_FUNC_RETURN (H5_SUCCESS);
}

/*
  Release a tagset. Special version for releasing a container.
 */
static h5_err_t
release_tagset2 (
	const void* __tagset
	) {
	H5_PRIV_FUNC_ENTER (h5_err_t);
	h5t_tagset_t* tagset = *(h5t_tagset_t**)__tagset;
	H5_PRIV_FUNC_RETURN (release_tagset (tagset));
}


/*
  Initialize a tag container
 */
static inline h5_err_t
init_container (
	const size_t ntags,
	h5t_tagcontainer_t* ctn
	) {
	H5_PRIV_FUNC_ENTER (h5_err_t);
	TRY (ctn->names = h5_calloc (ntags, sizeof(char*)));
	TRY (h5priv_hcreate_string_keyed (ntags, &ctn->sets,
					  release_tagset2));
	H5_PRIV_FUNC_RETURN (H5_SUCCESS);
}

/*
  Release all sets in given container
 */
static inline h5_err_t
release_container (
	h5t_tagcontainer_t* ctn
	) {
	H5_PRIV_FUNC_ENTER (h5_err_t);	
	if (ctn->num_sets == 0)
		H5_PRIV_FUNC_LEAVE (H5_SUCCESS);
	TRY (h5priv_hdestroy (&ctn->sets)); 
	TRY (h5_free (ctn->names));
	memset (ctn, 0, sizeof (*ctn));
	H5_PRIV_FUNC_RETURN (H5_SUCCESS);
}

/*
  Release all tagsets
*/
h5_err_t
h5tpriv_release_tags (
	h5_file_t* const f
	) {
	H5_PRIV_API_ENTER1 (h5_err_t, "f=0x%p", f);
	h5t_fdata_t* t = f->t;
	TRY (release_container (&t->mtags));
	H5_PRIV_API_RETURN (H5_SUCCESS);
}

static inline h5_err_t
tagset_exists (
	h5t_tagcontainer_t* ctn,
	char* name
	) {
	H5_PRIV_FUNC_ENTER (h5_err_t);
	h5t_tagset_t tagset;
	tagset.name = name;
	H5_PRIV_FUNC_RETURN (h5priv_hsearch (&tagset,H5_FIND,NULL,&ctn->sets));
}

h5_err_t
h5t_mtagset_exists (
	h5_file_t* const f,
	h5t_tagcontainer_t* ctn,
	char* name
	) {
	H5_CORE_API_ENTER3 (h5_err_t,
			   "f=0x%p, ctn=0x%p, name=\"%s\"",
			   f, ctn, name);
	H5_CORE_API_RETURN (tagset_exists (&f->t->mtags, name));
}

/*!
  Add tag to current mesh.

  \param[in]	f	File handle
  \param[in]	name	name of tag
  \param[in]	type	type
  \param[out]	tag	pointer to tag object if != 0

  \return	H5_SUCCESS or error code
*/
static h5_err_t
add_tagset (
	h5_file_t* const f,
	h5t_tagcontainer_t* ctn,
	char* name,
	h5_id_t type,
	h5t_tagset_t** rtagset
	) {
	H5_PRIV_FUNC_ENTER5 (h5_err_t,
			     "f=0x%p, ctn=0x%p, name=\"%s\", type=%llu, rtagset=0x%p",
			     f, ctn, name,
			     (long long unsigned)type,
			     rtagset);
	h5t_fdata_t* t = f->t;

	// Initialize data structure for m-tagsets, if not already done.
	if (ctn->names == NULL) {
		TRY (init_container (521, ctn));
	}
	// TODO: Resize if we have more then 80% of 521 filled!

	// validate name
	if (name == NULL || name[0] == '\0') {
		H5_PRIV_FUNC_LEAVE (
			h5_error (H5_ERR_INVAL, "Invalid name" ));
	}

	// validate type
	if (type != H5_INT64_T && type != H5_FLOAT64_T) {
		H5_PRIV_FUNC_LEAVE (
			h5_error (H5_ERR_INVAL, "Unsupported data type." ));
	}

	// check if a tagset with given name already exists
	h5_err_t h5err;
	TRY (h5err = tagset_exists (ctn, name) );
	if (h5err == H5_SUCCESS) {
		H5_PRIV_FUNC_LEAVE (
			h5_error (H5_ERR_INVAL,
				  "Tagset with name %s already exists!",
				  name));
	}

	// create new tagset
	h5t_tagset_t* tagset = NULL;
	size_t size = (t->num_elems[t->num_leaf_levels-1] - 1) * sizeof(*tagset->elems)
		+ sizeof(*tagset);
	TRY (tagset = h5_calloc (1, size));

	TRY (tagset->name = h5priv_strdup (name));
	tagset->type = type;
	tagset->num_elems = t->num_elems[t->num_leaf_levels-1];
	tagset->scope.min_level = 32767;
	tagset->scope.max_level = -1;

	// add tagset to hash of tagsets
	TRY (h5priv_hsearch (tagset, H5_ENTER, NULL, &ctn->sets));

	t->mtags.changed = 1;
	t->mtags.names[t->mtags.num_sets] = tagset->name;
	t->mtags.num_sets++;

	if (rtagset != NULL) *rtagset = tagset;
	H5_PRIV_FUNC_RETURN (H5_SUCCESS);
}

h5_err_t
h5t_add_mtagset (
	h5_file_t* const f,
	char* name,
	h5_id_t type
	) {
	H5_CORE_API_ENTER3 (h5_err_t,
			     "f=0x%p, name=\"%s\", type=%llu",
			     f, name,
			    (long long unsigned)type);
	h5t_fdata_t* t = f->t;
	H5_CORE_API_RETURN (add_tagset (f, &t->mtags, name, type, NULL));
}


/*!
  Remove a tagset from the current mesh.

  \param[in]	f	file handle
  \param[in]	name	name of tagset to remove

  \return	H5_SUCCESS or error code
 */
static h5_err_t
remove_tagset (
	h5_file_t* const f,
	h5t_tagcontainer_t* ctn,
	const char name[]
	) {
	H5_PRIV_FUNC_ENTER (h5_err_t);
	// remove tagset with NAME from m-tagsets dictionary
	void* __retval = NULL;
	TRY (h5priv_hsearch (&name, H5_REMOVE, &__retval, &ctn->sets));
	h5t_tagset_t* tagset = (h5t_tagset_t*)__retval;
	if (tagset == NULL) {
		H5_PRIV_FUNC_LEAVE (H5_SUCCESS);
	}
	TRY (release_tagset (tagset));

	// remove HDF5 datasets and group for this tagset
	hid_t loc_id;
	TRY (loc_id = h5priv_open_group (f, ctn->group_id, name));
	TRY (hdf5_delete_link (loc_id, "elems", H5P_DEFAULT));
	TRY (hdf5_delete_link (loc_id, "entities", H5P_DEFAULT));
	TRY (hdf5_delete_link (loc_id, "values", H5P_DEFAULT));
	TRY (hdf5_close_group (loc_id));
	TRY (hdf5_delete_link (ctn->group_id, name, H5P_DEFAULT));

	H5_PRIV_FUNC_RETURN (H5_SUCCESS);
}

/*!
  Remove a tagset from the current mesh.

  \param[in]	f	file handle
  \param[in]	name	name of tagset to remove

  \return	H5_SUCCESS or error code
 */
h5_err_t
h5t_remove_mtagset (
	h5_file_t* const f,
	const char name[]
	) {
	H5_CORE_API_ENTER2 (h5_err_t,
			    "f=0x%p, name=\"%s\"",
			    f, name);
	h5t_fdata_t* t = f->t;
	TRY (t->mtags.group_id = h5priv_open_group (f, t->mesh_gid, "Tags"));
	TRY (remove_tagset (f, &t->mtags, name));
	TRY (hdf5_close_group (t->mtags.group_id));
	H5_CORE_API_RETURN (H5_SUCCESS);
}

h5_ssize_t
h5t_get_num_mtagsets (
	h5_file_t* const f
	) {
	H5_CORE_API_ENTER1 (h5_ssize_t, "f=0x%p", f);
	h5t_fdata_t* t = f->t;
	H5_CORE_API_RETURN (t->mtags.num_sets);
}

/*!
  Get available tagset in current mesh.

  \param[in]	f	file handle
  \param[out]	names	names of available tagsets

  \return	Number of mesh tag-sets
 */
h5_ssize_t
h5t_get_mtagsets (
	h5_file_t* const f,
	char** names[]
	) {
	H5_CORE_API_ENTER2 (h5_ssize_t, "f=0x%p, names=0x%p", f, names);
	h5t_fdata_t* t = f->t;
	*names = t->mtags.names; 
	H5_CORE_API_RETURN (t->mtags.num_sets);
}

h5_err_t
h5t_open_mtagset (
	h5_file_t* const f,
	const char* name,
	h5t_tagset_t** retval
	) {
	H5_CORE_API_ENTER3 (h5_ssize_t,
			    "f=0x%p, name=\"%s\", retval=0x%p",
			    f, name, retval);
	h5t_fdata_t* t = f->t;
	void* __retval = NULL;
	TRY (h5priv_hsearch (&name, H5_FIND, &__retval, &t->mtags.sets));
	*retval = (h5t_tagset_t*)__retval;
	H5_CORE_API_RETURN (H5_SUCCESS);
}

/*!
  Get type of tagset in current mesh.

  \param[in]	f	file handle
  \param[in]	name	name of tagset

  \return	type or error code
 */
h5_id_t
h5t_get_mtagset_type_by_name (
	h5_file_t* const f,
	char* name
	) {
	H5_CORE_API_ENTER2 (h5_ssize_t,
			    "f=0x%p, name=\"%s\"",
			    f, name);
	h5t_tagset_t* tagset;
	TRY (h5t_open_mtagset (f, name, &tagset));
	H5_CORE_API_RETURN (tagset->type);
}

/*!
  Get information about a tagset 

  \param[in]	f	file handle
  \param[in]	idx	index of tagset we want information about
  \param[out]	name	name of tag-set
  \param[out]	type	type of tag-set
 */
h5_ssize_t
h5t_get_mtagset_info (
	h5_file_t* const f,
	const h5_id_t idx,
	char** name,
	h5_id_t* type
	) {
	H5_CORE_API_ENTER4 (h5_ssize_t,
			    "f=0x%p, idx=%lld, name=0x%p, type=0x%p",
			    f, (long long)idx, name, type);
	h5t_fdata_t* t = f->t;
	*name = t->mtags.names[idx];
	void* __retval = NULL;
	TRY (h5priv_hsearch (t->mtags.names[idx], H5_FIND, &__retval,
			      &t->mtags.sets));
	h5t_tagset_t* retval = (h5t_tagset_t*)__retval;
	*type = retval->type;
	H5_CORE_API_RETURN (H5_SUCCESS);
}

static int
find_face_id (
	h5t_tageleminfo_t* eleminfo,
	h5_loc_idx_t face_id
	) {
	if (eleminfo->num_tags == 0) return -1;
	h5t_taginfo_t* taginfo = eleminfo->ti;
	register int low = 0;
	register int high = eleminfo->num_tags-1;
	while (low <= high) {
		register int mid = (low + high) / 2;
		register int diff = taginfo[mid].face_id - face_id;
		if (diff > 0)
			high = mid -1;
		else if (diff < 0)
			low = mid + 1;
		else
			return mid;
	}
	return -(low+1);
}

static inline h5_err_t
tag_nexists_warn (
	const char* name,
	const h5_loc_idx_t face_id,
	const h5_loc_idx_t elem_idx
	) {
	return h5_warn (
		"Tag %s not set for face %llx of element %lld",
		name,
		(long long)face_id,
		(long long)elem_idx);
}

static h5_err_t
remove_tag (
	h5t_tagset_t* tagset,
	const h5_loc_idx_t face_id,
	const h5_loc_idx_t elem_idx
	) {
	H5_PRIV_FUNC_ENTER (h5_err_t);
	if (tagset->elems[elem_idx] == NULL) {
		H5_PRIV_FUNC_LEAVE (
			tag_nexists_warn (tagset->name, face_id, elem_idx));
	}
	h5t_tageleminfo_t* eleminfo = tagset->elems[elem_idx];

	// remove values
	int idx = find_face_id (eleminfo, face_id);
	if (idx < 0) {
		H5_PRIV_FUNC_LEAVE (
			tag_nexists_warn (tagset->name, face_id, elem_idx));
	}
	h5t_taginfo_t* ti = &eleminfo->ti[idx];

	tagset->num_values -= ti->val_dim;
	memmove (tagset->values + ti->val_idx,
		 tagset->values + ti->val_idx + ti->val_dim,
		 (tagset->num_values - idx) * sizeof (tagset->values[0]));

	// remove tag info for this entity
	memmove (ti,
		 ti + 1,
		 (eleminfo->num_tags-idx-1)*sizeof (ti[0]) );

	// we don't resize the eleminfo structure!!!
	H5_PRIV_FUNC_RETURN (H5_SUCCESS);
}

static h5_err_t
add_tag (
	h5t_tagset_t *tagset,
	const int idx,
	const h5_loc_idx_t face_id,
	const h5_loc_idx_t elem_idx,
	const size_t dim,
	void* val
	) {
	H5_PRIV_FUNC_ENTER (h5_err_t);
	// insert new taginfo
	h5t_tageleminfo_t* eleminfo = tagset->elems[elem_idx];
	TRY (eleminfo = tagset->elems[elem_idx] = h5_alloc (
		      tagset->elems[elem_idx],
		      sizeof (*eleminfo)
		      + eleminfo->num_tags * sizeof (eleminfo->ti[0])));
	h5t_taginfo_t* ti = &eleminfo->ti[idx];
	memmove (ti + 1,
		 ti,
		 (eleminfo->num_tags - idx) * sizeof(*ti));
	eleminfo->num_tags++;
	ti->face_id = (short)face_id;
	ti->val_dim = dim;

	// append values
	TRY (tagset->values = h5_alloc (
		      tagset->values,
		      (tagset->num_values+dim) * sizeof (*tagset->values)));
	memcpy (tagset->values + tagset->num_values,
		val,
		dim*sizeof (*tagset->values));
	ti->val_idx = tagset->num_values;
	tagset->num_values += dim;
	H5_PRIV_FUNC_RETURN (H5_SUCCESS);
}

static inline h5_err_t
overwrite_tag (
	h5_file_t* const f,
	h5t_tagset_t* tagset,
	const int idx,
	const h5_loc_idx_t elem_idx,
	void* val
	) {
	UNUSED_ARGUMENT (f);
	h5t_tageleminfo_t* eleminfo = tagset->elems[elem_idx];
	h5t_taginfo_t* ti = &eleminfo->ti[idx];

	memcpy (tagset->values + ti->val_idx,
		val,
		ti->val_dim * sizeof (*ti) );
	return H5_SUCCESS;
}

/*!
  Set tag for entity in current mesh.
 */
static h5_err_t
set_tag (
	h5_file_t* const f,
	h5t_tagset_t* tagset,
	const h5_loc_idx_t face_id,
	const h5_loc_idx_t elem_idx,
	const size_t dim,
	void* val
	) {
	H5_PRIV_FUNC_ENTER (h5_err_t);
	if (tagset->elems[elem_idx] == NULL) {
		TRY (tagset->elems[elem_idx] = h5_calloc (
			      1, sizeof (*tagset->elems)));
	}
	h5t_tageleminfo_t* eleminfo = tagset->elems[elem_idx];
	int i = find_face_id (eleminfo, face_id);
	h5t_taginfo_t* ti = eleminfo->ti + i;
	if (i >= 0 && dim != ti->val_dim) {
		/*
		  This is a very unusual case! So the processing can be
		  a bit more expensive ...
		 */
		TRY (remove_tag (tagset, face_id, elem_idx));
		TRY (add_tag (tagset, i, face_id, elem_idx, dim, val));
	} else if (i >= 0 && dim == ti->val_dim) {
		TRY (overwrite_tag (f, tagset, i, elem_idx, val));
	} else { // i < 0
		TRY (add_tag (tagset, -i-1, face_id, elem_idx, dim, val));
		tagset->num_entities++;
	}
	if (f->t->leaf_level < tagset->scope.min_level) {
		tagset->scope.min_level = f->t->leaf_level;
	}
	if (f->t->leaf_level > tagset->scope.max_level) {
		tagset->scope.max_level = f->t->leaf_level;
	}
	tagset->changed = 1;
	H5_PRIV_FUNC_RETURN (H5_SUCCESS);
}

/*
  Inheritance:
  - to coarser level only
  - if entity has tagged parrent, tag of parent
  - else: not tagged

  Parent of entity in tetrahedral grid:
  - tetrahedron: parent ID
  - triangles:
	face	child	parent face
	0	0	0
	1	0	1
	2	0	2
	3	0	-1
	0	1	0
	1	1	1
	2	1	-1
	3	1	3
	0	2	0
	1	2	-1
	2	2	2
	3	2	3
	0	3	-1	
	1	3	1
	2	3	2
	3	3	3
	0	4	-1
	1	4	1
	2	4	-1
	3	4	-1
	0	5	0
	1	5	-1
	2	5	-1
	3	5	-1
	0	6	-1
	1	6	-1
	2	6	-1
	3	6	3
	0	7	-1
	1	7	-1
	2	7	2
	3	7	-1
*/
int map_tri_to_parent_face[8][4] = {
	{0,1,2,-1},
	{0,1,-1,3},
	{0,-1,2,3},
	{-1,1,2,3},
	{-1,1,-1,-1},
	{0,-1,-1,-1},
	{-1,-1,-1,3},
	{-1,-1,2,-1}};
/*
  - edges:
	0	0-3	0
	1	0-3	1
	2	0-3	2
	3	0-3	3
	4	0-3	4
	5	0-3	5
	0-5	4-7	-1
*/
int map_edge_to_parent_face[8][6] = {
	
};
/*
  - vertex:
	0	0	0
	1-3	0	-1
	0	1	-1
	1	1	1
	2,3	1	-1
	0,1	2	-1
	2	2	2
	3	2	-1
	0-2	3	-1
	3	3	3
	0-3	4	-1
	0-3	5	-1
	0-3	6	-1
*/

static inline h5_err_t
get_idx_of_tagval (
	const h5t_tagset_t* tagset,
	const h5_loc_idx_t face_id,
	const h5_loc_idx_t elem_idx,
	int* taginfo_idx,
	h5_loc_idx_t* val_idx
	) {
	h5t_tageleminfo_t* eleminfo = tagset->elems[elem_idx];
	if (eleminfo == NULL) {
		return H5_NOK;
	}
	*taginfo_idx = find_face_id (eleminfo, face_id);
	if (*taginfo_idx < 0) {
		return H5_NOK; // not tagged
	}
	*val_idx = eleminfo->ti[*taginfo_idx].val_idx;
	return H5_SUCCESS;
}

static inline h5_ssize_t
get_tag (
	h5_file_t* const f,
	const h5t_tagset_t *tagset,
	const h5_loc_idx_t face_id,
	const h5_loc_idx_t elem_idx,
	size_t* const dim,
	void* const values
	) {
	if (f->t->leaf_level < tagset->scope.min_level) {
		return H5_NOK; // entity not tagged
	}
	h5_loc_idx_t val_idx = 0;
	int ti_idx = 0;
	h5_err_t h5err = get_idx_of_tagval (tagset, face_id, elem_idx,
					    &ti_idx, &val_idx);
	if (h5err < 0) {
		return h5err;
	}
	h5t_taginfo_t* ti = &tagset->elems[elem_idx]->ti[ti_idx];
	h5t_tagval_t* v = tagset->values;
	if (*dim > ti->val_dim || values == NULL) {
		*dim = ti->val_dim;
	}
	if (values != NULL) {
		memcpy (values, v + val_idx, *dim*sizeof(*v) );
	}
	return H5_SUCCESS;
}

/*!
  Set tag for entity in current mesh.

  \param[in]	f	file handle
  \param[in]	name	names of tagset
  \param[in]	id	local id of entity
  \param[in]	val	tag value
  \param[in]	size	size of value

  \return	H5_SUCCESS or error code
 */
h5_err_t
h5t_set_mtag_by_name (
	h5_file_t* const f,
	char name[],
	const h5_loc_id_t entity_id,
	const size_t dim,
	void* val
	) {
	H5_CORE_API_ENTER5 (h5_err_t,
			    "f=0x%p, name=\"%s\", entity_id=%lld, dim=%llu, val=0x%p",
			    f,
			    name,
			    (long long)entity_id,
			    (long long unsigned)dim,
			    val);
	h5t_tagset_t* tagset;
	TRY (h5t_open_mtagset (f, name, &tagset));
	h5_loc_idx_t face_id = h5tpriv_get_face_id (entity_id);
	h5_loc_idx_t elem_idx = h5tpriv_get_elem_idx (entity_id);
	H5_CORE_API_RETURN (set_tag (f, tagset, face_id, elem_idx, dim, val));
}
/*!
  Get tag for entity in given tagset.

  \param[in]	f	file handle
  \param[in]	name	names of tagset
  \param[in]	id	id of entity
  \param[out]	size	dimension of value
  \param[out]	val	tag value

  \return	H5_SUCCESS or error code
 */
h5_ssize_t
h5t_get_tag (
	h5_file_t* const f,
	const h5t_tagset_t* tagset,
	const h5_loc_id_t entity_id,
	size_t* const dim,
	void* const vals
	) {
	H5_CORE_API_ENTER5 (h5_err_t,
			    "f=0x%p, tagset=0x%p, entity_id=%lld, dim=0x%p, val=0x%p",
			    f,
			    tagset,
			    (long long)entity_id,
			    dim,
			    vals);
	h5_loc_idx_t face_id = h5tpriv_get_face_id (entity_id);
	h5_loc_idx_t elem_idx = h5tpriv_get_elem_idx (entity_id);
	H5_CORE_API_RETURN (get_tag (f, tagset, face_id, elem_idx, dim, vals));
}

/*!
  Get tag for entity in current mesh.

  \param[in]	f	file handle
  \param[in]	name	names of tagset
  \param[in]	id	id of entity
  \param[out]	size	dimension of value
  \param[out]	val	tag value

  \return	H5_SUCCESS or error code
 */
h5_ssize_t
h5t_get_mtag_by_name (
	h5_file_t* const f,
	const char name[],
	const h5_loc_id_t entity_id,
	size_t* dim,
	void* vals
	) {
	H5_CORE_API_ENTER5 (h5_err_t,
			    "f=0x%p, name=\"%s\", entity_id=%lld, dim=0x%p, val=0x%p",
			    f,
			    name,
			    (long long)entity_id,
			    dim,
			    vals);
	h5t_tagset_t* tagset;
	TRY (h5t_open_mtagset (f, name, &tagset));
	h5_loc_idx_t face_id = h5tpriv_get_face_id (entity_id);
	h5_loc_idx_t elem_idx = h5tpriv_get_elem_idx (entity_id);
	H5_CORE_API_RETURN (get_tag (f, tagset, face_id, elem_idx, dim, vals));
}

/*!
  Remove tag for entity in current mesh.

  \param[in]	f	file handle
  \param[in]	tagset	pointer to tagset
  \param[in]	id	id of entity
*/
h5_err_t
h5t_remove_mtag (
	h5_file_t* const f,
	h5t_tagset_t* tagset,
	const h5_loc_id_t entity_id
	) {
	UNUSED_ARGUMENT (f);
	H5_CORE_API_ENTER3 (h5_err_t,
			    "f=0x%p, tagset=0x%p, entity_id=%lld",
			    f,
			    tagset,
			    (long long)entity_id);
	h5_loc_idx_t face_id = h5tpriv_get_face_id (entity_id);
	h5_loc_idx_t elem_idx = h5tpriv_get_elem_idx (entity_id);
	H5_CORE_API_RETURN (remove_tag (tagset, face_id, elem_idx));
}

/*!
  Remove tag for entity in current mesh.

  \param[in]	f	file handle
  \param[in]	name	names of tagset
  \param[in]	id	id of entity
*/
h5_err_t
h5t_remove_mtag_by_name (
	h5_file_t* const f,
	const char name[],
	const h5_loc_id_t entity_id
	) {
	H5_CORE_API_ENTER3 (h5_err_t,
			    "f=0x%p, name=\"%s\", entity_id=%lld",
			    f,
			    name,
			    (long long)entity_id);
	h5t_tagset_t* tagset;
	TRY (h5t_open_mtagset (f, name, &tagset));
	h5_loc_idx_t face_id = h5tpriv_get_face_id (entity_id);
	h5_loc_idx_t elem_idx = h5tpriv_get_elem_idx (entity_id);
	H5_CORE_API_RETURN (remove_tag (tagset, face_id, elem_idx));
}

static hid_t
open_space_all (
	h5_file_t* const f,
	const hid_t dataset_id
	) {
	UNUSED_ARGUMENT (f);
	UNUSED_ARGUMENT (dataset_id);
	return H5S_ALL;
}

/*
  Write tagset to disk.
 */
static h5_err_t
write_tagset (
 	h5_file_t* const f,
	const hid_t loc_id,
	h5t_tagset_t* tagset
	) {
	H5_PRIV_FUNC_ENTER (h5_err_t);
	h5t_fdata_t* t = f->t;
	h5t_tageleminfo_t** eleminfos = tagset->elems;
	hid_t group_id;
	h5t_glb_tag_idx_t* elems = NULL;   // in memory dataset
	h5t_glb_tag_idx_t* elem = NULL;	   // reference an element in elems
	h5_loc_idx_t num_elems = 0;
	h5t_glb_tag_idx_t* entities = NULL;// in memory dataset
	h5t_glb_tag_idx_t* entity = NULL;  // reference an element in entities
	h5t_tagval_t* values = NULL;	   // in memory dataset

	h5_loc_idx_t elem_idx = 0;
	h5_loc_idx_t entity_idx = 0;
	h5_loc_idx_t val_idx = 0;

	if (t->num_leaf_levels <= 0) {
		H5_PRIV_FUNC_LEAVE (H5_SUCCESS); // nothing to do
	}
	num_elems = t->num_elems[t->num_leaf_levels-1];
	if (num_elems == 0 || tagset->num_entities == 0) {
		H5_PRIV_FUNC_LEAVE (H5_SUCCESS); // nothing to do
	}
	// allocate memory per element (plus 1)
	TRY (elems = h5_calloc (num_elems+1, sizeof(*elems)));
	elem = elems;

	// allocate memory per entity (plus 1)
	TRY (entities = h5_calloc (tagset->num_entities+1, sizeof(*entities)) );
	entity = entities;

	// allocate memory for all values
	TRY (values = h5_calloc (tagset->num_values, sizeof(*values)) );

	// build data structures in memory
	while (elem < elems+num_elems) {
		elem->eid = elem_idx;
		elem->idx = entity_idx;
		h5t_tageleminfo_t* eleminfo = *eleminfos;

		// loop over tagged faces of this element
		int ti_idx;
		for (ti_idx = 0; eleminfo && ti_idx < eleminfo->num_tags; ti_idx++) {
			h5t_taginfo_t* ti = eleminfo->ti+ti_idx;
			h5_glb_idx_t glb_elem_idx = h5tpriv_get_loc_elem_glb_idx (f, elem_idx);
			entity->eid = h5tpriv_build_entity_id2 (
				(h5_glb_id_t)ti->face_id, glb_elem_idx);
			entity->idx = val_idx;

			// copy values 
			memcpy (values + val_idx,
				&tagset->values[ti->val_idx],
				ti->val_dim * sizeof (*values));
			val_idx += ti->val_dim;
			entity_idx++;
			entity++;
		}
		elem_idx++;
		eleminfos++;
		elem++;
	}
	elem->eid = -1;		// last entry
	tagset->num_entities = elem->idx = entity_idx;
	entity->eid = -1;
	tagset->num_values = entity->idx = val_idx;

	// write data
	TRY (group_id = h5priv_open_group (f, loc_id, tagset->name));
	h5_dsinfo_t dsinfo;
	memset (&dsinfo, 0, sizeof(dsinfo));
	dsinfo.rank = 1;
	dsinfo.max_dims[0] = H5S_UNLIMITED;
	dsinfo.chunk_dims[0] = 4096;
	dsinfo.access_prop = H5P_DEFAULT;

	strcpy (dsinfo.name, "elems");
	dsinfo.dims[0] = num_elems + 1;
	dsinfo.type_id = t->dtypes.h5t_glb_tag_idx_t;
	TRY (dsinfo.create_prop = hdf5_create_property (H5P_DATASET_CREATE));
	TRY (hdf5_set_chunk_property (dsinfo.create_prop, dsinfo.rank,
					      dsinfo.chunk_dims));
	
	TRY (h5priv_write_dataset_by_name (
		      f,
		      group_id,
		      &dsinfo,
		      open_space_all, open_space_all,
		      elems));

	strcpy (dsinfo.name, "entities");
	dsinfo.dims[0] = tagset->num_entities + 1;
	
	TRY (h5priv_write_dataset_by_name (
		     f,
		     group_id,
		     &dsinfo,
		     open_space_all, open_space_all,
		     entities));

	strcpy (dsinfo.name, "values");
	dsinfo.dims[0] = tagset->num_values;
	dsinfo.type_id = t->dtypes.h5_int64_t;
	
	TRY (h5priv_write_dataset_by_name (
		     f,
		     group_id,
		     &dsinfo,
		     open_space_all, open_space_all,
		     values));
	h5_int64_t scope = tagset->scope.min_level;
	TRY (h5priv_write_attrib (group_id, "__scope_min__", H5_INT64_T, &scope, 1));
	scope = tagset->scope.max_level;
	TRY (h5priv_write_attrib (group_id, "__scope_max__", H5_INT64_T, &scope, 1));

	TRY (hdf5_close_group (group_id));
	TRY (h5_free (elems));
	TRY (h5_free (entities));
	TRY (h5_free (values));
	H5_PRIV_FUNC_RETURN (H5_SUCCESS);
}

/*
  Store given tag container. Write only changed tag-sets.
 */
static h5_err_t
write_container (
	h5_file_t* const f,
	h5t_tagcontainer_t* ctn
	) {
	H5_PRIV_FUNC_ENTER (h5_err_t);
	size_t idx;
	for (idx = 0; idx < ctn->num_sets; idx++) {
		void *__retval;
		TRY (h5priv_hsearch (
			     &ctn->names[idx],
			     H5_FIND,
			     &__retval,
			     &ctn->sets));
		h5t_tagset_t *tagset = (h5t_tagset_t*)__retval;
		if (tagset->changed) {
			TRY(write_tagset (
				     f,
				     ctn->group_id,
				     tagset));
		}
	}
	H5_PRIV_FUNC_RETURN (H5_SUCCESS);
}


/*
  Store mesh tags container
 */
h5_err_t
h5tpriv_write_mtags (
	h5_file_t* const f
	) {
	H5_PRIV_API_ENTER1 (h5_err_t, "f=0x%p", f);
	h5t_fdata_t* t = f->t;
	TRY (t->mtags.group_id = h5priv_open_group (f, t->mesh_gid, "Tags"));
	TRY (write_container (f, &f->t->mtags));
	TRY (hdf5_close_group (t->mtags.group_id));
	H5_PRIV_API_RETURN (H5_SUCCESS);
}

static h5_err_t
read_tagset (
	h5_file_t* const f,
	h5t_tagcontainer_t* ctn,
	hsize_t idx
	) {
	H5_PRIV_FUNC_ENTER (h5_err_t);
	h5t_fdata_t* t = f->t;
	hid_t loc_id = ctn->group_id;
	char* name;
	h5_id_t type;
	hid_t group_id;
	hid_t dset_id;
	ssize_t ssize;
	TRY ((ssize = hdf5_get_objname_by_idx (loc_id, idx, NULL, 0)));
	TRY ((name = h5_calloc (1, ++ssize)));
	TRY (hdf5_get_objname_by_idx (loc_id, idx, name, ssize));
	TRY (group_id = hdf5_open_group (loc_id, name));

	// read datasets: "elems", "entities" and "values"
	h5t_glb_tag_idx_t* elems;
	size_t num_elems = 0;

	TRY (dset_id = hdf5_open_dataset (group_id, "elems"));
	TRY (num_elems = hdf5_get_npoints_of_dataset (dset_id));
	TRY (elems = h5_calloc (num_elems, sizeof(*elems)));
	h5_dsinfo_t dsinfo;
	memset (&dsinfo, 0, sizeof (dsinfo));
	dsinfo.type_id = t->dtypes.h5t_glb_tag_idx_t;
	TRY (h5priv_read_dataset (
		     f,
		     dset_id,
		     &dsinfo,
		     open_space_all, open_space_all,
		      elems));
	TRY (hdf5_close_dataset (dset_id));
	num_elems--;

	h5t_glb_tag_idx_t* entities;
	size_t ent_idx = 0;
	size_t num_entities = 0;
	TRY (dset_id = hdf5_open_dataset (group_id, "entities"));
	TRY (num_entities = hdf5_get_npoints_of_dataset (dset_id));
	TRY (entities = h5_calloc (num_entities, sizeof(*entities)));
	TRY (h5priv_read_dataset (
		     f,
		     dset_id,
		     &dsinfo,
		     open_space_all, open_space_all,
		     entities));
	TRY (hdf5_close_dataset (dset_id));
	num_entities--;

	h5_int64_t* vals;
	size_t num_vals = 0;
	TRY (dset_id = hdf5_open_dataset (group_id, "values"));
	TRY (num_vals = hdf5_get_npoints_of_dataset (dset_id));
	TRY (vals = h5_calloc (num_vals, sizeof (*vals)));
	TRY (dsinfo.type_id = hdf5_get_dataset_type (dset_id));
	TRY (h5priv_read_dataset (
		      f,
		      dset_id,
		      &dsinfo,
		      open_space_all, open_space_all,
		      vals));
	TRY (hdf5_close_dataset (dset_id ));
	type = h5_normalize_h5_type (dsinfo.type_id);
	/*
	  add tagset and set values
	*/
	h5t_tagset_t* tagset;
	TRY (add_tagset (f, ctn, name, type, &tagset) );

	h5_int64_t scope;
	TRY (h5priv_read_attrib (group_id, "__scope_min__", H5_INT64_T, &scope));
	tagset->scope.min_level = scope;
	TRY (h5priv_read_attrib (group_id, "__scope_max__", H5_INT64_T, &scope));
	tagset->scope.max_level = scope;

	for (ent_idx = 0; ent_idx < num_entities; ent_idx++) {
		h5t_glb_tag_idx_t *entity = &entities[ent_idx];
		size_t dim = (entity+1)->idx - entity->idx;
		// map global face id and global element idx to local
		h5_loc_idx_t face_id;
		h5_loc_idx_t elem_idx;
		h5_glb_idx_t glb_elem_idx = h5tpriv_get_elem_idx (entity->eid);
		TRY (elem_idx = h5t_map_glb_elem_idx2loc (f, glb_elem_idx));
		face_id = h5tpriv_get_face_id (entity->eid);
		TRY (set_tag (
			     f,
			     tagset,
			     face_id,
			     elem_idx,
			     dim,
			     &vals[entity->idx]));
	}
	TRY (h5_free (name));
	H5_PRIV_FUNC_RETURN (H5_SUCCESS);
}

h5_err_t
h5tpriv_read_tag_container (
	h5_file_t* const f,
	h5t_tagcontainer_t* ctn
	) {
	H5_PRIV_API_ENTER2 (h5_err_t, "f=0x%p, ctn=0x%p", f, ctn);
	size_t num_sets;
	TRY (num_sets = hdf5_get_num_objs_in_group (ctn->group_id));
	hsize_t idx;
	
	for (idx = 0; idx < num_sets; idx++) {
		TRY (read_tagset (f, ctn, idx));
	}

	H5_PRIV_API_RETURN (H5_SUCCESS);
}

/*
  Get m-tagset of given entity. Return the number of tagsets as result
  of the function and the names in the pointer array \c name. To query the
  number of tagsets, use \c NULL as value of \c name. If the actual number
  of tagset is greater than \c dim, the first \c dim names found will be
  returned.

  \remark
  This functions performs with O(n) where n is the number of tagsets.
  
  \param[in]	f		file handle
  \param[in]	entity_id	ID of entity we want to know the set tags
  \param[out]	names		Array of ptr to tagset names
  \param[in]	dim		dimension of array

  \return	number of tagsets
 */
static h5_ssize_t
get_tagset_names_of_entity (
	h5t_tagcontainer_t* ctn,
	h5_loc_id_t entity_id,
	char* names[],
	h5_size_t dim
	) {
	H5_PRIV_FUNC_ENTER (h5_ssize_t);
	size_t idx;
	size_t _dim = 0;
	h5_loc_idx_t face_id = h5tpriv_get_face_id (entity_id);
	h5_loc_idx_t elem_idx = h5tpriv_get_elem_idx (entity_id);

	for (idx = 0; idx < ctn->num_sets; idx++) {
		void* __retval;
		TRY (h5priv_hsearch (
			     &ctn->names[idx],
			     H5_FIND,
			     &__retval,
			     &ctn->sets));
		h5t_tagset_t* tagset = (h5t_tagset_t*)__retval;
		int tagset_idx = 0;
		h5_loc_idx_t val_idx;
		if (0 < get_idx_of_tagval (tagset,
					   face_id, elem_idx,
					   &tagset_idx, &val_idx) ) {
			continue; // not tagged
		}
		if (names != NULL && _dim <= dim) {
			names[_dim] = ctn->names[idx];
		}
		_dim++;
	}
	H5_PRIV_FUNC_RETURN (_dim);
}

/*
  Return tag names for given entity.
 */
h5_ssize_t
h5t_get_mtagset_names_of_entity (
	h5_file_t* const f,
	const h5_loc_id_t entity_id,
	char* names[],
	const h5_size_t dim
	) {
	H5_CORE_API_ENTER4 (h5_err_t,
			    "f=0x%p, entity_id=%lld, names=0x%p, dim=%llu",
			    f,
			    (long long)entity_id,
			    names,
			    (long long unsigned)dim);
	H5_CORE_API_RETURN (
		get_tagset_names_of_entity (&f->t->mtags, entity_id, names, dim));
}
