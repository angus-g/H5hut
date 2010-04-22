#include <stdlib.h>
#include <string.h>
#include <hdf5.h>

#include "h5core/h5_core.h"
#include "h5_core_private.h"

/*
  To discuss:
  - read tagsets on demand
 */

/*
  Initialize a tag container
 */

static h5_err_t
_init_container (
	h5_file_t * const f,
	const size_t ntags,
	h5t_tagcontainer_t * ctn
	) {
	ctn->names = h5priv_calloc ( f, ntags, sizeof(char*) );
	TRY ( h5priv_hcreate_string_keyed ( f, ntags, &ctn->sets ) );
	return H5_SUCCESS;
}
/*
  Initialize tag container for current mesh and level.
 */
h5_err_t
h5tpriv_init_mtagsets (
	h5_file_t * const f,
	size_t ntags
	) {
	return _init_container ( f, ntags, &f->t->mtags );
}

/*
  Release all tag values for a specific element 
 */
static h5_err_t
_release_tagvals_of_elem (
 	h5_file_t * const f,
	h5t_tagsel_t *el_vals
	) {
	unsigned int i = 0;
	for ( i = 0; i < el_vals->size; i++ ) {
		TRY ( h5priv_free ( f, el_vals->valp[i] ) );
	}
	return H5_SUCCESS;
}

/*
  Release a tag-set
 */
static h5_err_t
_release_tagset (
 	h5_file_t * const f,
	const void *__set
	) {
	H5T_Tagset *set = *(H5T_Tagset**)__set;
	unsigned int i;
	for ( i = 0; i < set->num_elems; i++ ) {
		if ( set->elems[i] != NULL ) {
			TRY ( _release_tagvals_of_elem ( f, set->elems[i] ) );
		}
	}
	TRY ( h5priv_free ( f, set->name ) );
	TRY ( h5priv_free ( f, set ) );
	return H5_SUCCESS;
}

/*
  Release all sets in given container
 */
static h5_err_t
_release_container (
	h5_file_t * const f,
	h5t_tagcontainer_t * ctn
	) {
	TRY ( h5priv_hwalk ( f, &ctn->sets, _release_tagset ) ); 
	TRY ( h5priv_free ( f, ctn->names ) );

	return H5_SUCCESS;
}

/*
  Release mesh tag-sets
*/
h5_err_t
h5tpriv_release_tags (
	h5_file_t * const f
	) {
	h5t_fdata_t *t = f->t;
	TRY ( _release_container ( f, &t->mtags ) );

	return H5_SUCCESS;
}

/*!
  Add tag to current mesh.

  \param[in]	f	File handle
  \param[in]	name	name of tag
  \param[in]	type	type
  \param[out]	tag	pointer to tag object if != 0

  \return	H5_SUCCESS or error code
*/
h5_err_t
h5tpriv_add_mtagset (
	h5_file_t * const f,
	char * name,
	h5_id_t type,
	H5T_Tagset **rtagset
	) {
	h5t_fdata_t *t = f->t;

	/*
	  Initialize data structure for m-tagsets, if not already done.
	*/
	if ( t->mtags.names == NULL ) {
		TRY ( h5tpriv_init_mtagsets ( f, 521 ) );
	}
	/*
	  ToDo: Resize!
	*/

	if ( ( name == NULL ) || ( name[0] == '\0' ) ) {
		return h5_error (
			f,
			H5_ERR_INVAL,
			"Invalid name" );
	}
	/*
	  @TODO@ check:
	  - tagset with name doesn't already exist
	*/
	if ( type != H5_INT64_T && type != H5_FLOAT64_T ) {
		return h5_error ( f, H5_ERR_INVAL,
			"Unsupported data type." );
	}
	/*
	  create new tagset
	*/
	H5T_Tagset *tagset = NULL;
	size_t size = (t->num_elems[t->num_levels-1] - 1) * sizeof(*tagset->elems)
		+ sizeof(*tagset);
	TRY ( (tagset = h5priv_calloc ( f, 1, size ) ) );

	TRY ( ( tagset->name = h5priv_calloc ( f, 1, strlen(name)+1 ) ) );
	strcpy ( tagset->name, name );
	tagset->type = type;
	tagset->num_elems = t->num_elems[t->num_levels-1];
	/*
	  add tagset to hash of tagsets
	 */
	void *__retval = NULL;
	TRY ( h5priv_hsearch ( f, tagset, H5_ENTER, &__retval, &t->mtags.sets ) );

	t->mtags.changed = 1;
	t->mtags.names[t->mtags.num_sets] = tagset->name;
	t->mtags.num_sets++;

	if ( rtagset != 0 ) *rtagset = tagset;
	return H5_SUCCESS;
}

h5_err_t
h5t_add_mtagset (
	h5_file_t * const f,
	char * name,
	h5_id_t type
	) {
	return h5tpriv_add_mtagset ( f, name, type, 0 );
}


/*!
  Remove a tagset from the current mesh.

  \param[in]	f	file handle
  \param[in]	name	name of tagset to remove

  \return	H5_SUCCESS or error code
 */
static h5_err_t
_remove_tagset (
	h5_file_t *const f,
	h5t_tagcontainer_t *ctn,
	const char name[]
	) {
	h5t_fdata_t *t = f->t;

	/*
	  remove tagset with NAME from m-tagsets dictionary
	*/
	void *__retval = NULL;
	TRY ( h5priv_hsearch ( f, &name, H5_REMOVE, &__retval, &ctn->sets ) ); 
	H5T_Tagset *tagset = (H5T_Tagset*)__retval;
	if ( tagset == NULL ) return H5_SUCCESS;

	size_t el_idx, i;
	size_t num_elems = t->num_elems[t->num_levels-1];
	for ( el_idx = 0; el_idx < num_elems; el_idx++ ) {
		h5t_tagsel_t *tags_of_elem = tagset->elems[el_idx];
		/*
		  release all m-tags for this element
		*/
		for ( i = 0; i < tags_of_elem->size; i++ ) {
			TRY ( h5priv_free ( f, tags_of_elem->valp[i] ) );
		}
		/*
		  release m-tag object for this element 
		 */
		TRY ( h5priv_free ( f, tags_of_elem ) );
	}
	/*
	  release tagset object
	 */
	TRY ( h5priv_free ( f, tagset ) );
	/*
	  remove HDF5 datasets and group for this tagset
	 */
	hid_t loc_id;
	TRY ( loc_id = h5priv_open_group ( f, ctn->group_id, name ) );
	TRY ( h5priv_delete_hdf5_link ( f, loc_id, "elems", H5P_DEFAULT ) );
	TRY ( h5priv_delete_hdf5_link ( f, loc_id, "entities", H5P_DEFAULT ) );
	TRY ( h5priv_delete_hdf5_link ( f, loc_id, "values", H5P_DEFAULT ) );
	TRY ( h5priv_close_hdf5_group ( f, loc_id ) );
	TRY ( h5priv_delete_hdf5_link ( f, ctn->group_id, name, H5P_DEFAULT ) );

	return H5_SUCCESS;
}

/*!
  Remove a tagset from the current mesh.

  \param[in]	f	file handle
  \param[in]	name	name of tagset to remove

  \return	H5_SUCCESS or error code
 */
h5_err_t
h5t_remove_mtagset (
	h5_file_t *const f,
	const char name[]
	) {
	h5t_fdata_t *t = f->t;

	TRY ( t->mtags.group_id = h5priv_open_group ( f, t->mesh_gid, "Tags" ) );
	TRY ( _remove_tagset ( f, &t->mtags, name ) );
	TRY ( h5priv_close_hdf5_group ( f, t->mtags.group_id ) );
	return H5_SUCCESS;
}

h5_ssize_t
h5t_get_num_mtagsets (
	h5_file_t *const f
	) {
	h5t_fdata_t *t = f->t;
	return t->mtags.num_sets;
}

/*!
  Get available tagset in current mesh.

  \param[in]	f	file handle
  \param[out]	names	names of available tagsets

  \return	Number of mesh tag-sets
 */
h5_ssize_t
h5t_get_mtagsets (
	h5_file_t *const f,
	char **names[]
	) {
	h5t_fdata_t *t = f->t;
	*names = t->mtags.names; 
	return t->mtags.num_sets;
}

h5_err_t
h5t_open_mtagset (
	h5_file_t *const f,
	const char *name,
	H5T_Tagset **retval
	) {
	h5t_fdata_t *t = f->t;
	void *__retval = NULL;
	TRY ( h5priv_hsearch ( f, &name, H5_FIND, &__retval,
			    &t->mtags.sets ) ); 
	*retval = (H5T_Tagset*)__retval;
	return H5_SUCCESS;
}

/*!
  Get type of tagset in current mesh.

  \param[in]	f	file handle
  \param[in]	name	name of tagset

  \return	type or error code
 */
h5_id_t
h5t_get_mtagset_type_by_name (
	h5_file_t *const f,
	char *name
	) {
	H5T_Tagset *tagset;
	TRY ( h5t_open_mtagset ( f, name, &tagset ) );
	return tagset->type;
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
	h5_file_t *const f,
	h5_id_t idx,
	char **name,
	h5_id_t *type
	) {
	h5t_fdata_t *t = f->t;
	*name = t->mtags.names[idx];
	void *__retval = NULL;
	h5priv_hsearch ( f, t->mtags.names[idx], H5_FIND, &__retval,
		      &t->mtags.sets );
	H5T_Tagset *retval = (H5T_Tagset*)__retval;
	*type = retval->type;
	return H5_SUCCESS;
}

/*!
  Set tag for entity in current mesh.

  \param[in]	f	file handle
  \param[in]	tagset	pointer to tagset
  \param[in]	id	local id of entity
  \param[in]	size	size of value
  \param[in]	val	tag value

  \return	H5_SUCCESS or error code
 */
h5_err_t
h5tpriv_set_mtag (
	h5_file_t *const f,
	H5T_Tagset *tagset,
	const h5_id_t id,
	const size_t size,
	void *val 
	) {
	size_t offs[5] = { 14, 0, 4, 10, 14 };
	h5_id_t el_idx = h5tpriv_get_elem_idx ( id );
	h5_id_t eoe_id = h5tpriv_get_face_id ( id );
	h5_id_t type_id = h5tpriv_get_entity_type ( id );
	if ( tagset->elems[el_idx] == NULL ) {
		/*
		  alloc new structure to store all tags for this element
		  and the given name 
		*/
		TRY ( ( tagset->elems[el_idx] = h5priv_calloc (
			      f,
			      1,
			      sizeof(*tagset->elems[el_idx] ) ) ) );
		memset ( tagset->elems[el_idx]->idx, -1,
			 sizeof(tagset->elems[el_idx]->idx) );
	}
	h5t_tagsel_t *tagselem = tagset->elems[el_idx];
	size_t i = offs[type_id]+eoe_id;

	/*
	  new tag value?
	*/
	if ( tagselem->idx[i] < 0 ) {
		/*
		  Set new index of pointer to value
		  and increment number of values.
		 */
		size_t k = tagselem->idx[i] = tagselem->size++;
		/*
		  We have to resize (actually not, if this is the first value,
		  but to keep things simple ...)
		*/
		size_t num_bytes = sizeof(*tagselem)
			+ (tagselem->size-1) * sizeof(void*);
		TRY ( ( tagset->elems[el_idx] = h5priv_alloc (
				f, tagselem, num_bytes ) ) );
		tagselem = tagset->elems[el_idx];
		/*
		  we will allocate the memory later.
		 */
		tagselem->valp[k] = NULL;
	}

	/*
	  (re-)allocate memory for value and assign
	*/
	size_t num_bytes = (size-1) * sizeof(h5_float64_t) + sizeof(h5t_tagval_t);
	size_t k = tagselem->idx[i];
	TRY ( ( tagselem->valp[k] = h5priv_alloc (
			f, tagselem->valp[k], num_bytes ) ) );
	h5t_tagval_t *tagval = tagselem->valp[k];
	tagval->size = size;
	memcpy ( &tagval->vals, val, size*sizeof(h5_float64_t) );
	tagset->changed = 1;
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
	h5_file_t *const f,
	char name[],
	const h5_id_t id,
	const size_t size,
	void *val
	) {
	H5T_Tagset *tagset;
	TRY ( h5t_open_mtagset ( f, name, &tagset ) );
	return h5tpriv_set_mtag ( f, tagset, id, size, val );
}

static h5_err_t
_get_tag_valp (
	h5_file_t *const f,
	const H5T_Tagset *tagset,
	const h5_id_t entity_id,
	h5t_tagval_t **valp
	) {
	h5_id_t el_idx = h5tpriv_get_elem_idx ( entity_id );
	h5_id_t subentity_id = h5tpriv_get_face_id ( entity_id );
	h5_id_t type_id = h5tpriv_get_entity_type ( entity_id );

	if ( tagset->elems[el_idx] == NULL ) {
		return H5_NOK; /* no tags for this element */
	}
	h5t_tagsel_t *tagselem = tagset->elems[el_idx];
	size_t offs[5] = { 14, 0, 4, 10, 14 };
	size_t i = offs[type_id]+subentity_id;

	if ( tagselem->idx[i] < 0 ) {
		return H5_NOK; /* no value set for this subentity */
	}
	size_t k = tagselem->idx[i];
	valp = &tagselem->valp[k];

	return H5_SUCCESS;
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
	h5_file_t *const f,
	const H5T_Tagset *tagset,
	const h5_id_t entity_id,
	size_t *dim,
	void *vals
	) {
	h5t_tagval_t *tagval;
	TRY ( _get_tag_valp ( f, tagset, entity_id, &tagval ) );

	if ( (*dim > tagval->size) || (vals == NULL) ) {
		*dim = tagval->size;
	}
	if ( vals != NULL ) {
		memcpy ( vals, &tagval->vals, *dim * sizeof(h5_float64_t) );
	}
	return tagval->size;
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
	const h5_id_t id,
	size_t* dim,
	void* vals
	) {
	H5T_Tagset* tagset;
	TRY ( h5t_open_mtagset ( f, name, &tagset ) );
	return h5t_get_tag ( f, tagset, id, dim, vals );
}

/*!
  Remove tag for entity in current mesh.

  \param[in]	f	file handle
  \param[in]	tagset	pointer to tagset
  \param[in]	id	id of entity
*/
h5_err_t
h5t_remove_mtag (
	h5_file_t *const f,
	H5T_Tagset *tagset,
	const h5_id_t id
	) {
	h5_id_t el_idx = h5tpriv_get_elem_idx ( id );
	h5_id_t subentity_id = h5tpriv_get_face_id ( id );
	h5_id_t type_id = h5tpriv_get_entity_type ( id );

	if ( tagset->elems[el_idx] == NULL ) {
		return H5_SUCCESS; /* no tags for this element */
	}
	h5t_tagsel_t *tagselem = tagset->elems[el_idx];
	size_t offs[5] = { 14, 0, 4, 10, 14 };
	size_t i = offs[type_id]+subentity_id;

	if ( tagselem->idx[i] < 0 ) {
		return H5_SUCCESS; /* no value set for this subentity */
	}
	size_t k = tagselem->idx[i];
	h5t_tagval_t *tagval = tagselem->valp[k];
	TRY ( h5priv_free ( f, tagval ) );
	/*
	  remove from array
	 */
	size_t n = ( tagselem->size-k-1) * sizeof(tagselem->valp[0]);
	memmove ( tagselem->valp[k], tagselem->valp[k+1], n );
	tagselem->size--;
	/*
	  adapt indices
	 */
	for ( i = 0; i < tagselem->size; i++ ) {
		if ( tagselem->idx[i] > k ) {
			tagselem->idx[i]--;
		}
	}
	return H5_SUCCESS;
}

/*!
  Remove tag for entity in current mesh.

  \param[in]	f	file handle
  \param[in]	name	names of tagset
  \param[in]	id	id of entity
*/
h5_err_t
h5t_remove_mtag_by_name (
	h5_file_t *const f,
	const char name[],
	const h5_id_t id
	) {
	H5T_Tagset *tagset;
	TRY ( h5t_open_mtagset ( f, name, &tagset ) );
	return h5t_remove_mtag ( f, tagset, id );
}

static hid_t
_open_space_all (
	h5_file_t * const f,
	hid_t dataset_id
	) {
	return H5S_ALL;
}

/*
  Store given tagset.
 */
static h5_err_t
_write_tagset (
 	h5_file_t * const f,
	hid_t loc_id,
	H5T_Tagset *tagset
	) {
	h5t_fdata_t *t = f->t;
	hid_t group_id;
	/*
	  alloc memory
	 */
	h5t_tag_idx_t *elems;
	size_t el_idx;
	size_t num_elems = t->num_elems[t->num_levels-1];
	TRY ( ( elems = h5priv_calloc (
			f, num_elems+1, sizeof(*elems) ) ) );

	h5t_tag_idx_t *entities;
	size_t ent_idx = 0;
	size_t num_entities = 0;
	size_t max_entities = num_elems+1;
	TRY ( ( entities = h5priv_calloc (
			f, max_entities, sizeof(*entities) ) ) );

	h5_int64_t *vals;
	size_t val_idx = 0;
	size_t num_vals = 0;
	size_t max_vals = num_elems;
	TRY ( ( vals = h5priv_calloc (
			f, max_vals, sizeof(*vals) ) ) );

	h5_id_t tmap[15] = { H5T_ETYPE_VERTEX, H5T_ETYPE_VERTEX,
			     H5T_ETYPE_VERTEX, H5T_ETYPE_VERTEX,
			     H5T_ETYPE_EDGE, H5T_ETYPE_EDGE,
			     H5T_ETYPE_EDGE, H5T_ETYPE_EDGE,
			     H5T_ETYPE_EDGE, H5T_ETYPE_EDGE,
			     H5T_ETYPE_TRIANGLE, H5T_ETYPE_TRIANGLE,
			     H5T_ETYPE_TRIANGLE, H5T_ETYPE_TRIANGLE,
			     H5T_ETYPE_TET };
	/* sub-entity indices */
	h5_id_t smap[15] = { 0, 1, 2, 3,	/* vertices */
			     0, 1, 2, 3, 4, 5,	/* edges */
			     0, 1, 2, 3,	/* triangles */
			     0 };		/* tetrahedron */
	/*
	  build data structures in memory
	 */
	for ( el_idx = 0; el_idx < num_elems; el_idx++ ) {
		h5t_tagsel_t *tags_of_elem = tagset->elems[el_idx];
		size_t num_subentities = 0;
		h5_id_t start_idx_entities = ent_idx;
		size_t i;
		for ( i = 0; i < tags_of_elem->size; i++ ) {
			int k = tags_of_elem->idx[i];
			if ( k == -1 ) continue;
			h5t_tagval_t *valp = tags_of_elem->valp[k];
			num_subentities++;
			/*
			  append value to array of values
			*/
			if ( val_idx + valp->size > max_vals ) {
				max_vals += num_elems;
				TRY ( ( vals = h5priv_alloc (
						f, vals,
						max_vals*sizeof(*vals) ) ) );
			}
			memcpy (
				vals+val_idx,
				&valp->vals[0].i,
				valp->size*sizeof(*vals) );
			
			/*
			  append entity descriptor
			*/
			if ( ent_idx >= max_entities ) {
				max_entities += num_elems;
				TRY ( ( entities = h5priv_alloc (
						f, entities,
						max_entities*sizeof(*entities) )
					      ) );
			}
			h5_id_t type_id = tmap[i];
			h5_id_t sentity_id = smap[i];
			entities[ent_idx].eid = h5tpriv_build_id (
				type_id, sentity_id, el_idx );
			entities[ent_idx].idx = val_idx;
			val_idx += valp->size;
			ent_idx++;
			
		}
		/* append element descriptor */
		elems[el_idx].eid = el_idx;
		elems[el_idx].idx = start_idx_entities;
		start_idx_entities += num_subentities;
	}
	num_entities = ent_idx;
	num_vals = val_idx;

	elems[num_elems].eid = -1;
	elems[num_elems].idx = num_entities;
	entities[ent_idx].eid = -1;
	entities[ent_idx].idx = num_vals;

	/*
	  write data
	 */
	TRY ( group_id = h5priv_open_group ( f, loc_id, tagset->name ) );
	h5_dsinfo_t dsinfo;
	memset ( &dsinfo, 0, sizeof(dsinfo) );
	dsinfo.rank = 1;
	dsinfo.max_dims[0] = H5S_UNLIMITED;
	dsinfo.chunk_dims[0] = 4096;
	dsinfo.access_prop = H5P_DEFAULT;

	strcpy ( dsinfo.name, "elems" );
	dsinfo.dims[0] = num_elems + 1;
	dsinfo.type_id = t->dtypes.h5t_tag_idx_t;
	TRY( dsinfo.create_prop = h5priv_create_hdf5_property ( f,
							H5P_DATASET_CREATE ) );
	TRY( h5priv_set_hdf5_chunk_property ( f, dsinfo.create_prop, dsinfo.rank,
				      dsinfo.chunk_dims ) );
	
	TRY ( h5priv_write_dataset_by_name (
		      f,
		      group_id,
		      &dsinfo,
		      _open_space_all, _open_space_all,
		      elems ) );

	strcpy ( dsinfo.name, "entities" );
	dsinfo.dims[0] = num_entities + 1;
	
	TRY ( h5priv_write_dataset_by_name (
		      f,
		      group_id,
		      &dsinfo,
		      _open_space_all, _open_space_all,
		      entities ) );

	strcpy ( dsinfo.name, "values" );
	dsinfo.dims[0] = num_vals;
	dsinfo.type_id = t->dtypes.h5_int64_t;
	
	TRY ( h5priv_write_dataset_by_name (
		      f,
		      group_id,
		      &dsinfo,
		      _open_space_all, _open_space_all,
		      vals ) );

	return H5_SUCCESS;
}

/*
  Store given tag container. Write only changed tag-sets.
 */
static h5_err_t
_write_container (
	h5_file_t * const f,
	h5t_tagcontainer_t *ctn
	) {
	size_t idx;
	for ( idx = 0; idx < ctn->num_sets; idx++ ) {
		void *__retval;
		TRY ( h5priv_hsearch ( f,
				    &ctn->names[idx],
				    H5_FIND,
				    &__retval,
				    &ctn->sets ) );
		H5T_Tagset *tagset = (H5T_Tagset*)__retval;
		if ( tagset->changed ) {
			TRY ( _write_tagset (
				      f,
				      ctn->group_id,
				      tagset ) );
		}
	}
	return H5_SUCCESS;
}


/*
  Store mesh tags container
 */
h5_err_t
h5tpriv_write_mtags (
	h5_file_t *const f
	) {
	h5t_fdata_t *t = f->t;
	TRY ( t->mtags.group_id = h5priv_open_group ( f, t->mesh_gid, "Tags" ) );
	TRY ( _write_container ( f, &f->t->mtags ) );
	TRY ( h5priv_close_hdf5_group ( f, t->mtags.group_id ) );
	return H5_SUCCESS;
}

static h5_err_t
_read_tagset (
	h5_file_t * const f,
	hid_t loc_id,
	hsize_t idx
	) {
	h5t_fdata_t *t = f->t;
	char *name;
	h5_id_t type;
	hid_t group_id;
	hid_t dset_id;
	TRY ( h5priv_get_objname_by_idx_in_hdf5_group ( f, loc_id, idx, &name ) );
	TRY ( group_id = h5priv_open_hdf5_group ( f, loc_id, name ) );

	/*
	  read datasets: "elems", "entities" and "values"
	*/
	h5t_tag_idx_t *elems;
	size_t num_elems = 0;

	TRY ( dset_id = h5priv_open_hdf5_dataset ( f, group_id, "elems" ) );
	TRY ( num_elems = h5priv_get_npoints_of_hdf5_dataset ( f, dset_id ) );
	TRY ( elems = h5priv_calloc ( f, num_elems, sizeof(*elems) ) );
	h5_dsinfo_t dsinfo;
	memset ( &dsinfo, 0, sizeof(dsinfo) );
	dsinfo.type_id = t->dtypes.h5t_tag_idx_t;
	TRY ( h5priv_read_dataset (
		      f,
		      dset_id,
		      &dsinfo,
		      _open_space_all, _open_space_all,
		      elems ) );
	TRY ( h5priv_close_hdf5_dataset ( f, dset_id ) );
	num_elems--;

	h5t_tag_idx_t *entities;
	size_t ent_idx = 0;
	size_t num_entities = 0;
	TRY ( dset_id = h5priv_open_hdf5_dataset ( f, group_id, "entities" ) );
	TRY ( num_entities = h5priv_get_npoints_of_hdf5_dataset ( f, dset_id ) );
	TRY ( entities = h5priv_calloc ( f, num_entities, sizeof(*entities) ) );
	TRY ( h5priv_read_dataset (
		      f,
		      dset_id,
		      &dsinfo,
		      _open_space_all, _open_space_all,
		      entities ) );
	TRY ( h5priv_close_hdf5_dataset ( f, dset_id ) );
	num_entities--;

	h5_int64_t *vals;
	size_t num_vals = 0;
	TRY ( dset_id = h5priv_open_hdf5_dataset ( f, group_id, "values" ) );
	TRY ( num_vals = h5priv_get_npoints_of_hdf5_dataset ( f, dset_id ) );
	TRY ( vals = h5priv_calloc ( f, num_vals, sizeof(*vals) ) );
	TRY ( dsinfo.type_id = h5priv_get_hdf5_dataset_type ( f, dset_id ) );
	TRY ( h5priv_read_dataset (
		      f,
		      dset_id,
		      &dsinfo,
		      _open_space_all, _open_space_all,
		      vals ) );
	TRY ( h5priv_close_hdf5_dataset ( f, dset_id ) );
	type = h5_normalize_h5_type ( f, dsinfo.type_id );
	/*
	  add tagset and set values
	*/
	H5T_Tagset *tagset;
	TRY ( h5tpriv_add_mtagset ( f, name, type, &tagset ) );
	for ( ent_idx = 0; ent_idx < num_entities; ent_idx++ ) {
		h5t_tag_idx_t *entity = &entities[ent_idx];
		size_t dim = (entity+1)->idx - entity->idx;
		TRY ( h5tpriv_set_mtag (
			      f,
			      tagset,
			      entity->eid,
			      dim,
			      &vals[entity->idx] ) );
	}

	return H5_SUCCESS;
}

h5_err_t
h5tpriv_read_tag_container (
	h5_file_t * const f,
	h5t_tagcontainer_t *ctn
	) {
	size_t num_sets;
	TRY ( ( num_sets = h5priv_get_num_objs_in_hdf5_group (
			f, ctn->group_id ) ) );
	hsize_t idx;
	
	for ( idx = 0; idx < num_sets; idx++ ) {
		TRY ( _read_tagset ( f, ctn->group_id, idx ) );
	}

	return H5_SUCCESS;
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
h5_ssize_t
h5tpriv_get_tagset_names_of_entity (
	h5_file_t * const f,
	h5t_tagcontainer_t *ctn,
	h5_id_t entity_id,
	char *names[],
	h5_size_t dim
	) {
	size_t idx;
	size_t _dim = 0;
	for (idx = 0; idx < ctn->num_sets; idx++) {
		void *__retval;
		TRY (h5priv_hsearch (f,
				  &ctn->names[idx],
				  H5_FIND,
				  &__retval,
				  &ctn->sets));
		H5T_Tagset *tset = (H5T_Tagset*)__retval;
		h5t_tagval_t *tval;
		if (_get_tag_valp (f, tset, entity_id, &tval) != H5_SUCCESS) {
			continue;
		}
		if ( (names != NULL) && (_dim <= dim) ) {
			names[_dim] = ctn->names[idx];
		}
		_dim++;
	}
	return _dim;;
}

h5_ssize_t
h5t_get_mtagset_names_of_entity (
	h5_file_t * const f,
	h5_id_t entity_id,
	char *names[],
	h5_size_t dim
	) {
	return h5tpriv_get_tagset_names_of_entity (
		     f, &f->t->mtags, entity_id, names, dim);
}
