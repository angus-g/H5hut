#include <stdlib.h>
#include <string.h>
#include <hdf5.h>

#include "h5_core/h5_core.h"
#include "h5_core/h5_core_private.h"

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
	h5t_tagcontainer_t * container
	) {
	container->names = _h5_calloc ( f, ntags, sizeof(char*) );
	TRY ( _h5_hcreate_string_keyed ( f, ntags, &container->sets ) );
	return H5_SUCCESS;
}
/*
  Initialize tag container for current mesh and level.
 */
h5_err_t
_h5t_init_mtagsets (
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
		TRY ( _h5_free ( f, el_vals->valp[i] ) );
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
	h5t_tagset_t *set = *(h5t_tagset_t**)__set;
	unsigned int i;
	for ( i = 0; i < set->num_elems; i++ ) {
		if ( set->elems[i] != NULL ) {
			TRY ( _release_tagvals_of_elem ( f, set->elems[i] ) );
		}
	}
	TRY ( _h5_free ( f, set->name ) );
	TRY ( _h5_free ( f, set ) );
	return H5_SUCCESS;
}

/*
  Release all sets in given container
 */
static h5_err_t
_release_container (
	h5_file_t * const f,
	h5t_tagcontainer_t * container
	) {
	TRY ( _h5_hwalk ( f, &container->sets, _release_tagset ) ); 
	TRY ( _h5_free ( f, container->names ) );

	return H5_SUCCESS;
}

/*
  Release mesh tag-sets
*/
h5_err_t
_h5t_release_tags (
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
_h5t_add_mtagset (
	h5_file_t * const f,
	char * name,
	h5_id_t type,
	h5t_tagset_t **rtagset
	) {
	h5t_fdata_t *t = f->t;

	/*
	  Initialize data structure for m-tagsets, if not already done.
	*/
	if ( t->mtags.names == NULL ) {
		TRY ( _h5t_init_mtagsets ( f, 521 ) );
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
	h5t_tagset_t *tagset = NULL;
	size_t size = (t->num_elems[t->num_levels-1] - 1) * sizeof(*tagset->elems)
		+ sizeof(*tagset);
	TRY ( (tagset = _h5_calloc ( f, 1, size ) ) );

	TRY ( ( tagset->name = _h5_calloc ( f, 1, strlen(name)+1 ) ) );
	strcpy ( tagset->name, name );
	tagset->type = type;
	tagset->num_elems = t->num_elems[t->num_levels-1];
	/*
	  add tagset to hash of tagsets
	 */
	void *__retval = NULL;
	TRY ( _h5_hsearch ( f, tagset, H5_ENTER, &__retval, &t->mtags.sets ) );

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
	return _h5t_add_mtagset ( f, name, type, 0 );
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
	char name[]
	) {
	/*
	  - release all m-tags
	  - release pointer array
	  - remove tagset with NAME from m-tagsets dictionary
	  - remove HDF5 dataset
	*/
	return -2;
}


h5_size_t
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
h5_size_t
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
	h5t_tagset_t **retval
	) {
	h5t_fdata_t *t = f->t;
	void *__retval = NULL;
	TRY ( _h5_hsearch ( f, &name, H5_FIND, &__retval,
			    &t->mtags.sets ) ); 
	*retval = (h5t_tagset_t*)__retval;
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
	h5t_tagset_t *tagset;
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
h5_size_t
h5t_get_mtagset_info (
	h5_file_t *const f,
	h5_id_t idx,
	char **name,
	h5_id_t *type
	) {
	h5t_fdata_t *t = f->t;
	*name = t->mtags.names[idx];
	void *__retval = NULL;
	_h5_hsearch ( f, t->mtags.names[idx], H5_FIND, &__retval,
		      &t->mtags.sets );
	h5t_tagset_t *retval = (h5t_tagset_t*)__retval;
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
_h5t_set_mtag (
	h5_file_t *const f,
	h5t_tagset_t *tagset,
	const h5_id_t id,
	const size_t size,
	void *val 
	) {
	size_t offs[5] = { 14, 0, 4, 10, 14 };
	h5_id_t el_idx = _h5t_get_elem_idx ( id );
	h5_id_t eoe_id = _h5t_get_face_id ( id );
	h5_id_t type_id = _h5t_get_entity_type ( id );
	if ( tagset->elems[el_idx] == NULL ) {
		/*
		  alloc new structure to store all tags for this element
		  and the given name 
		*/
		TRY ( ( tagset->elems[el_idx] = _h5_calloc (
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
		TRY ( ( tagset->elems[el_idx] = _h5_alloc (
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
	TRY ( ( tagselem->valp[k] = _h5_alloc (
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
  \param[in]	size	size of value
  \param[in]	val	tag value

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
	h5t_tagset_t *tagset;
	TRY ( h5t_open_mtagset ( f, name, &tagset ) );
	return _h5t_set_mtag ( f, tagset, id, size, val );
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
h5_err_t
h5t_get_mtag (
	h5_file_t *const f,
	const h5t_tagset_t *tagset,
	const h5_id_t id,
	size_t *size,
	void *val
	) {
	h5_id_t el_idx = _h5t_get_elem_idx ( id );
	h5_id_t subentity_id = _h5t_get_face_id ( id );
	h5_id_t type_id = _h5t_get_entity_type ( id );

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
	h5t_tagval_t *tagval = tagselem->valp[k];

	*size = tagval->size;
	memcpy ( val, &tagval->vals, *size * sizeof(h5_float64_t) );

	return H5_SUCCESS;
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
h5_err_t
h5t_get_mtag_by_name (
	h5_file_t *const f,
	const char name[],
	const h5_id_t id,
	size_t *size,
	void *val
	) {
	h5t_tagset_t *tagset;
	TRY ( h5t_open_mtagset ( f, name, &tagset ) );
	return h5t_get_mtag ( f, tagset, id, size, val );
}

h5_err_t
h5t_remove_mtag (
	h5_file_t *const f,
	h5t_tagset_t *tagset,
	const h5_id_t id
	) {
	h5_id_t el_idx = _h5t_get_elem_idx ( id );
	h5_id_t subentity_id = _h5t_get_face_id ( id );
	h5_id_t type_id = _h5t_get_entity_type ( id );

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
	TRY ( _h5_free ( f, tagval ) );
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
	h5t_tagset_t *tagset;
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
	h5t_tagset_t *tagset
	) {
	h5t_fdata_t *t = f->t;
	hid_t group_id;
	/*
	  alloc memory
	 */
	h5t_tag_idx_t *elems;
	size_t idx_elem;
	size_t num_elems = t->num_elems[t->num_levels-1];
	TRY ( ( elems = _h5_calloc (
			f, num_elems+1, sizeof(*elems) ) ) );

	h5t_tag_idx_t *entities;
	size_t idx_entity = 0;
	size_t num_entities = 0;
	size_t max_entities = num_elems+1;
	TRY ( ( entities = _h5_calloc (
			f, max_entities, sizeof(*entities) ) ) );

	h5_int64_t *vals;
	size_t idx_val = 0;
	size_t num_vals = 0;
	size_t max_vals = num_elems;
	TRY ( ( vals = _h5_calloc (
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
	for ( idx_elem = 0; idx_elem < num_elems; idx_elem++ ) {
		h5t_tagsel_t *tags_of_elem = tagset->elems[idx_elem];
		size_t num_subentities = 0;
		h5_id_t start_idx_entities = idx_entity;
		size_t i;
		for ( i = 0; i < 15; i++ ) {
			int k = tags_of_elem->idx[i];
			if ( k == -1 ) continue;
			h5t_tagval_t *valp = tags_of_elem->valp[k];
			num_subentities++;
			/*
			  append value to array of values
			*/
			if ( idx_val + valp->size > max_vals ) {
				max_vals += num_elems;
				TRY ( ( vals = _h5_alloc (
						f, vals,
						max_vals*sizeof(*vals) ) ) );
			}
			memcpy (
				vals+idx_val,
				&valp->vals[0].i,
				valp->size*sizeof(*vals) );
			
			/*
			  append entity descriptor
			*/
			if ( idx_entity >= max_entities ) {
				max_entities += num_elems;
				TRY ( ( entities = _h5_alloc (
						f, entities,
						max_entities*sizeof(*entities) )
					      ) );
			}
			h5_id_t type_id = tmap[i];
			h5_id_t sentity_id = smap[i];
			entities[idx_entity].eid = _h5t_build_id (
				type_id, sentity_id, idx_elem );
			entities[idx_entity].idx = idx_val;
			idx_val += valp->size;
			idx_entity++;
			
		}
		/* append element descriptor */
		elems[idx_elem].eid = idx_elem;
		elems[idx_elem].idx = start_idx_entities;
		start_idx_entities += num_subentities;
	}
	num_entities = idx_entity;
	num_vals = idx_val;

	elems[num_elems].eid = -1;
	elems[num_elems].idx = num_entities;
	entities[idx_entity].eid = -1;
	entities[idx_entity].idx = num_vals;

	/*
	  write data
	 */
	TRY ( group_id = _h5_open_group ( f, loc_id, tagset->name ) );
	h5_dsinfo_t dsinfo;
	memset ( &dsinfo, 0, sizeof(dsinfo) );
	dsinfo.rank = 1;
	dsinfo.max_dims[0] = H5S_UNLIMITED;
	dsinfo.chunk_dims[0] = 4096;
	dsinfo.access_prop = H5P_DEFAULT;

	strcpy ( dsinfo.name, "elems" );
	dsinfo.dims[0] = num_elems + 1;
	dsinfo.type_id = t->dtypes.h5t_tag_idx_t;
	TRY( dsinfo.create_prop = _hdf_create_property ( f,
							H5P_DATASET_CREATE ) );
	TRY( _hdf_set_chunk_property ( f, dsinfo.create_prop, dsinfo.rank,
				      dsinfo.chunk_dims ) );
	
	TRY ( _h5_write_dataset_by_name (
		      f,
		      group_id,
		      &dsinfo,
		      _open_space_all, _open_space_all,
		      elems ) );

	strcpy ( dsinfo.name, "entities" );
	dsinfo.dims[0] = num_entities + 1;
	
	TRY ( _h5_write_dataset_by_name (
		      f,
		      group_id,
		      &dsinfo,
		      _open_space_all, _open_space_all,
		      entities ) );

	strcpy ( dsinfo.name, "values" );
	dsinfo.dims[0] = num_vals;
	dsinfo.type_id = t->dtypes.h5_int64_t;
	
	TRY ( _h5_write_dataset_by_name (
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
	h5t_tagcontainer_t *container
	) {
	size_t idx;
	for ( idx = 0; idx < container->num_sets; idx++ ) {
		void *__retval;
		TRY ( _h5_hsearch ( f,
				    &container->names[idx],
				    H5_FIND,
				    &__retval,
				    &container->sets ) );
		      h5t_tagset_t *tagset = (h5t_tagset_t*)__retval;
		      if ( tagset->changed ) {
			      TRY ( _write_tagset (
					    f,
					    container->group_id,
					    tagset ) );
		      }
	}
	return H5_SUCCESS;
}


/*
  Store mesh tags container
 */
h5_err_t
_h5t_write_mtags (
	h5_file_t *const f
	) {
	h5t_fdata_t *t = f->t;
	TRY ( t->mtags.group_id = _h5_open_group ( f, t->mesh_gid, "Tags" ) );
	return _write_container ( f, &f->t->mtags );
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
	TRY ( _hdf_get_objname_by_idx_in_group ( f, loc_id, idx, &name ) );
	TRY ( group_id = _hdf_open_group ( f, loc_id, name ) );

	/*
	  read datasets: "elems", "entities" and "values"
	*/
	h5t_tag_idx_t *elems;
	size_t num_elems = 0;

	TRY ( dset_id = _hdf_open_dataset ( f, group_id, "elems" ) );
	TRY ( num_elems = _hdf_get_npoints_of_dataset ( f, dset_id ) );
	TRY ( elems = _h5_calloc ( f, num_elems, sizeof(*elems) ) );
	h5_dsinfo_t dsinfo;
	memset ( &dsinfo, 0, sizeof(dsinfo) );
	dsinfo.type_id = t->dtypes.h5t_tag_idx_t;
	TRY ( _h5_read_dataset (
		      f,
		      dset_id,
		      &dsinfo,
		      _open_space_all, _open_space_all,
		      elems ) );
	TRY ( _hdf_close_dataset ( f, dset_id ) );
	num_elems--;

	h5t_tag_idx_t *entities;
	size_t idx_entity = 0;
	size_t num_entities = 0;
	TRY ( dset_id = _hdf_open_dataset ( f, group_id, "entities" ) );
	TRY ( num_entities = _hdf_get_npoints_of_dataset ( f, dset_id ) );
	TRY ( entities = _h5_calloc ( f, num_entities, sizeof(*entities) ) );
	TRY ( _h5_read_dataset (
		      f,
		      dset_id,
		      &dsinfo,
		      _open_space_all, _open_space_all,
		      entities ) );
	TRY ( _hdf_close_dataset ( f, dset_id ) );
	num_entities--;

	h5_int64_t *vals;
	size_t num_vals = 0;
	TRY ( dset_id = _hdf_open_dataset ( f, group_id, "values" ) );
	TRY ( num_vals = _hdf_get_npoints_of_dataset ( f, dset_id ) );
	TRY ( vals = _h5_calloc ( f, num_vals, sizeof(*vals) ) );
	TRY ( dsinfo.type_id = _hdf_get_dataset_type ( f, dset_id ) );
	TRY ( _h5_read_dataset (
		      f,
		      dset_id,
		      &dsinfo,
		      _open_space_all, _open_space_all,
		      vals ) );
	TRY ( _hdf_close_dataset ( f, dset_id ) );
	type = h5_normalize_h5_type ( f, dsinfo.type_id );
	/*
	  add tagset and set values
	*/
	h5t_tagset_t *tagset;
	TRY ( _h5t_add_mtagset ( f, name, type, &tagset ) );
	for ( idx_entity = 0; idx_entity < num_entities; idx_entity++ ) {
		h5t_tag_idx_t *entity = &entities[idx_entity];
		size_t dim = (entity+1)->idx - entity->idx;
		TRY ( _h5t_set_mtag (
			      f,
			      tagset,
			      entity->eid,
			      dim,
			      &vals[entity->idx] ) );
	}

	return H5_SUCCESS;
}

h5_err_t
_h5t_read_tag_container (
	h5_file_t * const f,
	h5t_tagcontainer_t *container
	) {
	size_t num_sets;
	TRY ( ( num_sets = _hdf_get_num_objs_in_group (
			f, container->group_id ) ) );
	hsize_t idx;
	
	for ( idx = 0; idx < num_sets; idx++ ) {
		TRY ( _read_tagset ( f, container->group_id, idx ) );
	}

	return H5_SUCCESS;
}

