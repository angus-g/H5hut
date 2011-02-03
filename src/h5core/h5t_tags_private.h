#ifndef __H5T_TAGS_PRIVATE_H
#define __H5T_TAGS_PRIVATE_H

/*
  * Tags can be assigned to all entities of a mesh
  * Tag values are arrays of int64 or float64
  * Complex numbers can be stored as array of float64 with even dimension
  * Tags are addressed via a name and the entity id.
  * Tags with the same name are called a "tagset"
  * Tagsets can be used to store time/step-constant data. These tagsets are
    called "m-tagsets" and are assigned directly to a mesh.
  * Tagsets can be used to store data which may change from step to step. 
    These tagsets are called "s-tagsets" and are assigned to a mesh and a
    (time-)step.

ToDo
  * Scalar values
 */


/*
  Tags are addressed via name and entity id. We access a tag very indirect:
  First we lookup for the name of the tagset in a hash table. The value of the 
  hash entry gives us information about the data type stored in this tagset,
  whether this tagset has been changed or not and a pointer to another data
  structure. This data structure keeps information about tags within the given
  tagset per element.
 */


/*
  All tags of an element are stored in an array of the below defined
  structure
*/
typedef struct {
	union {
		h5_float64_t	f;
		h5_int64_t	i;
	} item;
} h5t_tagval_t;


/*

*/
typedef struct {
	int16_t face_id;	// face id: type and face index
	uint16_t val_dim;	// dim of value for this entity
	uint32_t val_idx;	// index of first value
} h5t_taginfo_t; 

typedef struct {
	int32_t num_tags;
	h5t_taginfo_t ti[1];
} h5t_tageleminfo_t;

struct h5t_tagset {
	char* name;		// name of tagset
	unsigned int changed;	// flag tagset changed, ...
	struct {
		h5t_lvl_idx_t min_level;
		h5t_lvl_idx_t max_level;
	} scope;
	h5_id_t type;		// int64 or float64
	h5_loc_idx_t num_entities;// number of tagged entities
	h5_loc_idx_t num_values;// number of values
	h5_loc_idx_t num_elems; // number of elements in tagset
	h5t_tagval_t* values;	// ptr to array of tag values
	h5t_tageleminfo_t* elems[1]; // per element structure
};

/*
  Structure for hash table of tagsets
 */
struct h5t_tagcontainer {
	unsigned int changed;	// flag container changed
	hid_t group_id;
	hsize_t num_sets;	// number of tagsets
	char** names;		// fast access via index
	h5_hashtable_t	sets;	// hash table
};


typedef struct {
	h5_glb_id_t eid ;	// global entity id
	h5_glb_idx_t idx;	// global index
} h5t_glb_tag_idx_t;

typedef struct {
	h5_loc_id_t eid ;	// local entity id
	h5_loc_idx_t idx;	// local index
} h5t_loc_tag_idx_t;

h5_err_t h5tpriv_write_mtags ( h5_file_t *const f );
h5_err_t h5tpriv_release_tags ( h5_file_t * const f );
h5_err_t h5tpriv_read_tag_container ( h5_file_t * const f,
				   h5t_tagcontainer_t *container );
#endif
