#ifndef __H5T_TAGS_PRIVATE_H
#define __H5T_TAGS_PRIVATE_H

/*
  * Tags can be assigned to all entities of a mesh
  * Tag values are arrays of int64 or float64
  * Complex numbers can be stored as array of float64 with even dimension
  * Tags are addressed via a name and the entity.
  * Tags with the same name are called a "tagset"
  * Tagsets can be used to store time/step-constant data. These tagsets are
    called "m-tagsets" and are assigned directly to a mesh.
  * Tagsets can be used to store data which may change from step to step. 
    These tagsets are called "s-tagsets" and are assigned to a mesh and a
    (time-)step.
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
  Structure to store a tag value
*/
typedef struct {
	size_t size;
	union {
		h5_float64_t	f;
		h5_int64_t	i;
	} vals[1];
} h5t_tagval_t;


/*
  Structure with pointers to the tag values of a given tagset of an element.
  We use idx as an index into valp: 
  valp[idx[0]] ... valp[idx[3]] are pointing to the tag values of vertex 0 ... 3
  valp[idx[4]] ... valp[idx[9]] are pointing to the tag values of edge 0 ... 5
  valp[idx[10]] ... valp[idx[13]] are pointing to the tag values of triangle 0 ... 3
  valp[idx[14]] points to the tag value of the elements itself.

  If idx[k] is equal -1, no tag has been assigned to the appropriate entity.
*/
typedef struct {
	signed char size;	/* size of valp */
	signed char idx[15];
	h5t_tagval_t *valp[1];
} h5t_tagsel_t;


typedef struct h5t_tagset {
	char * name;
	unsigned int changed;	/* flag tagset changed, ... */
	unsigned int num_elems;
	h5_id_t type;		/* int64 or float64 */
	h5t_tagsel_t *elems[1];
} h5t_tagset_t;

/*
  Structure for hash table of tagsets
 */
typedef struct {
	unsigned int changed;		/* flag container changed */
	hid_t group_id;
	hsize_t num_sets;		/* number of tagsets */
	char **names;			/* fast access via index */
	h5_hashtable_t	sets;
} h5t_tagcontainer_t;


typedef struct {
	h5_id_t eid ;
	h5_id_t idx;
} h5t_tag_idx_t;

h5_err_t _h5t_write_mtags ( h5_file_t *const f );
h5_err_t _h5t_release_tags ( h5_file_t * const f );
h5_err_t _h5t_read_tag_container ( h5_file_t * const f,
				   h5t_tagcontainer_t *container );
#endif
