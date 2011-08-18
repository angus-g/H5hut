#ifndef __H5T_RETRIEVE_H
#define __H5T_RETRIEVE_H

#ifdef __cplusplus
extern "C" {
#endif

struct h5t_tagset;
union h5t_iterator;

typedef struct {
	h5t_mesh_t* mesh;
	h5_loc_id_t (*iter)(union h5t_iterator*);
	h5t_lvl_idx_t leaf_level;
	const h5t_ref_elem_t* ref_elem;
	h5_loc_idx_t elem_idx;
	h5_loc_idx_t face_idx;	// face according reference element
	int codim;		// dimension of entities to traverse
	h5_err_t (*find)(h5t_mesh_t *const, h5_loc_id_t, h5_loc_id_t, h5_loc_idlist_t**);
} h5t_leaf_iterator_t;

typedef struct {
	h5t_mesh_t* mesh;
	h5_loc_id_t (*iter)(union h5t_iterator* iter);
	h5t_lvl_idx_t refinement_level;
	const h5t_ref_elem_t* ref_elem;
	h5_loc_idx_t elem_idx;
	h5_loc_idx_t face_idx;	// face  according reference element
	int codim;		// dimension of entities to traverse
	h5_err_t (*find)(h5t_mesh_t *const f, h5_loc_id_t face_idx,
			 h5_loc_id_t elem_idx, h5_loc_idlist_t **retval);
} h5t_level_iterator_t;

typedef struct {
	h5t_mesh_t* mesh;
	h5_loc_id_t (*iter)(union h5t_iterator* iter);
	h5t_lvl_idx_t level_idx;
	struct h5t_tagset* tagset;
	h5_loc_idx_t elem_idx;
	int subentity_idx;
} h5t_tag_iterator_t;

typedef struct h5t_generic_iterator {
	h5t_mesh_t* mesh;
	h5_loc_id_t (*iter)(union h5t_iterator* iter);
} h5t_generic_iterator_t;

typedef union h5t_iterator {
	h5t_leaf_iterator_t	leaf;
	h5t_level_iterator_t	level;
	h5t_tag_iterator_t	tag;
	h5t_generic_iterator_t	generic;
} h5t_iterator_t;

typedef h5t_iterator_t* h5t_iterator_p;

h5_err_t
h5t_init_leaf_iterator (h5t_iterator_t*, h5t_mesh_t*, const int);

h5_err_t
h5t_init_boundary_face_iterator (h5t_iterator_t*, h5t_mesh_t*, const int);

h5_err_t
h5t_init_mtag_iterator (h5t_iterator_t*, h5t_mesh_t*, const char*);

h5_err_t
h5t_release_entity_iterator (h5t_iterator_t*);

h5_loc_id_t
h5t_iterate_entities (h5t_iterator_t*);

h5_err_t
h5t_end_iterate_entities (h5t_iterator_t*);

h5_err_t
h5t_get_vertex_coords_by_index (h5t_mesh_t* const, h5_loc_idx_t, h5_float64_t[3]);

h5_err_t
h5t_get_vertex_coords_by_id (h5t_mesh_t* const, h5_loc_id_t, h5_float64_t[3]);

#ifdef __cplusplus
}
#endif

#endif
