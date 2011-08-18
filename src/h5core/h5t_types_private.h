#ifndef __H5T_TYPES_PRIVATE_H
#define __H5T_TYPES_PRIVATE_H

typedef struct h5_glb_vertex {
	h5_glb_idx_t	idx;
	h5_coord3d_t	P;
} h5_glb_vertex_t;

typedef struct h5_glb_vertex h5_loc_vertex_t;

typedef struct h5_glb_triangle {
	h5_glb_idx_t	idx;
	h5_glb_idx_t	parent_idx;
	h5_glb_idx_t	child_idx;
	h5_glb_idx_t	vertex_indices[3];
	h5_glb_idx_t	neighbor_indices[3];
} h5_glb_triangle_t;

typedef struct h5_loc_triangle {
	h5_glb_idx_t	glb_idx;	// global index of element
	h5_loc_idx_t	parent_idx;	// index of parent element
	h5_loc_idx_t	child_idx;	// index of (first) children
	h5t_lvl_idx_t	level_idx;	// leaf level on which this element has been created
	h5t_lvl_idx_t	refinement_level; // refinements of father in macro-grid
	h5t_elem_flags_t flags;
	h5_loc_idx_t	vertex_indices[3];
	h5_loc_idx_t	neighbor_indices[3];
} h5_loc_triangle_t;

typedef struct h5_glb_tetrahedron {
	h5_glb_idx_t	idx;
	h5_glb_idx_t	parent_idx;
	h5_glb_idx_t	child_idx;
	h5_glb_idx_t	vertex_indices[4];
	h5_glb_idx_t	neighbor_indices[4];
} h5_glb_tetrahedron_t;
typedef h5_glb_tetrahedron_t	h5_glb_tet_t;

typedef struct h5_loc_tetrahedron {
	h5_glb_idx_t	glb_idx;
	h5_loc_idx_t	parent_idx;
	h5_loc_idx_t	child_idx;
	h5t_lvl_idx_t	level_idx;
	h5t_lvl_idx_t	refinement_level;
	h5t_elem_flags_t flags;
	h5_loc_idx_t	vertex_indices[4];
	h5_loc_idx_t	neighbor_indices[4];
} h5_loc_tetrahedron_t;
typedef h5_loc_tetrahedron_t	h5_loc_tet_t;

typedef struct h5_generic_glb_elem {
	h5_glb_idx_t	idx;
	h5_glb_idx_t	parent_idx;
	h5_glb_idx_t	child_idx;
	h5_glb_idx_t	indices[1];
} h5_generic_glb_elem_t;

typedef struct h5_generic_loc_elem {
	h5_glb_idx_t	glb_idx;
	h5_loc_idx_t	parent_idx;
	h5_loc_idx_t	child_idx;
	h5t_lvl_idx_t	level_idx;
	h5t_lvl_idx_t	refinement_level;
	h5t_elem_flags_t flags;
	h5_loc_idx_t	indices[1];
} h5_generic_loc_elem_t;

typedef union h5_glb_elems {
	h5_glb_tet_t	*tets;
	h5_glb_triangle_t *tris;
	void		*data;
} h5_glb_elems_t;

typedef union h5_loc_elems {
	h5_loc_tet_t	*tets;
	h5_loc_triangle_t *tris;
	void		*data;
} h5_loc_elems_t;

/*** type ids' for compound types ***/
typedef struct h5_dtypes {
	hid_t		h5_glb_idx_t;		/* ID's */
	hid_t		h5_int64_t;		/* 64 bit signed integer */
	hid_t		h5_float64_t;		/* 64 bit floating point */
	hid_t		h5_coord3d_t;		/* 3-tuple of 64-bit float */
	hid_t		h5_3glb_idx_t;		/* 3-tuple of indices */
	hid_t		h5_4glb_idx_t;		/* 4-tuple of indices */
	hid_t		h5_vertex_t;		/* vertex structure */
	hid_t		h5_triangle_t;		/* triangle structure */
	hid_t		h5_tet_t;		/* tetrahedron structure */
	hid_t		h5t_glb_tag_idx_t;
} h5_dtypes_t;

typedef struct h5t_adjacencies {
	struct {
		// h5_size_t size;
		h5_loc_idlist_t** v;
	} tv;
	h5_hashtable_t te_hash;
	h5_hashtable_t td_hash;
} h5t_adjacencies_t;

struct h5t_read_methods;
struct h5t_store_methods;
struct h5t_retrieve_methods;
struct h5t_access_methods;
struct h5t_adjacency_methods;

typedef struct h5t_methods {
	struct h5t_read_methods *read;
	struct h5t_store_methods *store;
	struct h5t_retrieve_methods *retrieve;
	struct h5t_access_methods *access;
	struct h5t_adjacency_methods *adjacency;
} h5t_methods_t;


struct h5t_mesh {
	/*** book-keeping ***/
	char		mesh_name[256];
	const h5t_ref_elem_t*  ref_elem;
	h5_id_t		mesh_changed;	/* true if new or has been changed */
	h5t_lvl_idx_t	leaf_level;	/* idx of current level */
	h5t_lvl_idx_t	num_leaf_levels;/* number of levels */
	h5t_lvl_idx_t	num_loaded_levels;

	h5_strlist_t*	mtagsets;

	/*** HDF5 IDs ***/
	hid_t		mesh_gid;

	/*** functions to handle differnt mesh types ***/
	struct h5t_methods methods;

	/*** type ids' for base & compound data types ***/
	h5_dtypes_t	dtypes;

	/*** vertices ***/
	h5_loc_vertex_t	*vertices;
	h5_size_t	*num_vertices;
	h5_idxmap_t	map_vertex_g2l;	/* map global to local idx */
	h5_loc_idx_t   	last_stored_vid;
	h5_dsinfo_t	dsinfo_vertices;

	/*** Elements ***/
	h5_glb_elems_t	glb_elems;
	h5_loc_elems_t	loc_elems;

	h5_size_t	*num_elems;
	h5_size_t	*num_leaf_elems;
	h5_idxmap_t	map_elem_g2l;	/* map global id to local id */

	h5_loc_idx_t	last_stored_eid;
	h5_dsinfo_t	dsinfo_elems;

	h5_loc_idlist_t* marked_entities;

	/*** Adjacencies ***/
	h5t_adjacencies_t adjacencies;

	/*** File ***/
	h5_file_t*	f;
};

#endif
