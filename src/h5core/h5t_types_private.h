#ifndef __H5T_TYPES_PRIVATE_H
#define __H5T_TYPES_PRIVATE_H

typedef struct h5_glb_vertex {
	h5_glb_idx_t	idx;
	h5_coord3d_t	P;
} h5_glb_vertex_t;

typedef struct h5_loc_vertex {
	h5_loc_idx_t	idx;
	h5_coord3d_t	P;
} h5_loc_vertex_t;

typedef struct h5_glb_triangle {
	h5_glb_idx_t	idx;
	h5_glb_idx_t	parent_idx;
	h5_glb_idx_t	child_idx;
	h5_glb_idx_t	vertex_indices[3];
	h5_glb_idx_t	neighbor_indices[3];
} h5_glb_triangle_t;

typedef struct h5_loc_triangle {
	h5_loc_idx_t	idx;
	h5_loc_idx_t	parent_idx;
	h5_loc_idx_t	child_idx;
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
	h5_loc_idx_t	idx;
	h5_loc_idx_t	parent_idx;
	h5_loc_idx_t	child_idx;
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
	h5_loc_idx_t	idx;
	h5_loc_idx_t	parent_idx;
	h5_loc_idx_t	child_idx;
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
	hid_t		h5_id_t;		/* ID's */

	hid_t		h5_glb_id_t;		/* ID's */
	hid_t		h5_glb_idx_t;		/* ID's */

	hid_t		h5_int64_t;		/* 64 bit signed integer */
	hid_t		h5_float64_t;		/* 64 bit floating point */
	hid_t		h5_coord3d_t;		/* 3-tuple of 64-bit float */
	hid_t		h5_3glb_idx_t;		/* 3-tuple of indices */
	hid_t		h5_4glb_idx_t;		/* 4-tuple of indices */
	hid_t		h5_vertex_t;		/* vertex structure */
	hid_t		h5_triangle_t;		/* triangle structure */
	hid_t		h5_tet_t;		/* tetrahedron structure */
	hid_t		h5t_tag_idx_t;
} h5_dtypes_t;

typedef struct h5t_adjacencies {
	struct {
		// h5_size_t size;
		h5_idlist_t* v;
	} tv;
	h5_hashtable_t te_hash;
	h5_hashtable_t td_hash;
} h5t_adjacencies_t;

struct h5t_store_methods {
	h5_err_t (*alloc_elems)(h5_file_t* const, const size_t, const size_t);
	h5_loc_idx_t (*refine_elem)(h5_file_t* const, const h5_loc_idx_t);
	h5_err_t (*end_store_elems)(h5_file_t* const);
	h5_err_t (*get_direct_children_of_edge)(
		h5_file_t* const, const h5_loc_idx_t, const h5_loc_idx_t,
		h5_loc_id_t*);
};

struct h5t_retrieve_methods {
	h5_err_t (*init_iterator)(
		h5_file_t* const, h5t_entity_iterator_t*, const int);
};

struct h5t_access_methods {
	h5_generic_loc_elem_t* (*get_loc_elem)(
		h5_file_t* const, const h5_loc_idx_t);
	h5_loc_idx_t (*get_loc_elem_parent_idx)(
		h5_file_t* const, const h5_loc_idx_t);
	h5_loc_idx_t (*set_loc_elem_parent_idx)(
		h5_file_t* const, const h5_loc_idx_t, const h5_loc_idx_t);
	h5_loc_id_t (*get_loc_elem_child_idx)(
		h5_file_t* const, const h5_loc_idx_t);
	h5_loc_id_t (*set_loc_elem_child_idx)(
		h5_file_t* const, const h5_loc_idx_t, const h5_loc_idx_t);
	h5_id_t (*get_loc_elem_level_idx)(
		h5_file_t* const, const h5_loc_idx_t);
	h5_id_t (*set_loc_elem_level_idx)(
		h5_file_t* const, const h5_loc_idx_t, const h5_id_t);
	h5_loc_idx_t* (*get_loc_elem_vertex_indices)(
		h5_file_t* const, const h5_loc_idx_t);
	h5_loc_idx_t (*get_loc_elem_vertex_idx)(
		h5_file_t* const, const h5_loc_idx_t, const h5_loc_idx_t);
	h5_loc_idx_t (*set_loc_elem_vertex_idx)(
		h5_file_t* const,
		const h5_loc_idx_t, const h5_loc_idx_t, const h5_loc_idx_t);
	h5_loc_idx_t* (*get_loc_elem_neighbor_indices)(
		h5_file_t* const, const h5_loc_idx_t);
	h5_loc_idx_t (*get_loc_elem_neighbor_idx)(
		h5_file_t* const, const h5_loc_idx_t, const h5_loc_idx_t);
	h5_loc_idx_t (*set_loc_elem_neighbor_idx)(
		h5_file_t* const,
		const h5_loc_idx_t, const h5_loc_idx_t, const h5_loc_idx_t);

	h5_generic_glb_elem_t* (*get_glb_elem)(
		h5_file_t* const, const h5_loc_idx_t);
	h5_glb_id_t (*get_glb_elem_idx)(
		h5_file_t* const, const h5_loc_id_t);
	h5_glb_id_t (*set_glb_elem_idx)(
		h5_file_t* const, const h5_loc_idx_t, const h5_glb_idx_t);
	h5_glb_idx_t (*get_glb_elem_parent_idx)(
		h5_file_t* const, const h5_loc_id_t);
	h5_glb_idx_t (*set_glb_elem_parent_idx)(
		h5_file_t* const, const h5_loc_idx_t, const h5_glb_idx_t);
	h5_glb_idx_t (*get_glb_elem_child_idx)(
		h5_file_t* const, const h5_loc_idx_t);
	h5_glb_idx_t (*set_glb_elem_child_idx)(
		h5_file_t* const, const h5_loc_idx_t, const h5_glb_idx_t);
	h5_glb_idx_t* (*get_glb_elem_vertex_indices)(
		h5_file_t* const, const h5_loc_idx_t);
	h5_glb_idx_t (*get_glb_elem_vertex_idx)(
		h5_file_t* const, const h5_loc_idx_t, const h5_loc_idx_t);
	h5_glb_id_t (*set_glb_elem_vertex_idx)(
		h5_file_t* const,
		const h5_loc_idx_t, const h5_loc_idx_t, const h5_glb_idx_t);
	h5_glb_idx_t* (*get_glb_elem_neighbor_indices)(
		h5_file_t* const, const h5_loc_idx_t);
	h5_glb_idx_t (*get_glb_elem_neighbor_idx)(
		h5_file_t* const, const h5_loc_idx_t, const h5_loc_idx_t);
	h5_glb_idx_t (*set_glb_elem_neighbor_idx)(
		h5_file_t* const,
		const h5_loc_idx_t, const h5_loc_idx_t, const h5_glb_idx_t);
};

struct h5t_read_methods {
	h5_err_t (*init_loc_elems_struct)(h5_file_t* const);
};

struct h5t_adjacency_methods {
	h5_err_t (*update_internal_structs)(h5_file_t* const, h5_id_t);
	h5_err_t (*release_internal_structs)(h5_file_t* const);
	h5_err_t (*get_adjacencies)(
		h5_file_t * const,
		const h5_loc_id_t, const h5_int32_t, h5_idlist_t**);
};

typedef struct h5t_methods {
	struct h5t_read_methods *read;
	struct h5t_store_methods *store;
	struct h5t_retrieve_methods *retrieve;
	struct h5t_access_methods *access;
	struct h5t_adjacency_methods *adjacency;
} h5t_methods_t;


typedef struct h5t_fdata {
	/*** book-keeping ***/
	char		mesh_name[16];
	char		mesh_label[256];
	h5_oid_t	mesh_type;	/* object id of element type */
	const h5t_ref_elem_t*  ref_elem;
	h5_id_t		cur_mesh;	/* id of current mesh */
	h5_id_t		mesh_changed;	/* true if new or has been changed */
	h5_id_t		num_meshes;	/* number of meshes */
	h5_id_t		cur_level;	/* id of current level */
	h5_size_t	num_levels;	/* number of levels */
	h5_id_t		num_loaded_levels;

	/*** HDF5 IDs ***/
	hid_t		topo_gid;	/* grp id of mesh in current
					   level		*/
	hid_t		meshes_gid;	/* HDF5 id */
	hid_t		mesh_gid;

	/*** type ids' for base & compound data types ***/
	h5_dtypes_t	dtypes;

	/*** functions to handle differnt mesh types ***/
	struct h5t_methods methods;

	/*** vertices ***/
	h5_loc_vertex_t	*vertices;
	h5_size_t	*num_vertices;
	h5_idxmap_t	map_vertex_g2l;	/* map global to local idx */
	h5_idlist_t	sorted_lvertices;
	h5_loc_idx_t		last_stored_vid;
	h5_dsinfo_t	dsinfo_vertices;
	h5_dsinfo_t	dsinfo_num_vertices;
	

	/*** Elements ***/
	h5_glb_elems_t	glb_elems;
	h5_loc_elems_t	loc_elems;

	h5_size_t	*num_elems;
	h5_size_t	*num_elems_on_level;
	h5_idxmap_t	map_elem_g2l;	/* map global id to local id */

	/*
	  array with geometrically sorted local entitiy ids
	  [0]: 0,1,2,3 sorted 
	  [1]: 1,0,2,3 sorted
	  ...
	*/
	h5_idlist_t	sorted_elems[H5_MAX_VERTICES_PER_ELEM];

	h5_loc_idx_t	last_stored_eid;
	h5_dsinfo_t	dsinfo_elems;
	h5_dsinfo_t	dsinfo_num_elems;
	h5_dsinfo_t	dsinfo_num_elems_on_level;

	h5_idlist_t	marked_entities;

	/*** Adjacencies ***/
	h5t_adjacencies_t adjacencies;

	/*** Tags ***/
	h5t_tagcontainer_t	mtags;
	h5t_tagcontainer_t	stags;
} h5t_fdata_t;

#endif
