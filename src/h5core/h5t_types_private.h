#ifndef __H5T_TYPES_PRIVATE_H
#define __H5T_TYPES_PRIVATE_H

typedef struct h5_vertex {
	h5_id_t		global_idx;
	h5_coord3d_t	P;
} h5_vertex_t;

typedef struct h5_vertex_data {
	h5_idlist_t	tv;
} h5_vertex_data_t;

typedef struct h5_triangle {
	h5_id_t		idx;
	h5_id_t		parent_idx;
	h5_id_t		child_idx;
	h5_id_t		level_idx;
	h5_3id_t	vertex_indices;
	h5_3id_t	neighbor_indices;
} h5_triangle_t;

typedef struct h5_tetrahedron {
	h5_id_t		idx;
	h5_id_t		parent_idx;
	h5_id_t		child_idx;
	h5_id_t		level_idx;
	h5_4id_t	vertex_indices;
	h5_4id_t	neighbor_indices;
} h5_tetrahedron_t;
typedef h5_tetrahedron_t	h5_tet_t;

typedef struct h5_generic_elem {
	h5_id_t		idx;
	h5_id_t		parent_idx;
	h5_id_t		child_idx;
	h5_id_t		level_idx;
	h5_id_t		indices[1];
} h5_generic_elem_t;


typedef union h5_elems {
	h5_tet_t	*tets;
	h5_triangle_t	*tris;
	void		*data;
} h5_elems_t;

/*** type ids' for compound types ***/
typedef struct h5_dtypes {
	hid_t		h5_id_t;		/* ID's */
	hid_t		h5_int64_t;		/* 64 bit signed integer */
	hid_t		h5_float64_t;		/* 64 bit floating point */
	hid_t		h5_coord3d_t;		/* 3-tuple of 64-bit float */
	hid_t		h5_3id_t;		/* 3-tuple of ID's */
	hid_t		h5_4id_t;		/* 4-tuple of ID's */
	hid_t		h5_vertex_t;		/* vertex structure */
	hid_t		h5_triangle_t;		/* triangle structure */
	hid_t		h5_tet_t;		/* tetrahedron structure */
	hid_t		h5t_tag_idx_t;
} h5_dtypes_t;

typedef struct h5t_adjacencies {
	h5_hashtable_t te_hash;
	h5_hashtable_t td_hash;
} h5t_adjacencies_t;

struct h5t_store_methods {
	h5_err_t (*alloc_elems)(h5_file_t* const, const size_t, const size_t);
	h5_id_t (*refine_elem)(h5_file_t* const, const h5_id_t);
	h5_err_t (*end_store_elems)(h5_file_t* const);
	h5_id_t (*get_direct_children_of_edge)(h5_file_t* const, const h5_id_t, const h5_id_t, h5_id_t*);
};

struct h5t_retrieve_methods {
	h5_err_t (*init_iterator)(h5_file_t* const, h5t_entity_iterator_t*, const int);
};


struct h5t_access_methods {
	h5_generic_elem_t* (*get_loc_elem)(
		h5_file_t* const, const h5_id_t);
	h5_id_t (*get_loc_elem_idx)(
		h5_file_t* const, const h5_id_t);
	h5_id_t (*set_loc_elem_idx)(
		h5_file_t* const, const h5_id_t, const h5_id_t);
	h5_id_t (*get_loc_elem_parent_idx)(
		h5_file_t* const, const h5_id_t);
	h5_id_t (*set_loc_elem_parent_idx)(
		h5_file_t* const, const h5_id_t, const h5_id_t);
	h5_id_t (*get_loc_elem_child_idx)(
		h5_file_t* const, const h5_id_t);
	h5_id_t (*set_loc_elem_child_idx)(
		h5_file_t* const, const h5_id_t, const h5_id_t);
	h5_id_t (*get_loc_elem_level_idx)(
		h5_file_t* const, const h5_id_t);
	h5_id_t (*set_loc_elem_level_idx)(
		h5_file_t* const, const h5_id_t, const h5_id_t);
	h5_id_t* (*get_loc_elem_vertex_indices)(
		h5_file_t* const, const h5_id_t);
	h5_id_t (*get_loc_elem_vertex_idx)(
		h5_file_t* const, const h5_id_t, const h5_id_t);
	h5_id_t (*set_loc_elem_vertex_idx)(
		h5_file_t* const, const h5_id_t, const h5_id_t, const h5_id_t);
	h5_id_t* (*get_loc_elem_neighbor_indices)(
		h5_file_t* const, const h5_id_t);
	h5_id_t (*get_loc_elem_neighbor_idx)(
		h5_file_t* const, const h5_id_t, const h5_id_t);
	h5_id_t (*set_loc_elem_neighbor_idx)(
		h5_file_t* const, const h5_id_t, const h5_id_t, const h5_id_t);

	h5_generic_elem_t* (*get_glb_elem)(
		h5_file_t* const, const h5_id_t);
	h5_id_t (*get_glb_elem_idx)(
		h5_file_t* const, const h5_id_t);
	h5_id_t (*set_glb_elem_idx)(
		h5_file_t* const, const h5_id_t, const h5_id_t);
	h5_id_t (*get_glb_elem_parent_idx)(
		h5_file_t* const, const h5_id_t);
	h5_id_t (*set_glb_elem_parent_idx)(
		h5_file_t* const, const h5_id_t, const h5_id_t);
	h5_id_t (*get_glb_elem_child_idx)(
		h5_file_t* const, const h5_id_t);
	h5_id_t (*set_glb_elem_child_idx)(
		h5_file_t* const, const h5_id_t, const h5_id_t);
	h5_id_t (*get_glb_elem_level_idx)(
		h5_file_t* const, const h5_id_t);
	h5_id_t (*set_glb_elem_level_idx)(
		h5_file_t* const, const h5_id_t, const h5_id_t);
	h5_id_t* (*get_glb_elem_vertex_indices)(
		h5_file_t* const, const h5_id_t);
	h5_id_t (*get_glb_elem_vertex_idx)(
		h5_file_t* const, const h5_id_t, const h5_id_t);
	h5_id_t (*set_glb_elem_vertex_idx)(
		h5_file_t* const, const h5_id_t, const h5_id_t, const h5_id_t);
	h5_id_t* (*get_glb_elem_neighbor_indices)(
		h5_file_t* const, const h5_id_t);
	h5_id_t (*get_glb_elem_neighbor_idx)(
		h5_file_t* const, const h5_id_t, const h5_id_t);
	h5_id_t (*set_glb_elem_neighbor_idx)(
		h5_file_t* const, const h5_id_t, const h5_id_t, const h5_id_t);

};

struct h5t_read_methods {
	h5_err_t (*init_loc_elems_struct)(h5_file_t* const);
};

struct h5t_adjacency_methods {
	h5_err_t (*update_internal_structs)(h5_file_t* const, h5_id_t);
	h5_err_t (*release_internal_structs)(h5_file_t* const);
	h5_err_t (*get_edges_upadjacent_to_vertex)(
		h5_file_t * const, const h5_id_t, h5_idlist_t**);
	h5_err_t (*get_triangles_upadjacent_to_vertex)(
		h5_file_t * const, const h5_id_t, h5_idlist_t**);
	h5_err_t (*get_tets_upadjacent_to_vertex)(
		h5_file_t * const, const h5_id_t, h5_idlist_t**);
	h5_err_t (*get_triangles_upadjacent_to_edge)(
		h5_file_t * const, const h5_id_t, h5_idlist_t**);
	h5_err_t (*get_tets_upadjacent_to_edge)(
		h5_file_t * const, const h5_id_t, h5_idlist_t**);
	h5_err_t (*get_tets_upadjacent_to_triangle)(
		h5_file_t * const, const h5_id_t, h5_idlist_t**);
	h5_err_t (*get_vertices_downadjacent_to_edge)(
		h5_file_t * const, const h5_id_t, h5_idlist_t**);
	h5_err_t (*get_vertices_downadjacent_to_triangle)(
		h5_file_t * const, const h5_id_t, h5_idlist_t**);
	h5_err_t (*get_vertices_downadjacent_to_tet)(
		h5_file_t * const, const h5_id_t, h5_idlist_t**);
	h5_err_t (*get_edges_downadjacent_to_triangle)(
		h5_file_t * const, const h5_id_t, h5_idlist_t**);
	h5_err_t (*get_edges_downadjacent_to_tet)(
		h5_file_t * const, const h5_id_t, h5_idlist_t**);
	h5_err_t (*get_triangles_downadjacent_to_tet)(
		h5_file_t * const, const h5_id_t, h5_idlist_t**);
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
	h5_vertex_t	*vertices;
	h5_vertex_data_t *vertices_data;
	h5_size_t	*num_vertices;
	h5_idmap_t	map_vertex_g2l;	/* map global id to local id */
	h5_idlist_t	sorted_lvertices;
	h5_id_t		last_stored_vid;
	h5_dsinfo_t	dsinfo_vertices;
	h5_dsinfo_t	dsinfo_num_vertices;
	

	/*** Elements ***/
	h5_elems_t	glb_elems;
	h5_elems_t	loc_elems;

	h5_size_t	*num_elems;
	h5_size_t	*num_elems_on_level;
	h5_idmap_t	map_elem_g2l;	/* map global id to local id */

	/*
	  array with geometrically sorted local entitiy ids
	  [0]: 0,1,2,3 sorted 
	  [1]: 1,0,2,3 sorted
	  ...
	*/
	h5_idlist_t	sorted_elems[H5_MAX_VERTICES_PER_ELEM];

	h5_id_t		last_stored_eid;
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
