#ifndef __H5T_TYPES_PRIVATE_H
#define __H5T_TYPES_PRIVATE_H

struct h5t_adjacency_methods {
	h5_err_t (*rebuild_internal_structs)(h5_file_t * const);
	h5_err_t (*release_internal_structs)(h5_file_t * const);
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

struct h5t_methods {
	h5_err_t (*_alloc_elems)(h5_file_t * const, const size_t, const size_t);
	h5_id_t (*_store_elem)(h5_file_t * const, const h5_id_t, const h5_id_t*);
	h5_id_t (*_refine_elem)(h5_file_t * const, const h5_id_t);
	struct h5t_adjacency_methods *adjacency;
};

typedef struct h5_vertex {
	h5_id_t		global_vid;
	h5_coord3d_t	P;
} h5_vertex_t;

typedef struct h5_vertex_data {
	h5_idlist_t	tv;
} h5_vertex_data_t;

typedef struct h5_triangle {
	h5_id_t		global_eid;
	h5_id_t		global_parent_eid;
	h5_id_t		global_child_eid;
	h5_3id_t	global_vids;
} h5_triangle_t;

typedef struct h5_tetrahedron {
	h5_id_t		global_eid;
	h5_id_t		global_parent_eid;
	h5_id_t		global_child_eid;
	h5_4id_t	global_vids;
} h5_tetrahedron_t;
typedef h5_tetrahedron_t	h5_tet_t;

typedef struct h5_elem {
	h5_id_t		global_eid;
	h5_id_t		global_parent_eid;
	h5_id_t		global_child_eid;
	h5_id_t		global_vids[1];
} h5_elem_t;

typedef struct h5_elem_ldta {
	h5_id_t		local_parent_eid;
	h5_id_t		local_child_eid;
	h5_id_t		level_id;
	h5_id_t		*local_vids;
} h5_elem_ldta_t;


typedef union h5_elems {
	h5_tet_t	*tets;
	h5_triangle_t	*tris;
	void		*data;
} h5_elems_t;




typedef struct boundary {
	char		name[16];
	char		label[256];
	h5_id_t		id;			/* name of boundary as integer */
	h5_id_t		changed;		/* true if boundary is new or
						   has been changed */
	h5_id_t		gid;			/* hdf5 grp id boundary */
	h5_id_t		*faces;
	h5_id_t		*lfaces;
	h5_size_t	*num_faces;		/* addit. num of faces per level */
	h5_size_t	*num_faces_on_level;	/* real num of faces per level */
	
	h5_id_t		last_accessed_face;
	h5_dsinfo_t	dsinfo;
} boundary_t;

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

typedef struct h5t_fdata {
	/*** book-keeping ***/
	char		mesh_name[16];
	char		mesh_label[256];
	h5_oid_t	mesh_type;	/* object id of element type */
	h5t_ref_element_t* ref_element;
	h5_id_t		cur_mesh;	/* id of current mesh */
	h5_id_t		mesh_changed;	/* true if new or has been changed */
	h5_id_t		num_meshes;	/* number of meshes */
	h5_id_t		cur_level;	/* id of current level */
	h5_id_t		new_level;	/* idx of the first new level or -1 */
	h5_size_t	num_levels;	/* number of levels */
	h5_id_t		level_changed;
	h5_id_t		storing_data;

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
	h5_elems_t	elems;
	h5_elem_ldta_t	*elems_ldta;	/* local, per element data */
	h5_id_t		*elems_lvids;
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

	/*** Boundary Meshes ***/
	h5_id_t		num_boundaries;		/* number of boundaries */
	h5_id_t		boundaries_gid;		/* hdf5 grp id container group */

	boundary_t	boundary;
	h5t_adjacencies_t adjacencies;

	/*** Tags ***/
	h5t_tagcontainer_t	mtags;
	h5t_tagcontainer_t	stags;
} h5t_fdata_t;

#endif
