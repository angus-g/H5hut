#ifndef __H5T_TYPES_PRIVATE_H
#define __H5T_TYPES_PRIVATE_H

typedef h5_id_t h5_2id_t[2];
typedef h5_id_t h5_3id_t[3];
typedef h5_id_t h5_4id_t[4];
typedef h5_float64_t h5_coord3d_t[3];

struct h5_vertex {
	h5_id_t		global_vid;
	h5_coord3d_t	P;
};
typedef struct h5_vertex	h5_vertex_t;

struct h5_vertex_data {
	h5_idlist_t	tv;
};
typedef struct h5_vertex_data h5_vertex_data_t;

struct h5_triangle {
	h5_id_t		global_eid;
	h5_id_t		global_parent_eid;
	h5_id_t		global_child_eid;
	h5_3id_t	global_vids;
};
typedef struct h5_triangle	h5_triangle_t;

struct h5_tetrahedron {
	h5_id_t		global_eid;
	h5_id_t		global_parent_eid;
	h5_id_t		global_child_eid;
	h5_4id_t	global_vids;
};

struct h5_elem {
	h5_id_t		global_eid;
	h5_id_t		global_parent_eid;
	h5_id_t		global_child_eid;
	h5_id_t		global_vids[1];
};

typedef struct h5_tetrahedron	h5_tetrahedron_t;
typedef struct h5_tetrahedron	h5_tet_t;
typedef struct h5_elem		h5_elem_t;

struct h5_elem_ldta {
	h5_id_t		local_parent_eid;
	h5_id_t		local_child_eid;
	h5_id_t		level_id;
	h5_id_t		*local_vids;
};
typedef struct h5_elem_ldta	h5_elem_ldta_t;


union h5_elems {
	h5_tet_t	*tets;
	h5_triangle_t	*tris;
	void		*data;
};
typedef union h5_elems h5_elems_t;


/*
  information about HDF5 dataset
*/
struct h5_dataset_info {
	char name[256];
	int rank;
	hsize_t dims[4];
	hsize_t maxdims[4];
	hsize_t chunk_size[4];
	hid_t *type_id;
	hid_t create_prop;
	hid_t access_prop;
};
typedef struct h5_dataset_info h5_dataset_info_t;


struct boundary {
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
	h5_dataset_info_t	dsinfo;
};
typedef struct boundary boundary_t;

/*** type ids' for compound types ***/
struct h5_dtypes {
	hid_t		h5_id_t;
	hid_t		h5_int64_t;
	hid_t		h5_float64_t;
	hid_t		h5_coord3d_t;		/* 3-tuple of 64-bit float */
	hid_t		h5_3id_t;		/* 3-tuple of id's */
	hid_t		h5_4id_t;		/* 4-tuple of id's */
	hid_t		h5_vertex_t;		/* vertex structure */
	hid_t		h5_triangle_t;		/* triangle structure */
	hid_t		h5_tet_t;		/* tetrahedron structure */
};
typedef struct h5_dtypes h5_dtypes_t;

struct h5_te_node_key {
	h5_2id_t vids;
};
typedef struct h5_te_node_key h5_te_node_key_t;

struct h5_te_node {
	h5_te_node_key_t key;
	h5_idlist_t value;
};
typedef struct h5_te_node h5_te_node_t;

struct h5_td_node_key {
	h5_3id_t vids;
};
typedef struct h5_td_node_key h5_td_node_key_t;

struct h5_td_node {
	h5_td_node_key_t key;
	h5_idlist_t value;
};
typedef struct h5_td_node h5_td_node_t;

struct h5t_adjacencies {
	void * te_tree;
	void * td_tree;
};
typedef struct h5t_adjacencies h5t_adjacencies_t;

struct h5t_elem_iterator {
	h5_id_t	cur_eid;
};
typedef struct h5t_elem_iterator h5t_elem_iterator_t;

struct h5t_vertex_iterator {
	h5_id_t	cur_vid;
};
typedef struct h5t_vertex_iterator h5t_vertex_iterator_t;

struct h5t_entity_iterator {
	h5_id_t	cur_fid;
	h5_id_t cur_eid;
};
typedef struct h5t_entity_iterator h5t_entity_iterator_t;

struct h5t_iterators {
	h5t_vertex_iterator_t	vertex;
	h5t_entity_iterator_t	edge;
	h5t_entity_iterator_t	triangle;
	h5t_elem_iterator_t	elem;
};

typedef struct h5t_iterators h5t_iterators_t;

struct h5t_fdata {
	/*** book-keeping ***/
	char		mesh_name[16];
	char		mesh_label[256];
	h5_oid_t	mesh_type;	/* object id of element type */
	h5_id_t		cur_mesh;	/* id of current mesh */
	h5_id_t		mesh_changed;	/* true if new or has been changed */
	h5_id_t		num_meshes;	/* number of meshes */
	hid_t		elem_tid;	/* HDF5 type id: tet, triangle etc */
	h5_id_t		cur_level;	/* id of current level */
	h5_id_t		new_level;	/* idx of the first new level or -1 */
	h5_size_t	num_levels;	/* number of levels */
	h5_id_t		level_changed;
	h5_id_t		storing_data;

	hid_t		topo_gid;		/* grp id of mesh in current
						   level		*/
	hid_t		meshes_gid;
	hid_t		mesh_gid;

	/*** type ids' for base & compound data types ***/
	h5_dtypes_t	dtypes;

	h5t_iterators_t	iters;

	/*** vertices ***/
	h5_vertex_t	*vertices;
	h5_vertex_data_t *vertices_data;
	h5_size_t	*num_vertices;
	h5_idmap_t	map_vertex_g2l;	/* map global id to local id */
	h5_idlist_t	sorted_lvertices;
	h5_id_t		last_stored_vid;
	h5_dataset_info_t	dsinfo_vertices;
	h5_dataset_info_t	dsinfo_num_vertices;
	

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
	h5_dataset_info_t	dsinfo_elems;
	h5_dataset_info_t	dsinfo_num_elems;
	h5_dataset_info_t	dsinfo_num_elems_on_level;

	/*** Boundary Meshes ***/
	h5_id_t		num_boundaries;		/* number of boundaries */
	h5_id_t		boundaries_gid;		/* hdf5 grp id container group */

	boundary_t	boundary;
	h5t_adjacencies_t adjacencies;


};
typedef struct h5t_fdata h5t_fdata_t;

#endif
