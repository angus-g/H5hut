#ifndef __H5T_TYPES_PRIVATE_H
#define __H5T_TYPES_PRIVATE_H


struct h5_vertex {
	h5_id_t		vid;
	h5_float64_t	P[3];
};
typedef struct h5_vertex	h5_vertex_t;

struct h5_triangle {
	h5_id_t		cid;
	h5_id_t		parent_cid;
	h5_id_t		refined_on_level;
	h5_id_t		vids[3];
};
typedef struct h5_triangle	h5_triangle_t;

struct h5_tetrahedron {
	h5_id_t		cid;
	h5_id_t		parent_cid;
	h5_id_t		refined_on_level;
	h5_id_t		vids[4];
};

typedef struct h5_tetrahedron	h5_tetrahedron_t;
typedef struct h5_tetrahedron	h5_tet_t;

struct h5_triangle_local {
	h5_id_t		parent_cid;
	h5_id_t		vids[3];	/* local(!) vertex ids */
};
typedef struct h5_triangle_local h5_triangle_local_t;

struct h5_tet_local_ids {
	h5_id_t		parent_cid;
	h5_id_t		vids[4];	/* local(!) vertex ids */
};
typedef struct h5_tet_local	h5_tet_local_t;

union elems {
	h5_tet_t	*tets;
	h5_triangle_t	*tris;
	void		*data;
};

union elems_local {
	h5_tet_local_t	*tets;
	h5_triangle_local_t *tris;
	void		*data;
};

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
};
typedef struct boundary boundary_t;

struct h5t_fdata {
	/*** book-keeping ***/
	char		mesh_name[16];
	char		mesh_label[256];
	enum h5_oid	mesh_type;
	h5_id_t		cur_mesh;
	h5_id_t		mesh_changed;		/* true if mesh is new or has
						   been changed		*/
	h5_id_t		num_meshes;
	hid_t		elem_tid;		/* HDF5 type id: tet, triangle
						   etc			*/
	h5_id_t		cur_level;
	h5_id_t		new_level;		/* idx of the first new level
						   or -1		*/
	h5_size_t	num_levels;

	/*** vertices ***/
	h5_vertex_t	*vertices;
	h5_size_t	*num_vertices;
	struct idmap	map_vertex_g2l;		/* map global id to local id */
	struct smap	sorted_lvertices;
	h5_id_t		last_retrieved_vid; 
	h5_id_t		last_stored_vid; 


	/*** Elements ***/
	union elems	elems;
	union elems_local elems_ldta;		/* local vertex id's of
						   elems		*/
	h5_size_t	*num_elems;
	h5_size_t	*num_elems_on_level;
	struct idmap	map_elem_g2l;		/* map global id to local id */

	struct smap				/* array with geometrically */
	sorted_elems_ldta[H5_MAX_VERTICES_PER_ELEM];/*  sorted local entitiy ids
							 [0]: 0,1,2,3 sorted 
							 [1]: 1,0,2,3 sorted */

	h5_id_t		last_retrieved_eid; 
	h5_id_t		last_stored_eid;

	/*** Boundary Meshes ***/
	h5_id_t		num_boundaries;		/* number of boundaries */
	h5_id_t		boundaries_gid;		/* hdf5 grp id container group */

	boundary_t	boundary;

	/*** HDF5 objects ***/
	hid_t		topo_gid;		/* grp id of mesh in current
						   level		*/
	hid_t		meshes_gid;
	hid_t		mesh_gid;

	/*** type ids' for compound types ***/
	hid_t		float64_3tuple_tid;	/* 3-tuple of 64-bit float */
	hid_t		int32_2tuple_tid;	/* 2-tuple of 32-bit int */
	hid_t		int32_3tuple_tid;	/* 3-tuple of 32-bit int */
	hid_t		int32_4tuple_tid;	/* 4-tuple of 32-bit int */
	hid_t		vertex_tid;		/* vertex structure */
	hid_t		triangle_tid;		/* triangle structure */
	hid_t		tet_tid;		/* tetrahedron structure */
};

#endif
