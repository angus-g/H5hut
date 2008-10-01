#ifndef __H5T_TYPES_PRIVATE_H
#define __H5T_TYPES_PRIVATE_H


struct h5_vertex {  /* 32Byte */
	h5_id_t		id;
	h5_id_t		unused;	/* for right alignment */
	h5_float64_t	P[3];
};
typedef struct h5_vertex	h5_vertex_t;

struct h5_edge { /* 16Bytes */
	h5_id_t		id;
	h5_id_t		parent_id;
	h5_id_t		refined_on_evel; /* = 0 if not refined*/
	h5_id_t		unused;	/* for right alignment */
	h5_id_t		vertex_ids[2];
};
typedef struct h5_edge		h5_edge_t;

struct h5_triangle { /*24Bytes*/
	h5_id_t		id;
	h5_id_t		parent_id;
	h5_id_t		vertex_ids[3];
	h5_id_t		refined_on_level;
};
typedef struct h5_triangle	h5_triangle_t;

struct h5_tetrahedron { /* 24Bytes */
	h5_id_t		id;
	h5_id_t		parent_id;
	h5_id_t		refined_on_level;
	h5_id_t		unused;	/* for right alignment */
	h5_id_t		vertex_ids[4];
};
typedef struct h5_tetrahedron	h5_tetrahedron_t;

struct h5_ltriangle {
	h5_id_t		vertex_ids[3];	/* local(!) vertex ids */
};

struct h5_ltetrahedron {
	h5_id_t		vertex_ids[4];	/* local(!) vertex ids */
};

union entities {
	struct h5_tetrahedron	*tets;
	struct h5_triangle	*tris;
	void			*data;
};

union lentities {
	struct h5_ltetrahedron	*tets;
	struct h5_ltriangle	*tris;
	void			*data;
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
	hid_t		entity_tid;		/* HDF5 type id: tet, triangle
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
	h5_id_t		last_retrieved_vertex_id; 
	h5_id_t		last_stored_vertex_id; 


	/*** Entities ***/
	union entities	entities;
	union lentities lentities;		/* local vertex id's of
						   entities		*/
	h5_size_t	*num_entities;
	h5_size_t	*num_entities_on_level;
	struct idmap	map_entity_g2l;		/* map global id to local id */

	struct smap				/* array with geometrically */
	sorted_lentities[H5_MAX_VERTICES_PER_ENTITY];/*  sorted local entitiy ids
							 [0]: 0,1,2,3 sorted 
							 [1]: 1,0,2,3 sorted */

	h5_id_t		last_retrieved_entity_id; 
	h5_id_t		last_stored_entity_id;

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
