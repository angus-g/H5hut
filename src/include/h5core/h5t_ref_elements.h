#ifndef __H5T_REFERENCE_ELEMENTS_H
#define __H5T_REFERENCE_ELEMENTS_H

#define H5T_MAX_DIM 3
#define H5T_MAX_FACES 6		// edges in tetrahedron
#define H5T_MAX_VERTICES 4	// tetrahedron

typedef struct {
	int dim;
	int num_faces[H5T_MAX_DIM+1];
	int num_vertices_of_face[H5T_MAX_DIM+1][H5T_MAX_FACES];
	int map[H5T_MAX_DIM+1][H5T_MAX_FACES][H5T_MAX_VERTICES];
	h5_float64_t coords[H5T_MAX_VERTICES][H5T_MAX_DIM];
} h5t_ref_elem_t;

extern const h5t_ref_elem_t h5t_tet_ref_elem;
extern const h5t_ref_elem_t h5t_tri_ref_elem;

#endif
