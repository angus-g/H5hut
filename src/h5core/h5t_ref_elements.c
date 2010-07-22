#include "h5core/h5_core.h"

const h5t_ref_elem_t h5t_tri_ref_elem = {
	2,
	{3, 3, 1}, // #vertices, #edges, #triangles
	{
		[0] = {1, 1, 1},		// #vertices of vertices
		[1] = {2, 2, 2},		// #vertices of edges
		[2] = {3}			// #vertices of trinagles
	},
	{
		[0] = {{0}, {1}, {2}},		// 3 vertices
		[1] = {{0,1}, {0,2}, {1,2}},	// 3 edges
		[2] = {{0,1,2}}			// 1 triangles
	},
	{{0.0, 0.0}, {1.0, 0.0}, {0.0, 1.0}},
};

const h5t_ref_elem_t h5t_tet_ref_elem = {
	3,
	{4, 6, 4, 1}, // #vertices, #edges, #triangles, #tetrahedra
	{
		[0] = {1,1,1,1},		// #vertices of vertices
		[1] = {2,2,2,2,2,2},		// #vertices of edges
		[2] = {3,3,3,3},			// #vertices of trinagles
		[3] = {4}			// #vertices of tets
	},
	{
		[0] = {{0}, {1}, {2}, {3}},		 	  // 4 vertices
		[1] = {{0,1}, {0,2}, {1,2}, {0,3}, {1,3}, {2,3}}, // 6 edges
		[2] = {{0,1,2}, {0,1,3}, {0,2,3}, {1,2,3}},	  // 4 triangles
		[3] = {{0,1,2,3}}					  // 1 tets
	},
	{{0.0, 0.0, 0.0}, {1.0, 0.0, 0.0}, {0.0, 1.0, 0.0}, {0.0, 0.0, 1.0}},
};


