#include "h5core/h5_core.h"

/*
  
  face: All kinds of sub-elements: vertices, edges, triangles ...
  facet: co-dim 1 face
  entities: all elements and faces of a grid
 */

/*
 2
 **
 *  *
 *    *
 *      *
 *        *
 *          *
 *            *
 1              2
 *                *
 *                  *
 *                    *
 *                      *
 *                        *
 *                          *
 *                            *
 0 * * * * * * * 0 * * * * * * *1

*/

const h5t_ref_elem_t h5t_tri_ref_elem = {
	2,					// dimension
	{
		3,				// #vertices
		3,				// #edges
		1				// #triangles
	},
	{	// number of vertices per sub-element
		[0] = {1, 1, 1},		// #vertices of vertices
		[1] = {2, 2, 2},		// #vertices of edges
		[2] = {3}			// #vertices of trinagles
	},
	{	// map sub-elements to vertices
		[0] = {{0}, {1}, {2}},		// 3 vertices
		[1] = {{0,1}, {0,2}, {1,2}},	// 3 edges
		[2] = {{0,1,2}}			// 1 triangles
	},
	{	// edges connected to vertex
		[0] = {0,1},
		[1] = {0,2},
		[2] = {1,2}
	},
	{	// triangles connected to vertex
		[0] = {0},
		[1] = {0},
		[2] = {0}
	},
	{	// coordinates
		{0.0, 0.0},
		{1.0, 0.0},
		{0.0, 1.0}
	}
};


/*
 3
 **
 ** *
 * *  *
 *  *   *
 *   *    *
 *    5     *
 *     *      *
 3      *       4
 *   2   *    3   *
 *        2         *
 *       *    *       *
 *     *          *     *
 *   1       0        2   *
 * *                      * *
 **                          **
 0 * * * * * * * 0 * * * * * * *1

 Front face is {0,1,3} with id 1
*/

const h5t_ref_elem_t h5t_tet_ref_elem = {
	3,					// dimension
	{
		4,				// #vertices
		6,				// #edges
		4,				// #triangles
		1				// #tetrahedra
	},
	{	// number of vertices per sub-element
		[0] = {1,1,1,1},		// #vertices of vertices
		[1] = {2,2,2,2,2,2},		// #vertices of edges
		[2] = {3,3,3,3},	       	// #vertices of trinagles
		[3] = {4}			// #vertices of tets
	},
	{	// map sub-elements to vertices
		[0] = {{0}, {1}, {2}, {3}},		 	  // 4 vertices
		[1] = {{0,1}, {0,2}, {1,2}, {0,3}, {1,3}, {2,3}}, // 6 edges
		[2] = {{0,1,2}, {0,1,3}, {0,2,3}, {1,2,3}},	  // 4 triangles
		[3] = {{0,1,2,3}}      				  // 1 tets
	},
	{	// edges connected to vertex
		[0] = {0,1,3},
		[1] = {0,2,4},
		[2] = {1,2,5},
		[3] = {3,4,5}
	},
	{	// triangles connected to vertex
		[0] = {0,1,2},
		[1] = {0,1,3},
		[2] = {0,2,3},
		[3] = {1,2,3}
	},
	{	// coordinates
		{0.0, 0.0, 0.0},
		{1.0, 0.0, 0.0},
		{0.0, 1.0, 0.0},
		{0.0, 0.0, 1.0}
	}
};
