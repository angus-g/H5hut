#include "h5core/h5_core.h"
#include "h5_core_private.h"

/*
  information we can retrieve from the reference element:
  - number of vertices per sub-entity (1)
  - vertex indices of all sub-elements
  - upward connectivity
    - which co-dim 1 entities are connected to a given vertex
    - which co-dim 2 entities are connected to a given vertex
    - which co-dim 1 entities are connected to a given co-dim 2 entity
  - downward connectivity
  - vertex coordinates
  
get_conn(face, dim, dim) -> list

for dim 3 reference elements:
get_conn(0, 3, 0) -> 0, 1, 2, 3 (all vertices)
get_conn(0, 3, 1) -> 0, 1, 2, 3, 4, 5 (all edges)
get_conn(0, 3, 2) -> 0, 1, 2, 3 (all facets)
get_conn(0, 3, 3) -> 0
get_conn(i, 2, 0) -> vertices  of facet i
get_conn(i, 2, 1) -> edges of facet i
get_conn(i, 2, 2) -> facets connected to facet i 
get_conn(i, 2, 3) -> 0 
get_conn(i, 1, 0) -> vertices of edge i
get_conn(i, 1, 1) -> edges connected to edge i
get_conn(i, 1, 2) -> facets connected to edge i
get_conn(i, 1, 3) -> 0
get_conn(i, 0, 0) -> i
get_conn(i, 0, 1) -> edges connected to vertex i
get_conn(i, 0, 2) -> facets connected to vertex i
get_conn(i, 0, 3) -> 0

(1) For triangle and tetrahedral meshes the number of vertices is 
identical for all sub-entities within the same dimension. This is not 
true for other grids (like prismen)

  
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
		H5T_TYPE_VERTEX,
		H5T_TYPE_EDGE,
		H5T_TYPE_TRIANGLE,
	},
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
	{	// connectivity
		// vertices to vertices
		[0][0] = {{0},     {1},     {2}},
		// vertices to edges
		[0][1] = {{0,1},   {0,2},   {1,2}},
		// vertices to triangles
		[0][2] = {{0},     {0},     {0}},
		// edges to vertices
		[1][0] = {{0,1},   {0,2},   {1,2}},
		// edges to edges
		[1][1] = {{0,1,2}, {0,1,2}, {0,1,2}},
		// edges to triangle
		[1][2] = {{0},     {0},     {0}},
		// triangle to vertices
		[2][0] = {{0,1,2}}
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
		H5T_TYPE_VERTEX,
		H5T_TYPE_EDGE,
		H5T_TYPE_TRIANGLE,
		H5T_TYPE_TET,
	},
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
	{	// connectivity
		// vertex to vertices
		[0][0] = {{0},     {1},     {2},   {3}},
		// vertex to edges
		[0][1] = {{0,1,3}, {0,2,4}, {1,2,5}, {3,4,5}},
		// vertex to triangles
		[0][2] = {{0,1,2}, {0,1,3}, {0,2,3}, {1,2,3}},
		// vertex to tetrahedron
		[0][3] = {{0},     {0},     {0},     {0}},
		// edge to vertices
		[1][0] = {{0,1},   {0,2},   {1,2},   {0,3},   {1,3},   {2,3}}, 
		// edge to edges
		[1][1] = {{0,1,2,3,4}, {0,1,2,3,5}, {0,1,2,4,5},
			  {0,1,3,4,5}, {0,2,3,4,5}, {1,2,3,4,5}},
		// edge to triangle
		[1][2] = {{0,1},   {0,2},   {0,3},   {1,2},   {1,3},   {2,3}},
		// edge to tetrahedra
		[1][3] = {{0},     {0},     {0},     {0},     {0},     {0}},
		// triangle to vertices
		[2][0] = {{0,1,2}, {0,1,3}, {0,2,3}, {1,2,3}},
		// triangle to edges
		[2][1] = {{0,1,2}, {0,3,4}, {1,3,5}, {2,4,5}},
		// triangle to triangle
		[2][2] = {{0,1,2,3}, {0,1,2,3}, {0,1,2,3}, {0,1,2,3}},
		// tetrahedron to tetrahedron
		[2][3] = {{0}},
		// tetrahedron to vertices
		[3][0] = {{0,1,2,3}},
		// tetrahedron to edges
		[3][1] = {{0,1,2,3,4,5}},
		// tetrahedron to triangles
		[3][2] = {{0,1,2,3}},
		// tetrahedron to tetrahedron
		[3][2] = {{0}}
	},
	{	// coordinates
		{0.0, 0.0, 0.0},
		{1.0, 0.0, 0.0},
		{0.0, 1.0, 0.0},
		{0.0, 0.0, 1.0}
	}
};
