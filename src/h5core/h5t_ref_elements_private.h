#ifndef __H5T_REF_ELEMENTS_PRIVATE_H
#define __H5T_REF_ELEMENTS_PRIVATE_H

#define h5tpriv_ref_elem_get_num_vertices(this) (this->ref_elem->num_faces[0])

#define h5tpriv_ref_elem_get_num_edges(this) (this->ref_elem->num_faces[1])

#define h5tpriv_ref_elem_get_num_facets(this)			\
	(this->ref_elem->num_faces[this->ref_elem->dim - 1])

#define h5tpriv_ref_elem_get_num_faces(this, dim) (this->ref_elem->num_faces[dim])

#define h5tpriv_ref_elem_get_dim(this) (this->ref_elem->dim)

#define h5tpriv_ref_elem_get_entity_type(this,dim)	\
	(this->ref_elem->entity_types[dim])

#define h5tpriv_ref_elem_get_vertex_idx(this, dim, face_idx, i)	\
	 (this->ref_elem->connect[dim][0][face_idx][i])

#define h5tpriv_ref_elem_get_edge_idx(this, dim, face_idx, i)	\
	 (this->ref_elem->connect[dim][1][face_idx][i])

#define h5tpriv_ref_elem_get_triangle_idx(this, dim, face_idx, i)	\
	 (this->ref_elem->connect[dim][2][face_idx][i])

#endif
