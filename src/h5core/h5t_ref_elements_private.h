#ifndef __H5T_REF_ELEMENTS_PRIVATE_H
#define __H5T_REF_ELEMENTS_PRIVATE_H

#define h5tpriv_ref_elem_get_num_vertices(this) (this->ref_elem->num_faces[0])
#define h5tpriv_ref_elem_get_num_edges(this) (this->ref_elem->num_faces[1])
#define h5tpriv_ref_elem_get_num_triangles(this) (this->ref_elem->num_faces[2])
#define h5tpriv_ref_elem_get_num_faces(this, dim) (this->ref_elem->num_faces[dim])

#define h5tpriv_ref_elem_get_dim(this) (this->ref_elem->dim)

#endif
