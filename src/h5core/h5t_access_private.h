#ifndef __H5T_ACCESS_PRIVATE_H
#define __H5T_ACCESS_PRIVATE_H

extern struct h5t_access_methods h5tpriv_access_trim_methods;
extern struct h5t_access_methods h5tpriv_access_tetm_methods;

#define h5tpriv_get_loc_elem(f, elem_idx)			\
	(*f->t->methods.access->get_loc_elem)(f, elem_idx)

#define h5tpriv_get_loc_elem_idx(f, elem_idx)			\
	(*f->t->methods.access->get_loc_elem_idx)(f, elem_idx)

#define h5tpriv_set_loc_elem_idx(f, elem_idx, idx)		\
	(*f->t->methods.access->set_loc_elem_idx)(f, elem_idx, idx)

#define h5tpriv_get_loc_elem_parent_idx(f, elem_idx)			\
	(*f->t->methods.access->get_loc_elem_parent_idx)(f, elem_idx)

#define h5tpriv_set_loc_elem_parent_idx(f, elem_idx, idx)		\
	(*f->t->methods.access->set_loc_elem_parent_idx)(f, elem_idx, idx)

#define h5tpriv_get_loc_elem_child_idx(f, elem_idx)			\
	(*f->t->methods.access->get_loc_elem_child_idx)(f, elem_idx)

#define h5tpriv_set_loc_elem_child_idx(f, elem_idx, idx)		\
	(*f->t->methods.access->set_loc_elem_child_idx)(f, elem_idx, idx)

#define h5tpriv_get_loc_elem_level_idx(f, elem_idx)			\
	(*f->t->methods.access->get_loc_elem_level_idx)(f, elem_idx)

#define h5tpriv_set_loc_elem_level_idx(f, elem_idx, idx)		\
	(*f->t->methods.access->set_loc_elem_level_idx)(f, elem_idx, idx)

#define h5tpriv_get_loc_elem_vertex_indices(f, elem_idx)		\
	(*f->t->methods.access->get_loc_elem_vertex_indices)(f, elem_idx)

#define h5tpriv_get_loc_elem_vertex_idx(f, elem_idx, face_idx)	\
	(*f->t->methods.access->get_loc_elem_vertex_idx)(f, elem_idx, face_idx)

#define h5tpriv_set_loc_elem_vertex_idx(f, elem_idx, face_idx, idx)	\
	(*f->t->methods.access->get_loc_elem_vertex_idx)(f, elem_idx, face_idx, idx)

#define h5tpriv_get_loc_elem_neighbor_indices(f, elem_idx)		\
	(*f->t->methods.access->get_loc_elem_neighbor_indices)(f, elem_idx)

#define h5tpriv_get_loc_elem_neighbor_idx(f, elem_idx, face_idx)	\
	(*f->t->methods.access->get_loc_elem_neighbor_idx)(f, elem_idx, face_idx)

#define h5tpriv_set_loc_elem_neighbor_idx(f, elem_idx, face_idx, idx)	\
	(*f->t->methods.access->get_loc_elem_neighbor_idx)(f, elem_idx, face_idx, idx)

#define h5tpriv_get_glb_elem(f, elem_idx)			\
	(*f->t->methods.access->get_glb_elem)(f, elem_idx)

#define h5tpriv_get_glb_elem_idx(f, elem_idx)			\
	(*f->t->methods.access->get_glb_elem_idx)(f, elem_idx)

#define h5tpriv_set_glb_elem_idx(f, elem_idx, idx)		\
	(*f->t->methods.access->set_glb_elem_idx)(f, elem_idx, idx)

#define h5tpriv_get_glb_elem_parent_idx(f, elem_idx)			\
	(*f->t->methods.access->get_glb_elem_parent_idx)(f, elem_idx)

#define h5tpriv_set_glb_elem_parent_idx(f, elem_idx, idx)		\
	(*f->t->methods.access->set_glb_elem_parent_idx)(f, elem_idx, idx)

#define h5tpriv_get_glb_elem_child_idx(f, elem_idx)			\
	(*f->t->methods.access->get_glb_elem_child_idx)(f, elem_idx)

#define h5tpriv_set_glb_elem_child_idx(f, elem_idx, idx)		\
	(*f->t->methods.access->set_glb_elem_child_idx)(f, elem_idx, idx)

#define h5tpriv_get_glb_elem_level_idx(f, elem_idx)			\
	(*f->t->methods.access->get_glb_elem_level_idx)(f, elem_idx)

#define h5tpriv_set_glb_elem_level_idx(f, elem_idx, idx)		\
	(*f->t->methods.access->set_glb_elem_level_idx)(f, elem_idx, idx)

#define h5tpriv_get_glb_elem_vertex_indices(f, elem_idx)		\
	(*f->t->methods.access->get_glb_elem_vertex_indices)(f, elem_idx)

#define h5tpriv_get_glb_elem_vertex_idx(f, elem_idx, face_idx)	\
	(*f->t->methods.access->get_glb_elem_vertex_idx)(f, elem_idx, face_idx)

#define h5tpriv_set_glb_elem_vertex_idx(f, elem_idx, face_idx, idx)	\
	(*f->t->methods.access->get_glb_elem_vertex_idx)(f, elem_idx, face_idx, idx)

#define h5tpriv_get_glb_elem_neighbor_indices(f, elem_idx)		\
	(*f->t->methods.access->get_glb_elem_neighbor_indices)(f, elem_idx)

#define h5tpriv_get_glb_elem_neighbor_idx(f, elem_idx, face_idx)	\
	(*f->t->methods.access->get_glb_elem_neighbor_idx)(f, elem_idx, face_idx)

#define h5tpriv_set_glb_elem_neighbor_idx(f, elem_idx, face_idx, idx)	\
	(*f->t->methods.access->get_glb_elem_neighbor_idx)(f, elem_idx, face_idx, idx)


#endif
