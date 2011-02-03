#ifndef __H5T_READWRITE_PRIVATE_H
#define __H5T_READWRITE_PRIVATE_H

struct h5t_read_methods {
	h5_err_t (*init_loc_elems_struct)(h5_file_t* const, h5t_lvl_idx_t);
	h5_err_t (*init_geom_boundary_info)(h5_file_t* const, h5t_lvl_idx_t);
	h5_err_t (*alloc_glb_elems_struct)(h5_file_t* const, h5_loc_idx_t);
	h5_err_t (*init_glb2loc_elem_map)(h5_file_t* const);
	h5_err_t (*init_glb_elems_struct)(h5_file_t* const);
};

extern struct h5t_read_methods h5tpriv_read_trim_methods;
extern struct h5t_read_methods h5tpriv_read_tetm_methods;

h5_err_t
h5tpriv_write_obj (
	h5_file_t * f,
	const hid_t gid,
	const hsize_t current_dims,
	const hsize_t max_dims,
	const hid_t tid,
	const void * const object,
	const char * const dsname
	);

h5_err_t
h5tpriv_read_mesh (
	h5_file_t * const f
	);

h5_err_t
h5tpriv_write_mesh (
	h5_file_t * const f
	);

static inline h5_err_t
h5tpriv_init_loc_elems_struct (
	h5_file_t* const f,
	const h5t_lvl_idx_t from_lvl
	) {
	return (*f->t->methods.read->init_loc_elems_struct) (f, from_lvl);
}

static inline h5_err_t
h5tpriv_init_geom_boundary_info (
	h5_file_t* const f,
	const h5t_lvl_idx_t from_lvl
	) {
	return (*f->t->methods.read->init_geom_boundary_info) (f, from_lvl);
}

static inline h5_err_t
h5tpriv_alloc_glb_elems_struct (
	h5_file_t* const f,
	h5_loc_idx_t num_elems
	) {
	return (*f->t->methods.read->alloc_glb_elems_struct) (f, num_elems);
}

static inline h5_err_t
h5tpriv_init_glb2loc_elem_map (
	h5_file_t* const f
	) {
	return (*f->t->methods.read->init_glb2loc_elem_map) (f);
}

static inline h5_err_t
h5tpriv_init_glb_elems_struct (
	h5_file_t* const f
	) {
	return (*f->t->methods.read->init_glb_elems_struct) (f);
}

#endif
