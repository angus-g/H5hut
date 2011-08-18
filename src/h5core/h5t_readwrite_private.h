#ifndef __H5T_READWRITE_PRIVATE_H
#define __H5T_READWRITE_PRIVATE_H

struct h5t_read_methods {
	h5_err_t (*init_loc_elems_struct)(h5t_mesh_t* const, h5t_lvl_idx_t);
	h5_err_t (*init_geom_boundary_info)(h5t_mesh_t* const, h5t_lvl_idx_t);
	h5_err_t (*alloc_glb_elems_struct)(h5t_mesh_t* const, h5_loc_idx_t);
	h5_err_t (*init_glb2loc_elem_map)(h5t_mesh_t* const);
	h5_err_t (*init_glb_elems_struct)(h5t_mesh_t* const);
};

extern struct h5t_read_methods h5tpriv_read_trim_methods;
extern struct h5t_read_methods h5tpriv_read_tetm_methods;

h5_err_t
h5tpriv_write_obj (
	h5t_mesh_t* m,
	const hid_t gid,
	const hsize_t current_dims,
	const hsize_t max_dims,
	const hid_t tid,
	const void* const object,
	const char* const dsname
	);

h5_err_t
h5tpriv_read_mesh (
	h5t_mesh_t* const m
	);

h5_err_t
h5tpriv_write_mesh (
	h5t_mesh_t* const m
	);

static inline h5_err_t
h5tpriv_init_loc_elems_struct (
	h5t_mesh_t* const m,
	const h5t_lvl_idx_t from_lvl
	) {
	return (*m->methods.read->init_loc_elems_struct) (m, from_lvl);
}

static inline h5_err_t
h5tpriv_init_geom_boundary_info (
	h5t_mesh_t* const m,
	const h5t_lvl_idx_t from_lvl
	) {
	return (*m->methods.read->init_geom_boundary_info) (m, from_lvl);
}

static inline h5_err_t
h5tpriv_alloc_glb_elems_struct (
	h5t_mesh_t* const m,
	h5_loc_idx_t num_elems
	) {
	return (*m->methods.read->alloc_glb_elems_struct) (m, num_elems);
}

static inline h5_err_t
h5tpriv_init_glb2loc_elem_map (
	h5t_mesh_t* const m
	) {
	return (*m->methods.read->init_glb2loc_elem_map) (m);
}

static inline h5_err_t
h5tpriv_init_glb_elems_struct (
	h5t_mesh_t* const m
	) {
	return (*m->methods.read->init_glb_elems_struct) (m);
}

#endif
