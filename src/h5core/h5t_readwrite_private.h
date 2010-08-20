#ifndef __H5T_READWRITE_PRIVATE_H
#define __H5T_READWRITE_PRIVATE_H

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

extern struct h5t_read_methods h5tpriv_read_trim_methods;
extern struct h5t_read_methods h5tpriv_read_tetm_methods;

static inline h5_err_t
h5tpriv_init_loc_elems_struct (
	h5_file_t* const f
	) {
	return (*f->t->methods.read->init_loc_elems_struct) (f);
}

#endif
