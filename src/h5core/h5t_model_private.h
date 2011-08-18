#ifndef __H5T_MODEL_PRIVATE_H
#define __H5T_MODEL_PRIVATE_H

h5_err_t h5tpriv_alloc_num_vertices (h5t_mesh_t* const, const h5_size_t);
h5_err_t h5tpriv_alloc_tris (h5t_mesh_t* const, const size_t, const size_t);
h5_err_t h5tpriv_alloc_tets (h5t_mesh_t* const,	const size_t, const size_t);
h5_err_t h5tpriv_init_mesh (h5t_mesh_t* const, h5_file_t* const, const char*, const hid_t);

#endif