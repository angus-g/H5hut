#ifndef __H5T_MODEL_H
#define __H5T_MODEL_H

#ifdef __cplusplus
extern "C" {
#endif

h5_err_t h5t_open_tetrahedral_mesh (h5_file_t* const, const char*, h5t_mesh_t**);
h5_err_t h5t_open_tetrahedral_mesh_by_idx (h5_file_t* const, const h5_id_t, h5t_mesh_t**);

h5_err_t h5t_open_triangle_mesh (h5_file_t* const, const char*,	h5t_mesh_t**);
h5_err_t h5t_open_triangle_mesh_by_idx (h5_file_t* const, const h5_id_t, h5t_mesh_t**);


h5_id_t h5t_add_tetrahedral_mesh (h5_file_t* const, const char* name, h5t_mesh_t**);

h5_id_t h5t_add_triangle_mesh (h5_file_t* const, const char*, h5t_mesh_t**);

h5_err_t h5t_set_level (h5t_mesh_t* const, const h5t_lvl_idx_t);

h5_err_t h5t_close_mesh (h5t_mesh_t* const);

#ifdef __cplusplus
}
#endif

#endif
