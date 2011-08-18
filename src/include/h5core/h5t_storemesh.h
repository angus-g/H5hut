#ifndef __H5T_STOREMESH_H
#define __H5T_STOREMESH_H

#ifdef __cplusplus
extern "C" {
#endif

h5t_lvl_idx_t
h5t_add_level (h5t_mesh_t* const);

h5_err_t
h5t_begin_store_vertices (h5t_mesh_t* const, const h5_size_t);

h5_loc_id_t
h5t_store_vertex (h5t_mesh_t* const, const h5_glb_id_t, const h5_float64_t[3]);

h5_err_t
h5t_end_store_vertices (h5t_mesh_t* const);

h5_err_t
h5t_begin_store_elems (h5t_mesh_t* const, const h5_size_t);

h5_loc_idx_t
h5t_store_elem (h5t_mesh_t* const, const h5_loc_idx_t, const h5_loc_idx_t*);

h5_err_t
h5t_end_store_elems (h5t_mesh_t* const);

h5_err_t
h5t_begin_refine_elems (h5t_mesh_t* const);

h5_err_t
h5t_refine_marked_elems (h5t_mesh_t* const);

h5_err_t
h5t_end_refine_elems (h5t_mesh_t* const);

h5_err_t
h5t_mark_entity (h5t_mesh_t* const, const h5_loc_id_t);

h5_err_t
h5t_pre_refine (h5t_mesh_t* const);

h5_err_t
h5t_refine (h5t_mesh_t* const);

h5_err_t
h5t_post_refine (h5t_mesh_t* const);

#ifdef __cplusplus
}
#endif

#endif
