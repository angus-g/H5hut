#ifndef __H5T_TAGS_H 
#define __H5T_TAGS_H

#ifdef __cplusplus
extern "C" {
#endif

typedef struct h5t_tagset h5t_tagset_t;
typedef struct h5t_tagcontainer h5t_tagcontainer_t;

h5_ssize_t
h5t_get_num_mtagsets (h5t_mesh_t* const);

h5_err_t
h5t_get_mtagset_info (h5t_mesh_t* const, const h5_size_t, char[],
		      const h5_size_t, h5_int64_t* const);

h5_err_t
h5t_mtagset_exists (h5t_mesh_t* const, const char* const);

h5_err_t
h5t_create_mtagset (h5t_mesh_t* const, const char[], const h5_id_t, h5t_tagset_t**);

h5_err_t
h5t_open_mtagset (h5t_mesh_t* const, const char* const, h5t_tagset_t**);

h5_err_t
h5t_close_mtagset (h5t_tagset_t*);

h5_err_t
h5t_remove_mtagset (h5t_mesh_t* const, const char[]);

h5_err_t
h5t_set_tag (h5t_tagset_t* const, const h5_loc_id_t, const h5_size_t, void*);

h5_loc_id_t
h5t_get_tag (const h5t_tagset_t*, const h5_loc_id_t, h5_size_t* const, void* const);

h5_err_t
h5t_remove_tag (h5t_tagset_t*, const h5_loc_id_t);

#ifdef __cplusplus
}
#endif

#endif
