#ifndef __H5T_TAGS_H
#define __H5T_TAGS_H

typedef struct h5t_tagset H5T_Tagset;

h5_err_t h5t_add_mtagset ( h5_file_t* const f, char name[], h5_id_t id );
h5_err_t h5t_remove_mtagset ( h5_file_t* const f, const char name[] );
h5_err_t h5t_open_mtagset ( h5_file_t* const f, const char *name,
			    H5T_Tagset** retval );
h5_size_t h5t_get_num_mtagsets ( h5_file_t* const f );
h5_size_t h5t_get_mtagsets ( h5_file_t* const f, char** names[] );
h5_err_t h5t_get_mtagset_info ( h5_file_t* const f, const h5_id_t idx,
				char** names, h5_id_t* type );
h5_id_t h5t_get_mtagset_type_by_name ( h5_file_t* const f, char name[] );

h5_err_t h5t_set_mtag_by_name ( h5_file_t* const f, char name[], const h5_id_t id,
				const size_t dim, void* value );

h5_err_t h5t_get_mtag_by_name ( h5_file_t* const f, const char name[],
				const h5_id_t id, size_t* dim, void* vals );

h5_err_t h5t_remove_mtag_by_name ( h5_file_t* const f, const char name[],
			   const h5_id_t id );

#endif
