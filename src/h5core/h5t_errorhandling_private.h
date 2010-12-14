#ifndef __H5T_ERRORHANDLING_PRIVATE_H
#define __H5T_ERRORHANDLING_PRIVATE_H

#define ERR_ELEM_NEXIST  "Element with local vertex IDs (%s) doesn't exist!"

static inline h5_err_t
h5tpriv_error_local_elem_nexist (
	h5_file_t * const f,
	h5_loc_idx_t vertex_indices[]
	) {
	h5t_fdata_t* t = f->t;
	char s[1024];

	int num_chars_printed = snprintf (s, sizeof(s), "%lld,",
					  (long long)vertex_indices[0]);
	int i;
	int num_vertices = h5tpriv_ref_elem_get_num_vertices (t);
	for (i = 1; i < num_vertices; i++) {
		num_chars_printed += snprintf (
			s + num_chars_printed, sizeof (s) - num_chars_printed,
			"%lld,", (long long)vertex_indices[i]);
		if ((sizeof (s) - num_chars_printed) < 32) {
			// buffer to small
			return h5_error_internal (f, __FILE__, __func__, __LINE__);
		}
	}

	return h5_error (f, H5_ERR_NOENTRY, ERR_ELEM_NEXIST, s);
}

static inline h5_err_t
h5tpriv_inval_codim (
	h5_file_t * const f,
	int codim,
	int min_codim,
	int max_codim
	) {
	return h5_error (f, H5_ERR_INVAL,
			 "Co-dimension %d requested, "
			 "but must be between %d and %d",
			 codim, min_codim, max_codim);
}

#define h5tpriv_error_undef_mesh( f )		\
	h5_error(				\
		f,				\
		H5_ERR_INVAL,			\
		"Mesh not yet defined." );

#define h5tpriv_error_undef_level( f )	\
	h5_error(				\
		f,				\
		H5_ERR_INVAL,			\
		"Level not defined." );


#define h5tpriv_error_nexist_level( f, level_id )	\
	h5_error(				\
		f,				\
		H5_ERR_INVAL,			\
		"Level %lld doesn't exist.", (long long)level_id );

#define h5tpriv_error_global_id_nexist( f, name, id )		\
	h5_error(						\
		f,						\
		H5_ERR_NOENTRY,					\
		"%s with global id %lld does not exist!",	\
		name, (long long)id );


#define h5tpriv_error_global_triangle_id_nexist( f, vids )			\
	h5_error(							\
		f,							\
		H5_ERR_NOENTRY,						\
		"Triangle with global vertex ids (%lld,%lld,%lld) doesn't exist!", \
		(long long)vids[0], (long long)vids[1], (long long)vids[2] );

#define h5tpriv_error_local_triangle_nexist( f, indices )	\
	h5_error(							\
		f,							\
		H5_ERR_NOENTRY,						\
		"Triangle with global vertex ids (%lld,%lld,%lld) doesn't exist!", \
		(long long)indices[0], (long long)indices[1], (long long)indices[2] );


#define h5tpriv_error_store_boundaryface_local_id( f, local_fid )	     \
	h5_error(						     \
		f,						     \
		H5_ERR_INVAL,					     \
		"Boundary face with local id %lld is not on level 0!", \
		(long long)local_fid );

#endif
