
h5_size_t
h5_get_num_steps(

	return h5_get_num_hdf5_groups_matching_prefix (
		f,
		f->step_gid,
		f->prefix_step_name);
