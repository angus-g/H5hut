
h5_size_t
h5_get_num_steps(

	return hdf5_get_num_groups_matching_prefix (
		f,
		f->step_gid,
		f->prefix_step_name);
