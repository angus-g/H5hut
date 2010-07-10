#ifndef __H5B_MODEL_PRIVATE_H
#define __H5B_MODEL_PRIVATE_H

h5_err_t
h5bpriv_have_field_group (
	h5_file_t *const f,			/*!< IN: file handle */
	const char *name
	);

h5_err_t
h5bpriv_open_field_group (
	h5_file_t *const f,			/*!< IN: file handle */
	const char *name
	);

h5_err_t
h5bpriv_open_block_group (
	h5_file_t *const f			/*!< IN: file handle */
	);

h5_err_t
h5bpriv_release_hyperslab (
	h5_file_t *const f			/*!< IN: file handle */
	);

#endif
