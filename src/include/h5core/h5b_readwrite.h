#ifndef __H5B_READWRITE_H
#define __H5B_READWRITE_H

#ifdef __cplusplus
extern "C" {
#endif

h5_err_t
h5b_write_scalar_data (
	h5_file_t *const f,		/*!< IN: file handle */
	const char *field_name,		/*!< IN: name of field */
	const void *data,		/*!< IN: data to write */
	const hid_t type		/*!< IN: data type */
	);

h5_err_t
h5b_write_vector3d_data (
	h5_file_t *const f,		/*!< IN: file handle */
	const char *field_name,		/*!< IN: name of field */
	const void *xdata,		/*!< IN: x data to write */
	const void *ydata,		/*!< IN: y data to write */
	const void *zdata,		/*!< IN: z data to write */
	const hid_t type		/*!< IN: data type */
	);

h5_err_t
h5b_read_scalar_data (
	h5_file_t *const f,		/*!< IN: file handle */
	const char *field_name,		/*!< IN: name of field */
	void *data,			/*!< OUT: read bufer */
	const hid_t type		/*!< IN: data type */
	);

h5_err_t
h5b_read_vector3d_data (
	h5_file_t *const f,		/*!< IN: file handle */
	const char *field_name,		/*!< IN: name of field */
	void *xdata,			/*!< IN: x data to write */
	void *ydata,			/*!< IN: y data to write */
	void *zdata,			/*!< IN: z data to write */
	const hid_t type		/*!< IN: data type */
	);

#ifdef __cplusplus
}
#endif

#endif
