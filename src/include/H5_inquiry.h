#ifndef __H5_INQUIRY_H
#define __H5_INQUIRY_H

int
H5GetNumNodes (
	h5_file_t * const f
	);

h5_size_t
H5GetNumSteps (
	h5_file_t * const f
	);

h5_err_t
H5HasStep (
	h5_file_t * const f,
	h5_id_t step
	);
#endif
