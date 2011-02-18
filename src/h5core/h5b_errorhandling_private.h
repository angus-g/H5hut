#ifndef __H5B_ERRORHANDLING_PRIVATE_H
#define __H5B_ERRORHANDLING_PRIVATE_H

#define CHECK_LAYOUT( f )			\
	if (! f->b->have_layout)		\
		return h5_error(		\
			H5_ERR_LAYOUT,		\
			"No view has been defined!")

#define HANDLE_H5_LAYOUT_ERR( f )			\
	h5_error(					\
		H5_ERR_LAYOUT,				\
		"Bad view!");

#endif
