#ifndef __H5B_ERRORHANDLING_PRIVATE_H
#define __H5B_ERRORHANDLING_PRIVATE_H

#define CHECK_LAYOUT( f )			\
	if ( ! f->block->have_layout )		\
		return h5_error(		\
			H5PART_ERR_LAYOUT,	\
			"No layout defined." )

#define HANDLE_H5_LAYOUT_ERR \
	h5_error( \
		H5PART_ERR_LAYOUT, \
		"Bad layout." );

#endif
