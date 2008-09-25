#ifndef __H5U_ERRORHANDLING_PRIVATE_H
#define __H5U_ERRORHANDLING_PRIVATE_H

#define HANDLE_H5_SET_VIEW_ERR( rc, start, end ) \
	h5_error( \
		rc, \
		"Cannot set view to (%lld, %lld).", \
		(long long)start, (long long)end );

#define HANDLE_H5_GET_NUM_PARTICLES_ERR( rc )		\
	h5_error(					\
		rc,					\
		"Cannot get number of particles." );

#endif
