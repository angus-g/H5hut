#ifndef __H5_TYPES_H
#define __H5_TYPES_H

#ifdef   WIN32
typedef __int64			int64_t;
#endif /* WIN32 */

typedef int64_t			h5_int64_t;
typedef	h5_int64_t		h5_int_t;
typedef	h5_int64_t		h5part_int64_t;
typedef int32_t			h5_id_t;
typedef int32_t			h5_size_t;	/* size in number of elements */

typedef double			h5_float64_t;
typedef	h5_float64_t		h5_float_t;
typedef	h5_float64_t		h5part_float64_t;

typedef h5_int64_t (*h5_error_handler)( const char*, const h5_int64_t, const char*,...)
#ifdef __GNUC__
__attribute__ ((format (printf, 3, 4)))
#endif
 ;

typedef h5_error_handler	h5part_error_handler;

#ifndef PARALLEL_IO
typedef unsigned long		MPI_Comm;
#endif


#endif
