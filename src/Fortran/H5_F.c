#include <stdlib.h>
#include <string.h>

#include "h5core/h5_core.h"
#include "Underscore.h"

#if defined(F77_SINGLE_UNDERSCORE)
#define F77NAME(a,b) a
#elif defined(F77_CRAY_UNDERSCORE)
#define F77NAME(a,b) b
#elif defined(F77_NO_UNDERSCORE)
#else
#error Error, no way to determine how to construct fortran bindings
#endif

#if ! defined(F77_NO_UNDERSCORE)

#define h5_openr F77NAME (						\
					h5_openr_,			\
					H5_OPENR )
#define h5_openw F77NAME (						\
					h5_openw_,			\
					H5_OPENW )
#define h5_opena F77NAME (						\
					h5_opena_,			\
					H5_OPENA )
#define h5_openr_par F77NAME (						\
					h5_openr_par_,			\
					H5_OPENR_PAR )
#define h5_openw_par F77NAME (						\
					h5_openw_par_,			\
					H5_OPENW_PAR )
#define h5_opena_par F77NAME (						\
					h5_opena_par_,			\
					H5_OPENA_PAR )
#define h5_close F77NAME (						\
					h5_close_,			\
					H5_CLOSE) 
#define h5_check F77NAME (						\
					h5_check_,			\
					H5_CHECK) 
#define h5_setstep F77NAME (						\
					h5_setstep_,			\
					H5_SETSTEP )
#define h5_getnsteps F77NAME (						\
					h5_getnsteps_,			\
					H5_GETNSTEPS )
#define h5_set_verbosity_level F77NAME (				\
					h5_set_verbosity_level_,	\
					H5_SET_VERBOSITY_LEVEL )

#endif

static h5_int32_t
_flagsfor2c (
	char * flags
	) {

	h5_int32_t fbits = 0x00;

	flags = strtok ( flags, "," );
	while ( flags != NULL ) {
		if ( strcmp ( flags, "vfd_mpiposix" ) == 0 )
				fbits |= H5_VFD_MPIPOSIX;
		else if ( strcmp ( flags, "vfd_independent" ) == 0 )
				fbits |= H5_VFD_INDEPENDENT;
		flags = strtok ( NULL, "," );
	}

	return fbits;
}

/* open/close interface */
h5_err_t
h5_openr (
	const char *file_name,
	const int l_file_name
	) {

	char *file_name2 = h5_strdupfor2c ( file_name, l_file_name );
	h5_file_t* f = h5_open_file ( file_name2, H5_O_RDONLY, 0 );
	free ( file_name2 );
	return (h5_int64_t)(size_t)f; 
}

h5_err_t
h5_openw (
	const char *file_name,
	const int l_file_name
	) {

	char *file_name2 = h5_strdupfor2c ( file_name, l_file_name );
	h5_file_t* f = h5_open_file ( file_name2, H5_O_WRONLY, 0 );
	free ( file_name2 );
	return (h5_int64_t)(size_t)f; 
}

h5_err_t
h5pt_opena (
	const char *file_name,
	const int l_file_name
	) {
	
	char *file_name2 = h5_strdupfor2c ( file_name, l_file_name );
	h5_file_t* f = h5_open_file ( file_name2, H5_O_APPEND, 0 );
	free ( file_name2 );
	return (h5_int64_t)(size_t)f;
}

#ifdef PARALLEL_IO
h5_err_t
h5_openr_par (
	const char *file_name,
	MPI_Fint *fcomm,
	const char *flags,
	const int l_file_name,
	const int l_flags
	) {

	MPI_Comm ccomm = MPI_Comm_f2c (*fcomm);
	char *file_name2 = h5_strdupfor2c ( file_name, l_file_name );
	char *flags2 = h5_strdupfor2c ( flags, l_flags );

	h5_int32_t fbits = H5_O_RDONLY | _flagsfor2c ( flags2 );

	h5_file_t* f = h5_open_file ( file_name2, fbits, ccomm );

	free ( file_name2 );
	free ( flags2 );
	return (h5_int64_t)(size_t)f; 
}

h5_err_t
h5_openw_par (
	const char *file_name,
	MPI_Fint *fcomm,
	const char *flags,
	const int l_file_name,
	const int l_flags
	) {

	MPI_Comm ccomm = MPI_Comm_f2c (*fcomm);
	char *file_name2 = h5_strdupfor2c ( file_name, l_file_name );
	char *flags2 = h5_strdupfor2c ( flags, l_flags );

	h5_int32_t fbits = H5_O_WRONLY | _flagsfor2c ( flags2 );

	h5_file_t* f = h5_open_file ( file_name2, ccomm, fbits );

	free ( file_name2 );
	free ( flags2 );
	return (h5_int64_t)(size_t)f; 
}

h5_err_t
h5pt_opena_par_align (
	const char *file_name,
	MPI_Fint *fcomm,
	const h5_int64_t *align,
	const char *flags,
	const int l_file_name,
	const int l_flags
	) {
	
	MPI_Comm ccomm = MPI_Comm_f2c (*fcomm);
	char *file_name2 = h5_strdupfor2c ( file_name, l_file_name );
	char *flags2 = h5_strdupfor2c ( flags, l_flags );
       
	h5_int32_t fbits = H5_O_APPEND | _flagsfor2c ( flags2 );

	h5_file_t* f = h5_open_file( file_name2, ccomm, fbits );

	free ( file_name2 );
	free ( flags2 );
	return (h5_int64_t)(size_t)f;
}
#endif

h5_err_t
h5_close (
	const h5_int64_t *f
	) {

	h5_file_t *filehandle = (h5_file_t*)(size_t)*f;
	h5_set_funcname( filehandle, __func__ );
	return h5_close_file ( filehandle );
}

h5_err_t
h5_check (
	const h5_int64_t *f
	) {

	h5_file_t *filehandle = (h5_file_t*)(size_t)*f;
	h5_set_funcname( filehandle, __func__ );
	return h5_check_filehandle ( filehandle );
}

h5_err_t
h5_setstep (
	const h5_int64_t *f,
	h5_int64_t *step ) {

	h5_file_t *filehandle = (h5_file_t*)(size_t)*f;
	h5_set_funcname( filehandle, __func__ );
	return h5_set_step ( filehandle, (*step)-1 );
}

h5_ssize_t
h5_getnsteps (
	const h5_int64_t *f
	) {

	h5_file_t *filehandle = (h5_file_t*)(size_t)*f;
	h5_set_funcname( filehandle, __func__ );
	return h5_get_num_steps ( filehandle );
}

h5_err_t
h5_set_verbosity_level (
	const h5_int64_t *level
	) {
	return h5_set_debuglevel ( *level );
}

