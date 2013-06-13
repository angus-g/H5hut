/*
  Copyright (c) 2006-2013, The Regents of the University of California,
  through Lawrence Berkeley National Laboratory (subject to receipt of any
  required approvals from the U.S. Dept. of Energy) and the Paul Scherrer
  Institut (Switzerland).  All rights reserved.

  License: see file COPYING in top level of source distribution.
*/

#include "h5_private.h"
//#include "h5core/h5_model.h"

/* file handling interface */
static inline h5_int64_t
open_file (
        const char *name,
        const int l_name,
        h5_int32_t flags,
        MPI_Comm ccomm,
        h5_size_t align
        ) {
        char *name2 = h5_strdupfor2c ( name, l_name );
        h5_file_t* f = h5_open_file ( name2, flags, ccomm, align );
        free ( name2 );
        return (h5_int64_t)f;
}

#define h5_openr F77_NAME( \
                h5_openr,  \
                h5_openr_, \
                H5_OPENR)
h5_int64_t
h5_openr (
	const char *file_name,
	const int l_file_name
	) {
	H5_API_ENTER (h5_int64_t, "file_name='%s'",
                      file_name);
	H5_API_RETURN (open_file (file_name, l_file_name, H5_O_RDONLY, 0, 0));
}

#define h5_openw F77_NAME(                      \
                h5_openw,                       \
                h5_openw_,                      \
                H5_OPENW)
h5_int64_t
h5_openw (
	const char *file_name,
	const int l_file_name
	) {
	H5_API_ENTER (h5_int64_t, "file_name='%s'",
                      file_name);
	H5_API_RETURN (open_file (file_name, l_file_name, H5_O_WRONLY, 0, 0));
}

#define h5_opena F77_NAME(                      \
                h5_opena,                       \
                h5_opena_,                      \
                H5_OPENA)
h5_int64_t
h5_opena (
	const char *file_name,
	const int l_file_name
	) {
	H5_API_ENTER (h5_int64_t, "file_name='%s'",
                      file_name);
	H5_API_RETURN (open_file (file_name, l_file_name, H5_O_APPEND, 0, 0));
}

#define h5_openr_align F77_NAME(                \
                h5_openr_align,                 \
                h5_openr_align_,                \
                H5_OPENR_ALIGN)
h5_int64_t
h5_openr_align (
	const char *file_name,
	const h5_int64_t *align,
	const int l_file_name
	) {
	H5_API_ENTER (h5_int64_t, "file_name='%s', align=%llu",
                      file_name, (long long unsigned)align);
	H5_API_RETURN (open_file (file_name, l_file_name, H5_O_RDONLY, 0, *align));
}

#define h5_openw_align F77_NAME(                \
                h5_openw_align,                 \
                h5_openw_align_,                \
                H5_OPENW_ALIGN)
h5_int64_t
h5_openw_align (
	const char *file_name,
	const h5_int64_t *align,
	const int l_file_name
	) {
	H5_API_ENTER (h5_int64_t, "file_name='%s', align=%llu",
                      file_name, (long long unsigned)align);
	H5_API_RETURN (open_file (file_name, l_file_name, H5_O_WRONLY, 0, *align));
}

#define h5_opena_align F77_NAME(                \
                h5_opena_align,                 \
                h5_opena_align_,                \
                H5_OPENA_ALIGN)
h5_int64_t
h5_opena_align (
	const char *file_name,
	const h5_int64_t *align,
	const int l_file_name
	) {
	H5_API_ENTER (h5_int64_t, "file_name='%s', align=%llu",
                      file_name, (long long unsigned)align);
	H5_API_RETURN (open_file (file_name, l_file_name, H5_O_APPEND, 0, *align));
}

#ifdef PARALLEL_IO
h5_int32_t
flagsfor2c (
	const char* flags,
        const int l_flags
	) {
        if (flags == NULL)
                return 0;

	h5_int32_t  fbits = 0;
	char* flags2 = h5_strdupfor2c (flags, l_flags);

	flags2 = strtok (flags2, ",");
	while (flags != NULL) {
		if (strcmp (flags2, "vfd_mpiposix") == 0)
                        fbits |= H5_VFD_MPIPOSIX;
		else if (strcmp (flags2, "vfd_core") == 0)
                        fbits |= H5_VFD_CORE;
		else if (strcmp (flags2, "vfd_mpio_ind") == 0)
                        fbits |= H5_VFD_MPIIO_IND;
		else if (strcmp (flags2, "fs_lustre") == 0)
                        fbits |= H5_FS_LUSTRE;
                else {
                        // :FIXME: ignore unknown strings!?
                }
		flags2 = strtok (NULL, ",");
	}
        free (flags2);
	return fbits;
}

static inline h5_int64_t
open_file_par (
	const char* file_name,
	const int l_file_name,
	MPI_Fint* fcomm,
        const h5_int32_t mode,
	const char* flags,
	const int l_flags,
        h5_int64_t align
	) {
        return open_file (
                file_name, l_file_name,
                mode | flagsfor2c (flags, l_flags),
                MPI_Comm_f2c (*fcomm),
                align);
}

#define h5_openr_par F77_NAME(                  \
                h5_openr_par,                   \
                h5_openr_par_,                  \
                H5_OPENR_PAR)
h5_int64_t
h5_openr_par (
	const char *file_name,
	MPI_Fint *fcomm,
	const int l_file_name
	) {
	H5_API_ENTER (h5_int64_t, "file_name='%*s', fcomm=%d",
                      l_file_name, file_name, *fcomm);
        H5_API_RETURN (
                open_file_par (
                        file_name, l_file_name,
                        fcomm,
                        H5_O_RDONLY,
                        NULL, 0,
                        0));
}

#define h5_openw_par F77_NAME(                  \
                h5_openw_par,                   \
                h5_openw_par_,                  \
                H5_OPENW_PAR)
h5_int64_t
h5_openw_par (
	const char* file_name,
	MPI_Fint* fcomm,
	const int l_file_name
	) {
	H5_API_ENTER (h5_int64_t, "file_name='%*s', fcomm=%d",
                      l_file_name, file_name, *fcomm);
        H5_API_RETURN (
                open_file_par (
                        file_name, l_file_name,
                        fcomm,
                        H5_O_WRONLY,
                        NULL, 0,
                        0));
}

#define h5_opena_par F77_NAME(                  \
                h5_opena_par,                   \
                h5_opena_par_,                  \
                H5_OPENA_PAR)
h5_int64_t
h5_opena_par (
	const char* file_name,
	MPI_Fint* fcomm,
	const int l_file_name
	) {
	H5_API_ENTER (h5_int64_t, "file_name='%*s', fcomm=%d",
                      l_file_name, file_name, *fcomm);
        H5_API_RETURN (
                open_file_par (
                        file_name, l_file_name,
                        fcomm,
                        H5_O_APPEND,
                        NULL, 0,
                        0));
}

#define h5_openr_par_align F77_NAME(            \
                h5_openr_par_align,             \
                h5_openr_par_align_,            \
                H5_OPENR_PAR_ALIGN)
h5_int64_t
h5_openr_par_align (
	const char *file_name,
	MPI_Fint *fcomm,
	const h5_int64_t *align,
	const char *flags,
	const int l_file_name,
	const int l_flags
	) {
	H5_API_ENTER (h5_int64_t, "file_name='%*s', fcomm=%d, flags=%s, align=%lld",
                      l_file_name, file_name, *fcomm, flags, (long long)align);
        H5_API_RETURN (
                open_file_par (
                        file_name, l_file_name,
                        fcomm,
                        H5_O_RDONLY,
                        flags, l_flags,
                        *align));
}

#define h5_openw_par_align F77_NAME(            \
                h5_openw_par_align,             \
                h5_openw_par_align_,            \
                H5_OPENW_PAR_ALIGN)
h5_int64_t
h5_openw_par_align (
	const char *file_name,
	MPI_Fint *fcomm,
	const h5_int64_t *align,
	const char *flags,
	const int l_file_name,
	const int l_flags
	) {
	H5_API_ENTER (h5_int64_t, "file_name='%*s', fcomm=%d, flags=%s, align=%lld",
                      l_file_name, file_name, *fcomm, flags, (long long)align);
        H5_API_RETURN (
                open_file_par (
                        file_name, l_file_name,
                        fcomm,
                        H5_O_WRONLY,
                        flags, l_flags,
                        *align));
}

#define h5_opena_par_align F77_NAME(            \
                h5_opena_par_align,             \
                h5_opena_par_align_,            \
                H5_OPENA_PAR_ALIGN)
h5_int64_t
h5_opena_par_align (
	const char *file_name,
	MPI_Fint *fcomm,
	const h5_int64_t *align,
	const char *flags,
	const int l_file_name,
	const int l_flags
	) {
	H5_API_ENTER (h5_int64_t, "file_name='%*s', fcomm=%d, flags=%s, align=%lld",
                      l_file_name, file_name, *fcomm, flags, (long long)align);
        H5_API_RETURN (
                open_file_par (
                        file_name, l_file_name,
                        fcomm,
                        H5_O_APPEND,
                        flags, l_flags,
                        *align));
}
#endif

#define h5_close F77_NAME(                      \
                h5_close,                       \
                h5_close_,                      \
                H5_CLOSE)
h5_int64_t
h5_close (
	const h5_int64_t *f
	) {
	h5_file_t* fh = h5_filehandlefor2c(f);
	H5_API_ENTER (h5_int64_t, "f=%p", (h5_file_p)fh);
	H5_API_RETURN (h5_close_file (fh));
}

#define h5_finalize F77_NAME(                      \
                h5_finalize,                       \
                h5_finalize_,                      \
                H5_FINALIZE)
h5_int64_t
h5_finalize (
	void
	) {
	H5_API_ENTER (h5_int64_t, "%s", "");
	H5_API_RETURN (h5_close_hdf5());
}

#define h5_check F77_NAME(                      \
                h5_check,                       \
                h5_check_,                      \
                H5_CHECK)
h5_int64_t
h5_check (
	const h5_int64_t *f
	) {

	h5_file_t* fh = h5_filehandlefor2c(f);
	H5_API_ENTER (h5_int64_t, "f=%p", (h5_file_p)fh);
	H5_API_RETURN (h5_check_filehandle (fh));
}

/* H5hut data model */
#define h5_setstep F77_NAME(                            \
                            h5_setstep,                 \
                            h5_setstep_,                \
                            H5_SETSTEP)
h5_int64_t
h5_setstep (
	const h5_int64_t *f,
	h5_int64_t *step ) {

	h5_file_t* fh = h5_filehandlefor2c(f);
	H5_API_ENTER (h5_int64_t, "f=%p, step=%lld", (h5_file_p)fh, (long long)*step);
	H5_API_RETURN (h5_set_step (fh, (*step)-1));
}

#define h5_getstep F77_NAME(                    \
                h5_getstep,                     \
                h5_getstep_,                    \
                H5_GETSTEP)
h5_int64_t
h5_getstep (
	const h5_int64_t *f
	) {

	h5_file_t* fh = h5_filehandlefor2c(f);
	H5_API_ENTER (h5_int64_t, "f=%p", (h5_file_p)fh);
	H5_API_RETURN (h5_get_step (fh) + 1);
}

#define h5_getnsteps F77_NAME(                  \
                h5_getnsteps,                   \
                h5_getnsteps_,                  \
                H5_GETNSTEPS)
h5_int64_t
h5_getnsteps (
	const h5_int64_t *f
	) {

	h5_file_t* fh = h5_filehandlefor2c(f);
	H5_API_ENTER (h5_int64_t, "f=%p", (h5_file_p)fh);
	H5_API_RETURN (h5_get_num_steps (fh));
}


/* debug output */
#define h5_set_verbosity_level F77_NAME(        \
                h5_set_verbosity_level,         \
                h5_set_verbosity_level_,        \
                H5_SET_VERBOSITY_LEVEL)
h5_int64_t
h5_set_verbosity_level (
	const h5_int64_t *level
	) {

	H5_API_ENTER (h5_int64_t, "level=%lld", (long long)*level);
	H5_API_RETURN(h5_set_debuglevel (*level));
}

