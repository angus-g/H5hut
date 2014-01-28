/*
  Copyright (c) 2006-2013, The Regents of the University of California,
  through Lawrence Berkeley National Laboratory (subject to receipt of any
  required approvals from the U.S. Dept. of Energy) and the Paul Scherrer
  Institut (Switzerland).  All rights reserved.

  License: see file COPYING in top level of source distribution.
*/

#include "h5_private.h"

#include "h5core/h5.h"
#include "h5core/h5_debug.h"
#include "h5core/h5_model.h"

#define h5_createprop_file F77_NAME(    \
                h5_createprop_file,     \
                h5_createprop_file_,    \
                H5_CREATEPROP_FILE)
h5_int64_t
h5_createprop_file (
        void
        ) {
        H5_API_ENTER (h5_int64_t, "%s", "");
        H5_API_RETURN ((h5_int64_t)h5_create_prop (H5_PROP_FILE));
}

#define h5_setprop_file_mpio F77_NAME( \
                h5_setprop_file_mpio,  \
                h5_setprop_file_mpio_, \
                H5_SETPROP_FILE_MPIO)
h5_int64_t
h5_setprop_file_mpio (
        h5_int64_t* _prop,
	MPI_Fint* _comm
        ) {
        H5_API_ENTER (h5_int64_t,
                      "prop=%lld, comm=%lld",
                      (long long int)*_prop, (long long int)*_comm);
        h5_prop_t prop = (h5_prop_t)*_prop;
        MPI_Comm comm = MPI_Comm_f2c (*_comm);
        H5_API_RETURN ((h5_int64_t)h5_set_prop_file_mpio_collective (prop, &comm));
}

#define h5_setprop_file_mpio_collective F77_NAME( \
                h5_setprop_file_mpio_collective,  \
                h5_setprop_file_mpio_collective_, \
                H5_SETPROP_FILE_MPIO_COLLECTIVE)
h5_int64_t
h5_setprop_file_mpio_collective (
        h5_int64_t* _prop,
	MPI_Fint* _comm
        ) {
        H5_API_ENTER (h5_int64_t,
                      "prop=%lld, comm=%lld",
                      (long long int)*_prop, (long long int)*_comm);
        h5_prop_t prop = (h5_prop_t)*_prop;
        MPI_Comm comm = MPI_Comm_f2c (*_comm);
        H5_API_RETURN ((h5_int64_t)h5_set_prop_file_mpio_collective (prop, &comm));
}

#define h5_setprop_file_mpio_independent F77_NAME( \
                h5_setprop_file_mpio_independent,  \
                h5_setprop_file_mpio_independent_, \
                H5_SETPROP_FILE_MPIO_INDEPENDENT)
h5_int64_t
h5_setprop_file_mpio_independent (
        h5_int64_t* _prop,
	MPI_Fint* _comm
        ) {
        H5_API_ENTER (h5_int64_t,
                      "prop=%lld, comm=%lld",
                      (long long int)*_prop, (long long int)*_comm);
        h5_prop_t prop = (h5_prop_t)*_prop;
        MPI_Comm comm = MPI_Comm_f2c (*_comm);
        H5_API_RETURN ((h5_int64_t)h5_set_prop_file_mpio_independent (prop, &comm));
}

#define h5_setprop_file_mpio_posix F77_NAME( \
                h5_setprop_file_mpio_posix,  \
                h5_setprop_file_mpio_posix_, \
                H5_SETPROP_FILE_MPIO_POSIX)
h5_int64_t
h5_setprop_file_mpio_posix (
        h5_int64_t* _prop,
	MPI_Fint* _comm
        ) {
        H5_API_ENTER (h5_int64_t,
                      "prop=%lld, comm=%lld",
                      (long long int)*_prop, (long long int)*_comm);
        h5_prop_t prop = (h5_prop_t)*_prop;
        MPI_Comm comm = MPI_Comm_f2c (*_comm);
        H5_API_RETURN ((h5_int64_t)h5_set_prop_file_mpio_posix (prop, &comm));
}

#define h5_setprop_file_corevfd F77_NAME( \
                h5_setprop_file_corevfd,  \
                h5_setprop_file_corevfd,  \
                H5_SETPROP_FILE_COREVFD)
h5_int64_t
h5_setprop_file_corevfd (
        h5_int64_t* _prop
        ) {
        H5_API_ENTER (h5_int64_t,
                      "prop=%lld",
                      (long long int)*_prop);
        h5_prop_t prop = (h5_prop_t)*_prop;
        H5_API_RETURN ((h5_int64_t)h5_set_prop_file_core_vfd (prop));
}

#define h5_setprop_file_align F77_NAME (  \
                h5_setprop_file_align,    \
                h5_setprop_file_align_,   \
                H5_SETPROP_FILE_ALIGN)
h5_int64_t
h5_setprop_file_align (
        h5_int64_t* _prop,
        h5_int64_t* align
        ) {
        H5_API_ENTER (h5_err_t,
                      "prop=%lld, align=%lld",
                      (long long int)*_prop, (long long int)*align);
        h5_prop_t prop = (h5_prop_t)*_prop;
        H5_API_RETURN (h5_set_prop_file_align (prop, *align));
}

#define h5_setprop_file_throttle F77_NAME (               \
                h5_setprop_file_throttle,                 \
                h5_setprop_file_throttle_,                \
                H5_SETPROP_FILE_THROTTLE)

h5_int64_t
h5_setprop_file_throttle (
        h5_int64_t* _prop,
        h5_int64_t* throttle
        ) {
        H5_API_ENTER (
                h5_err_t,
                "prop=%lld, throttle=%lld",
                (long long int)*_prop, (long long int)*throttle);
        h5_prop_t prop = (h5_prop_t)*_prop;
        H5_API_RETURN (h5_set_prop_file_throttle (prop, *throttle));
}

#define h5_closeprop F77_NAME (                 \
                h5_closeprop,                   \
                h5_closeprop_,                  \
                H5_CLOSEPROP)
h5_int64_t
h5_closeprop (
        h5_int64_t* _prop
        ) {
        H5_API_ENTER (h5_err_t,
                      "prop=%lld",
                      (long long int)*_prop);
        h5_prop_t prop = (h5_prop_t)*_prop;
        H5_API_RETURN (h5_close_prop (prop));
}

#define h5_openfile F77_NAME( \
                h5_openfile,  \
                h5_openfile_, \
                H5_OPENFILE)
h5_int64_t
h5_openfile (
	const char* _fname,
        h5_int64_t* _mode,
        h5_int64_t* _props,
	const int _len_fname
        ) {
        int len_fname = strlenf (_fname, _len_fname);
        H5_API_ENTER (h5_int64_t,
                      "fname = %*s, mode=%lld, props=%lld",
                      len_fname, _fname, (long long int)*_mode, (long long int)*_props);
        char* fname = h5_strdupfor2c (_fname, _len_fname);
        h5_int64_t mode = *_mode;
        h5_prop_t props = (h5_prop_t)*_props;
        h5_file_t f = h5_open_file2 (fname, mode, props);
        free (fname);
        H5_API_RETURN ((h5_int64_t)f);
}

#define h5_closefile F77_NAME(                  \
                h5_closefile,                   \
                h5_closefile_,                  \
                H5_CLOSEFILE)
h5_int64_t
h5_closefile (
	const h5_int64_t *f
	) {
	h5_file_t fh = h5_filehandlefor2c(f);
	H5_API_ENTER (h5_int64_t, "f=%p", (h5_file_p)fh);
	H5_API_RETURN (h5_close_file (fh));
}

#define h5_checkfile F77_NAME(                  \
                h5_checkfile,                       \
                h5_checkfile_,                      \
                H5_CHECKFILE)
h5_int64_t
h5_checkfile (
	const h5_int64_t *f
	) {
	h5_file_t fh = h5_filehandlefor2c(f);
	H5_API_ENTER (h5_int64_t, "f=%p", (h5_file_p)fh);
	H5_API_RETURN (h5_check_filehandle (fh));
}

#define h5_flushfile F77_NAME(                      \
                h5_flushfile,                       \
                h5_flushfile_,                      \
                H5_FLUSHFILE)
h5_int64_t
h5_flushfile (
	const h5_int64_t* f
	) {
	h5_file_t fh = h5_filehandlefor2c(f);
	H5_API_ENTER (h5_int64_t, "f=%p", (h5_file_p)fh);
	H5_API_RETURN (h5_flush_file (fh));
}

#define h5_flushstep F77_NAME(                      \
                h5_flushstep,                       \
                h5_flushstep_,                      \
                H5_FLUSHSTEP)
h5_int64_t
h5_flushstep (
	const h5_int64_t* f
	) {
	h5_file_t fh = h5_filehandlefor2c(f);
	H5_API_ENTER (h5_int64_t, "f=%p", (h5_file_p)fh);
	H5_API_RETURN (h5_flush_step (fh));
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

#define h5_abort_on_error F77_NAME( \
                h5_abort_on_error,  \
                h5_abort_on_error_, \
                H5_ABORT_ON_ERROR)
h5_int64_t
h5_abort_on_error (
        void
        ) {
	H5_API_ENTER (h5_int64_t, "%s", "");
        h5_set_debuglevel (1);
        H5_API_RETURN (h5_set_errorhandler (h5_abort_errorhandler));
}

#define h5_get_error_number F77_NAME( \
                h5_get_error_number,  \
                h5_get_error_number_, \
                H5_GET_ERROR_NUMBER)
h5_int64_t
h5_get_error_number (
        void
        ) {
	H5_API_ENTER (h5_int64_t, "%s", "");
        H5_API_RETURN (h5_get_errno ());
}
