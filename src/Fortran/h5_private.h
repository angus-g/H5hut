/*
  Copyright (c) 2006-2013, The Regents of the University of California,
  through Lawrence Berkeley National Laboratory (subject to receipt of any
  required approvals from the U.S. Dept. of Energy) and the Paul Scherrer
  Institut (Switzerland).  All rights reserved.

  License: see file COPYING in top level of source distribution.
*/

#ifndef __FORTRAN_H5_PRIVATE_H
#define __FORTRAN_H5_PRIVATE_H

#include <stdlib.h>
#include <string.h>

#include "h5core/h5.h"

#include "Underscore.h"

#if defined(F77_NO_UNDERSCORE)
#define F77_NAME(a,b,c) a
#elif defined(F77_SINGLE_UNDERSCORE)
#define F77_NAME(a,b,c) b
#elif defined(F77_CRAY_UNDERSCORE)
#define F77_NAME(a,b,c) c
#else
#error Error, no way to determine how to construct fortran bindings
#endif

#define convert_type2for(type)                  \
        if (*type == H5_STRING_T) {             \
                *type = 1;                      \
        } else if (*type == H5_INT16_T) {       \
                *type = 2;                      \
        } else if (*type == H5_INT32_T) {       \
                *type = 3;                      \
        } else if (*type == H5_INT64_T) {       \
                *type = 4;                      \
        } else if (*type == H5_FLOAT32_T) {     \
                *type = 5;                      \
        } else if (*type == H5_FLOAT64_T) {     \
                *type = 6;                      \
        } else {                                \
                H5_API_LEAVE (                  \
                        h5_error (              \
                                H5_ERR_H5,      \
                                "Unknown attribute type")); \
        }

static inline char*
h5_strdupfor2c (
	const char* s,
	const ssize_t len
	) {
        // :FIXME: error handling
	char* dup = (char*)malloc (len + 1);
	strncpy (dup, s, len);
	dup[len] = '\0';
	for (int i = len-1; i >= 0; i--) {
		if (dup[i] == ' ') dup[i] = '\0';
		else break;
	}
	return dup;
}

static inline char*
h5_strc2for (
	char* const str,
	const ssize_t l_str
	) {
	size_t len = strlen (str);
	memset (str+len, ' ', l_str-len);

	return str;
}

static inline h5_file_t
h5_filehandlefor2c (
	const h5_int64_t* ptr
	) {
	return (h5_file_t)*ptr;
}

static inline
int strlenf (
        const char* s,
        int len
        ) {
        if (len == 0) return 0;
        while (s[--len] == ' ');
        return ++len;
}

#endif
