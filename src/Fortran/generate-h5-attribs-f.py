#!/usr/bin/python

fc_head = """
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
"""

string_fi = """
!> \ingroup h5hut_attrib_f
!! See \ref H5WriteFileAttribString
!! \return 0 on success or error code
!<
INTEGER*8 FUNCTION h5_writefileattrib_string ( filehandle, name, buffer )
    INTEGER*8, INTENT(IN) :: filehandle         !< the handle returned during file open
    CHARACTER(LEN=*), INTENT(IN) :: name        !< the name of the attribute
    CHARACTER(LEN=*), INTENT(IN) :: buffer      !< the string value to store
END FUNCTION

!> \ingroup h5hut_attrib_f
!! See \ref H5WriteStepAttribString
!! \return 0 on success or error code
!<
INTEGER*8 FUNCTION h5_writestepattrib_string ( filehandle, name, buffer )
    INTEGER*8, INTENT(IN) :: filehandle         !< the handle returned during file open
    CHARACTER(LEN=*), INTENT(IN) :: name        !< the name of the attribute
    CHARACTER(LEN=*), INTENT(IN) :: buffer      !< the string value to store
END FUNCTION

!> \ingroup h5hut_attrib_f
!! See \ref H5ReadFileAttribString
!! \return 0 on success or error code
!<
INTEGER*8 FUNCTION h5_readfileattrib_string ( filehandle, name, buffer )
    INTEGER*8, INTENT(IN) :: filehandle         !< the handle returned during file open
    CHARACTER(LEN=*), INTENT(IN) :: name        !< the name of the attribute
    CHARACTER(LEN=*), INTENT(OUT) :: buffer     !< buffer to read the string value into
END FUNCTION

!> \ingroup h5hut_attrib_f
!! See \ref H5ReadStepAttribString
!! \return 0 on success or error code
!<
INTEGER*8 FUNCTION h5_readstepattrib_string ( filehandle, name, buffer )
    INTEGER*8, INTENT(IN) :: filehandle         !< the handle returned during file open
    CHARACTER(LEN=*), INTENT(IN) :: name        !< the name of the attribute
    CHARACTER(LEN=*), INTENT(OUT) :: buffer     !< buffer to read the string value into
END FUNCTION
"""

string_fc = """
#if ! defined(F77_NO_UNDERSCORE)

#define h5_writefileattrib_string F77NAME (				\
					h5_writefileattrib_string_,	\
					H5_writefileattrib_string )
#define h5_writestepattrib_string F77NAME (				\
					h5_writestepattrib_string_,	\
					H5_WRITESTEPATTRIB_STRING )
#define h5_readstepattrib_string F77NAME (				\
					h5_readstepattrib_string_,	\
					h5_READSTEPATTRIB_STRING )
#define h5_readfileattrib_string F77NAME (				\
					h5_readfileattrib_string_,	\
					h5_READFILEATTRIB_STRING )

#endif

h5_err_t
h5_writefileattrib_string (
	h5_int64_t *const f,
	const char *name,
	const char *buffer,
	const int l_name,
	const int l_buffer
	) {

	h5_file_t *fh = h5_filehandlefor2c(f);
	char *name2 = h5_strdupfor2c ( name, l_name );
	char *buffer2 = h5_strdupfor2c ( buffer, l_buffer );
	H5_API_ENTER5 (h5_err_t,
		       "f=%p, name=\\"%s\\", buffer=\\"%s\\", l_name=%d, l_buffer=%d",
		       fh, name2, buffer2, l_name, l_buffer);

	h5_err_t herr = h5_write_attrib (
		fh, H5_ATTRIB_FILE, name2,
		H5_STRING_T, buffer2, strlen(buffer2)+1 );

	free ( name2 );
	free ( buffer2 );
	H5_API_RETURN(herr);
}

h5_err_t
h5_writestepattrib_string (
	h5_int64_t *const f,
	const char *name,
	const char *buffer,
	const int l_name,
	const int l_buffer
	) {

	h5_file_t *fh = h5_filehandlefor2c(f);
	char *name2 = h5_strdupfor2c ( name, l_name );
	char *buffer2 = h5_strdupfor2c ( buffer, l_buffer );
	H5_API_ENTER5 (h5_err_t,
		       "f=%p, name=\\"%s\\", buffer=\\"%s\\", l_name=%d, l_buffer=%d",
		       fh, name2, buffer2, l_name, l_buffer);

	h5_err_t herr = h5_write_attrib (
		fh, H5_ATTRIB_STEP, name2,
		H5_STRING_T, buffer2, strlen(buffer2)+1 );

	free ( name2 );
	free ( buffer2 );
	H5_API_RETURN(herr);
}

h5_err_t
h5_readfileattrib_string (
	h5_int64_t *const f,
	const char *name,
	char *buffer,
	const int l_name,
	const int l_buffer
	) {
		
	h5_file_t *fh = h5_filehandlefor2c(f);
	char * name2 = h5_strdupfor2c ( name, l_name );
	H5_API_ENTER5 (h5_err_t,
		       "f=%p, name=\\"%s\\", buffer=0x%p, l_name=%d, l_buffer=%d",
		       fh, name2, buffer, l_name, l_buffer);

	h5_err_t herr = h5_read_attrib (
		fh, H5_ATTRIB_FILE, name2, H5_STRING_T, buffer );

	h5_strc2for ( buffer, l_buffer );

	free ( name2 );
	H5_API_RETURN(herr);
}

h5_err_t
h5_readstepeattrib_string (
	h5_int64_t *const f,
	const char *name,
	char *buffer,
	const int l_name,
	const int l_buffer
	) {
		
	h5_file_t *fh = h5_filehandlefor2c(f);
	char * name2 = h5_strdupfor2c ( name, l_name );
	H5_API_ENTER5 (h5_err_t,
		       "f=%p, name=\\"%s\\", buffer=0x%p, l_name=%d, l_buffer=%d",
		       fh, name2, buffer, l_name, l_buffer);

	h5_err_t herr = h5_read_attrib (
		fh, H5_ATTRIB_STEP, name2, H5_STRING_T, buffer );

	h5_strc2for ( buffer, l_buffer );

	free ( name2 );
	H5_API_RETURN(herr);
}
"""

write_fi = """
!> \\ingroup h5hut_attrib_f
!! See \\ref H5Write#Group#Attrib#TYPE_ABV#
!! \\return 0 on success or error code
!<
INTEGER*8 FUNCTION h5bl_write#group#attrib_#TYPE_F90_ABV# ( filehandle, name, buffer, nelems )
    INTEGER*8, INTENT(IN) :: filehandle		!< the handle returned at file open
    CHARACTER(LEN=*), INTENT(IN) :: name	!< the name of the attribute
    #TYPE_F90#, INTENT(IN) :: buffer(*)		!< the buffer to write from
    INTEGER*8, INTENT(IN) :: nelems		!< the number of elements in the array
END FUNCTION
"""

write_fc = """
#if ! defined(F77_NO_UNDERSCORE)
#define h5_write#group#attrib_#TYPE_F90_ABV# F77NAME ( \\
	h5_write#group#attrib_#TYPE_F90_ABV#_, \\
	H5_WRITE#GROUP#ATTRIB_#TYPE_F90_ABVC# )
#endif

h5_err_t
h5_write#group#attrib_#TYPE_F90_ABV# (
	h5_int64_t *const f,
	const char *name,
	const h5_#TYPE_H5P#_t *buffer,
	const h5_int64_t *nelem,
	const int l_name
	) {

	h5_file_t *fh = h5_filehandlefor2c(f);
	char *name2 = h5_strdupfor2c ( name, l_name );
	H5_API_ENTER5 (h5_err_t,
		       "f=%p, name=\\"%s\\", buffer=0x%p, nelem=%lld, l_name=%d",
		       fh, name2, buffer, (long long)*nelem, l_name);

	h5_err_t herr = h5_write_attrib(
		fh, H5_ATTRIB_#GROUP#, name2,
		#TYPE_HDF5#, buffer, (hsize_t)*nelem);

	free ( name2 );
	H5_API_RETURN(herr);
}
"""

read_fi = """
!> \\ingroup h5hut_attrib_f
!! See \\ref H5Write#Group#Attrib#TYPE_ABV#
!! \\return 0 on success or error code
!<
INTEGER*8 FUNCTION h5_read#group#attrib_#TYPE_F90_ABV# ( filehandle, name, buffer )
    INTEGER*8, INTENT(IN) :: filehandle		!< the handle returned at file open
    CHARACTER(LEN=*), INTENT(IN) :: name	!< the name of the attribute
    #TYPE_F90#, INTENT(IN) :: buffer(*)		!< the buffer to read to
END FUNCTION
"""

read_fc = """
#if ! defined(F77_NO_UNDERSCORE)
#define h5_read#group#attrib_#TYPE_F90_ABV# F77NAME ( \\
	h5_read#group#attrib_#TYPE_F90_ABV#_, \\
	H5_READ#GROUP#ATTRIB_#TYPE_F90_ABVC# )
#endif

h5_err_t
h5bl_read#group#attrib_#TYPE_F90_ABV# (
	h5_int64_t *const f,
	const char *name,
	h5_float64_t *buffer,
	const int l_name
	) {

	h5_file_t *fh = h5_filehandlefor2c(f);
	char *name2 = h5_strdupfor2c ( name, l_name );
	H5_API_ENTER4 (h5_err_t,
		       "f=%p, name=\\"%s\\", buffer=0x%p, l_name=%d",
		       fh, name2, buffer, l_name);

	h5_err_t herr = h5_read_attrib(
		fh, H5_ATTRIB_#GROUP#, name2, #TYPE_HDF5#, buffer);

	free ( name2 );
	H5_API_RETURN(herr);
}
"""

groups = [ "file", "step" ]

types = [
  ["float64", "H5_FLOAT64_T", "REAL*8", "r8", "R8", "Float64"],
  ["float32", "H5_FLOAT32_T", "REAL*4", "r4", "R4", "Float32"],
  ["int64", "H5_INT64_T", "INTEGER*8", "i8", "I8", "Int64"],
  ["int32", "H5_INT32_T", "INTEGER*4", "i4", "I4", "Int32"]
]

def create_call(template, type, group):
  fcn = template
  fcn = fcn.replace('#group#',group)\
           .replace('#GROUP#',group.upper())\
           .replace('#TYPE_H5P#',type[0])\
           .replace('#TYPE_HDF5#',type[1])\
           .replace('#TYPE_F90#',type[2])\
           .replace('#TYPE_F90_ABV#',type[3])\
           .replace('#TYPE_F90_ABVC#',type[4])\
           .replace('#TYPE_ABV#',type[5]) 
  return fcn

def write_calls():
  fcfile = file('H5_attribs_F.c','w')
  fcfile.write(fc_head)
  fcfile.write(string_fc)
  fifile = file('H5_attribs.f90','w')
  fifile.write(string_fi)
  for group in groups:
    for type in types:
      fcfile.write(create_call(write_fc,type,group));
      fcfile.write(create_call(read_fc,type,group));
      fifile.write(create_call(write_fi,type,group));
      fifile.write(create_call(read_fi,type,group));
  fcfile.close()
  fifile.close()

write_calls()

