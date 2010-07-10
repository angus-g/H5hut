!!!!!!!! Reading and Writing Attributes !!!!!!!!

!> \ingroup h5_attrib_f
!! See \ref H5WriteFileAttribString
!! \return 0 on success or error code
!<
INTEGER*8 FUNCTION h5_writefileattrib_string (filehandle,attrib_name,attrib_value)
    INTEGER*8, INTENT(IN) :: filehandle         !< the handle returned during file open
    CHARACTER(LEN=*), INTENT(IN) :: name        !< the name of the attribute
    CHARACTER(LEN=*), INTENT(IN) :: value       !< the string value to store
END FUNCTION

!> \ingroup h5_attrib_f
!! See \ref H5WriteStepAttribString
!! \return 0 on success or error code
!<
INTEGER*8 FUNCTION h5_writestepattrib_string (filehandle,attrib_name,attrib_value)
    INTEGER*8, INTENT(IN) :: filehandle         !< the handle returned during file open
    CHARACTER(LEN=*), INTENT(IN) :: name        !< the name of the attribute
    CHARACTER(LEN=*), INTENT(IN) :: value       !< the string value to store
END FUNCTION

!> \ingroup h5_attrib_f
!! See \ref H5ReadFileAttribString
!! \return 0 on success or error code
!<
INTEGER*8 FUNCTION h5_readfileattrib_string (filehandle,attrib_name,attrib_value)
    INTEGER*8, INTENT(IN) :: filehandle         !< the handle returned during file open
    CHARACTER(LEN=*), INTENT(IN) :: name        !< the name of the attribute
    CHARACTER(LEN=*), INTENT(OUT) :: value      !< buffer to read the string value into
END FUNCTION

!> \ingroup h5_attrib_f
!! See \ref H5ReadStepAttribString
!! \return 0 on success or error code
!<
INTEGER*8 FUNCTION h5_readstepattrib_string (filehandle,attrib_name,attrib_value)
    INTEGER*8, INTENT(IN) :: filehandle         !< the handle returned during file open
    CHARACTER(LEN=*), INTENT(IN) :: name        !< the name of the attribute
    CHARACTER(LEN=*), INTENT(OUT) :: value      !< buffer to read the string value into
END FUNCTION

!< \ingroup h5_attrib_f
!! See \ref H5WriteFileAttribFloat64
!! \return 0 on success or error code
!>
INTEGER*8 FUNCTION h5_writefileattrib_r8 ( filehandle, field_name, attrib_name, attrib_value, attrib_nelem)
    INTEGER*8, INTENT(IN) :: filehandle !< the handle returned at file open
    CHARACTER(LEN=*), INTENT(IN) :: name !< the name of the attribute
    REAL*8, INTENT(IN) :: data(*) !< the array of data to write into the attribute
    INTEGER*8, INTENT(IN) :: nelem !< the number of elements in the array
END FUNCTION

!< \ingroup h5_attrib_f
!! See \ref H5ReadFileAttribFloat64
!! \return 0 on success or error code
!>
INTEGER*8 FUNCTION h5_readfileattrib_r8 ( filehandle, name, data )
    INTEGER*8, INTENT(IN) :: filehandle !< the handle returned at file open
    CHARACTER(LEN=*), INTENT(IN) :: name   !< the name of the attribute
    REAL*8, INTENT(OUT) :: data(*) !< buffer to read value into
END FUNCTION

!< \ingroup h5_attrib_f
!! See \ref H5WriteFileAttribFloat32
!! \return 0 on success or error code
!>
INTEGER*8 FUNCTION h5_writefileattrib_r4 ( filehandle, field_name, attrib_name, attrib_value, attrib_nelem)
    INTEGER*8, INTENT(IN) :: filehandle !< the handle returned at file open
    CHARACTER(LEN=*), INTENT(IN) :: name !< the name of the attribute
    REAL*4, INTENT(IN) :: data(*) !< the array of data to write into the attribute
    INTEGER*8, INTENT(IN) :: nelem !< the number of elements in the array
END FUNCTION

!< \ingroup h5_attrib_f
!! See \ref H5ReadFileAttribFloat32
!! \return 0 on success or error code
!>
INTEGER*8 FUNCTION h5_readfileattrib_r4 ( filehandle, name, data )
    INTEGER*8, INTENT(IN) :: filehandle !< the handle returned at file open
    CHARACTER(LEN=*), INTENT(IN) :: name   !< the name of the attribute
    REAL*4, INTENT(OUT) :: data(*) !< buffer to read value into
END FUNCTION

!< \ingroup h5_attrib_f
!! See \ref H5WriteFileAttribInt64
!! \return 0 on success or error code
!>
INTEGER*8 FUNCTION h5_writefileattrib_i8 ( filehandle, field_name, attrib_name, attrib_value, attrib_nelem)
    INTEGER*8, INTENT(IN) :: filehandle !< the handle returned at file open
    CHARACTER(LEN=*), INTENT(IN) :: name !< the name of the attribute
    INTEGER*8, INTENT(IN) :: data(*) !< the array of data to write into the attribute
    INTEGER*8, INTENT(IN) :: nelem !< the number of elements in the array
END FUNCTION

!< \ingroup h5_attrib_f
!! See \ref H5ReadFileAttribInt64
!! \return 0 on success or error code
!>
INTEGER*8 FUNCTION h5_readfileattrib_i8 ( filehandle, name, data )
    INTEGER*8, INTENT(IN) :: filehandle !< the handle returned at file open
    CHARACTER(LEN=*), INTENT(IN) :: name   !< the name of the attribute
    INTEGER*8, INTENT(OUT) :: data(*) !< buffer to read value into
END FUNCTION

!< \ingroup h5_attrib_f
!! See \ref H5WriteFileAttribInt32
!! \return 0 on success or error code
!>
INTEGER*8 FUNCTION h5_writefileattrib_i4 ( filehandle, field_name, attrib_name, attrib_value, attrib_nelem)
    INTEGER*8, INTENT(IN) :: filehandle !< the handle returned at file open
    CHARACTER(LEN=*), INTENT(IN) :: name !< the name of the attribute
    INTEGER*4, INTENT(IN) :: data(*) !< the array of data to write into the attribute
    INTEGER*8, INTENT(IN) :: nelem !< the number of elements in the array
END FUNCTION

!< \ingroup h5_attrib_f
!! See \ref H5ReadFileAttribInt32
!! \return 0 on success or error code
!>
INTEGER*8 FUNCTION h5_readfileattrib_i4 ( filehandle, name, data )
    INTEGER*8, INTENT(IN) :: filehandle !< the handle returned at file open
    CHARACTER(LEN=*), INTENT(IN) :: name   !< the name of the attribute
    INTEGER*4, INTENT(OUT) :: data(*) !< buffer to read value into
END FUNCTION

!< \ingroup h5_attrib_f
!! See \ref H5WriteStepAttribFloat64
!! \return 0 on success or error code
!>
INTEGER*8 FUNCTION h5_writestepattrib_r8 ( filehandle, field_name, attrib_name, attrib_value, attrib_nelem)
    INTEGER*8, INTENT(IN) :: filehandle !< the handle returned at file open
    CHARACTER(LEN=*), INTENT(IN) :: name !< the name of the attribute
    REAL*8, INTENT(IN) :: data(*) !< the array of data to write into the attribute
    INTEGER*8, INTENT(IN) :: nelem !< the number of elements in the array
END FUNCTION

!< \ingroup h5_attrib_f
!! See \ref H5ReadStepAttribFloat64
!! \return 0 on success or error code
!>
INTEGER*8 FUNCTION h5_readstepattrib_r8 ( filehandle, name, data )
    INTEGER*8, INTENT(IN) :: filehandle !< the handle returned at file open
    CHARACTER(LEN=*), INTENT(IN) :: name   !< the name of the attribute
    REAL*8, INTENT(OUT) :: data(*) !< buffer to read value into
END FUNCTION

!< \ingroup h5_attrib_f
!! See \ref H5WriteStepAttribFloat32
!! \return 0 on success or error code
!>
INTEGER*8 FUNCTION h5_writestepattrib_r4 ( filehandle, field_name, attrib_name, attrib_value, attrib_nelem)
    INTEGER*8, INTENT(IN) :: filehandle !< the handle returned at file open
    CHARACTER(LEN=*), INTENT(IN) :: name !< the name of the attribute
    REAL*4, INTENT(IN) :: data(*) !< the array of data to write into the attribute
    INTEGER*8, INTENT(IN) :: nelem !< the number of elements in the array
END FUNCTION

!< \ingroup h5_attrib_f
!! See \ref H5ReadStepAttribFloat32
!! \return 0 on success or error code
!>
INTEGER*8 FUNCTION h5_readstepattrib_r4 ( filehandle, name, data )
    INTEGER*8, INTENT(IN) :: filehandle !< the handle returned at file open
    CHARACTER(LEN=*), INTENT(IN) :: name   !< the name of the attribute
    REAL*4, INTENT(OUT) :: data(*) !< buffer to read value into
END FUNCTION

!< \ingroup h5_attrib_f
!! See \ref H5WriteStepAttribInt64
!! \return 0 on success or error code
!>
INTEGER*8 FUNCTION h5_writestepattrib_i8 ( filehandle, field_name, attrib_name, attrib_value, attrib_nelem)
    INTEGER*8, INTENT(IN) :: filehandle !< the handle returned at file open
    CHARACTER(LEN=*), INTENT(IN) :: name !< the name of the attribute
    INTEGER*8, INTENT(IN) :: data(*) !< the array of data to write into the attribute
    INTEGER*8, INTENT(IN) :: nelem !< the number of elements in the array
END FUNCTION

!< \ingroup h5_attrib_f
!! See \ref H5ReadStepAttribInt64
!! \return 0 on success or error code
!>
INTEGER*8 FUNCTION h5_readstepattrib_i8 ( filehandle, name, data )
    INTEGER*8, INTENT(IN) :: filehandle !< the handle returned at file open
    CHARACTER(LEN=*), INTENT(IN) :: name   !< the name of the attribute
    INTEGER*8, INTENT(OUT) :: data(*) !< buffer to read value into
END FUNCTION

!< \ingroup h5_attrib_f
!! See \ref H5WriteStepAttribInt32
!! \return 0 on success or error code
!>
INTEGER*8 FUNCTION h5_writestepattrib_i4 ( filehandle, field_name, attrib_name, attrib_value, attrib_nelem)
    INTEGER*8, INTENT(IN) :: filehandle !< the handle returned at file open
    CHARACTER(LEN=*), INTENT(IN) :: name !< the name of the attribute
    INTEGER*4, INTENT(IN) :: data(*) !< the array of data to write into the attribute
    INTEGER*8, INTENT(IN) :: nelem !< the number of elements in the array
END FUNCTION

!< \ingroup h5_attrib_f
!! See \ref H5ReadStepAttribInt32
!! \return 0 on success or error code
!>
INTEGER*8 FUNCTION h5_readstepattrib_i4 ( filehandle, name, data )
    INTEGER*8, INTENT(IN) :: filehandle !< the handle returned at file open
    CHARACTER(LEN=*), INTENT(IN) :: name   !< the name of the attribute
    INTEGER*4, INTENT(OUT) :: data(*) !< buffer to read value into
END FUNCTION

!> \ingroup h5_attrib_f
!! See \ref H5GetNumStepAttribs
!! \return number of attributes or error code
!<
INTEGER*8 FUNCTION h5_getnstepattribs (filehandle)
    INTEGER*8, INTENT(IN) :: filehandle         !< the handle returned during file open
END FUNCTION

!> \ingroup h5_attrib_f
!! See \ref H5GetNumFileAttribs
!! \return number of attributes or error code
!<
INTEGER*8 FUNCTION h5_getnfileattribs (filehandle)
    INTEGER*8, INTENT(IN) :: filehandle         !< the handle returned during file open
END FUNCTION

!> \ingroup h5_attrib_f
!! See \ref H5GetStepAttribInfo
!! \return 0 on success or error code
!<
INTEGER*8 FUNCTION h5_getstepattribinfo (filehandle,idx,attrib_name,attrib_nelem)
    INTEGER*8, INTENT(IN) :: filehandle         !< the handle returned during file open
    INTEGER*8, INTENT(IN) :: index              !< index of the attribute to query (starting from 0)
    CHARACTER(LEN=*), INTENT(OUT) :: name       !< buffer to read the attribute name into
    INTEGER*8, INTENT(OUT) :: nelem             !< number of elements in the attribute's array
END FUNCTION

!> \ingroup h5_attrib_f
!! See \ref H5GetFileAttribInfo
!! \return 0 on success or error code
!<
INTEGER*8 FUNCTION h5_getfileattribinfo (filehandle,idx,attrib_name,attrib_nelem)
    INTEGER*8, INTENT(IN) :: filehandle         !< the handle returned during file open
    INTEGER*8, INTENT(IN) :: index              !< index of the attribute to query (starting from 0)
    CHARACTER(LEN=*), INTENT(OUT) :: name       !< buffer to read the attribute name into
    INTEGER*8, INTENT(OUT) :: nelem             !< number of elements in the attribute's array
END FUNCTION


