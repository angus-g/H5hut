
!> \ingroup h5hut_attrib_f
!! See ef H5WriteFileAttribString
!! eturn 0 on success or error code
!<
INTEGER*8 FUNCTION h5_writefileattrib_string ( filehandle, name, buffer )
    INTEGER*8, INTENT(IN) :: filehandle         !< the handle returned during file open
    CHARACTER(LEN=*), INTENT(IN) :: name        !< the name of the attribute
    CHARACTER(LEN=*), INTENT(IN) :: buffer      !< the string value to store
END FUNCTION

!> \ingroup h5hut_attrib_f
!! See ef H5WriteStepAttribString
!! eturn 0 on success or error code
!<
INTEGER*8 FUNCTION h5_writestepattrib_string ( filehandle, name, buffer )
    INTEGER*8, INTENT(IN) :: filehandle         !< the handle returned during file open
    CHARACTER(LEN=*), INTENT(IN) :: name        !< the name of the attribute
    CHARACTER(LEN=*), INTENT(IN) :: buffer      !< the string value to store
END FUNCTION

!> \ingroup h5hut_attrib_f
!! See ef H5ReadFileAttribString
!! eturn 0 on success or error code
!<
INTEGER*8 FUNCTION h5_readfileattrib_string ( filehandle, name, buffer )
    INTEGER*8, INTENT(IN) :: filehandle         !< the handle returned during file open
    CHARACTER(LEN=*), INTENT(IN) :: name        !< the name of the attribute
    CHARACTER(LEN=*), INTENT(OUT) :: buffer     !< buffer to read the string value into
END FUNCTION

!> \ingroup h5hut_attrib_f
!! See ef H5ReadStepAttribString
!! eturn 0 on success or error code
!<
INTEGER*8 FUNCTION h5_readstepattrib_string ( filehandle, name, buffer )
    INTEGER*8, INTENT(IN) :: filehandle         !< the handle returned during file open
    CHARACTER(LEN=*), INTENT(IN) :: name        !< the name of the attribute
    CHARACTER(LEN=*), INTENT(OUT) :: buffer     !< buffer to read the string value into
END FUNCTION

!> \ingroup h5hut_attrib_f
!! See \ref H5Write#Group#AttribFloat64
!! \return 0 on success or error code
!<
INTEGER*8 FUNCTION h5bl_writefileattrib_r8 ( filehandle, name, buffer, nelems )
    INTEGER*8, INTENT(IN) :: filehandle		!< the handle returned at file open
    CHARACTER(LEN=*), INTENT(IN) :: name	!< the name of the attribute
    REAL*8, INTENT(IN) :: buffer(*)		!< the buffer to write from
    INTEGER*8, INTENT(IN) :: nelems		!< the number of elements in the array
END FUNCTION

!> \ingroup h5hut_attrib_f
!! See \ref H5Write#Group#AttribFloat64
!! \return 0 on success or error code
!<
INTEGER*8 FUNCTION h5_readfileattrib_r8 ( filehandle, name, buffer )
    INTEGER*8, INTENT(IN) :: filehandle		!< the handle returned at file open
    CHARACTER(LEN=*), INTENT(IN) :: name	!< the name of the attribute
    REAL*8, INTENT(IN) :: buffer(*)		!< the buffer to read to
END FUNCTION

!> \ingroup h5hut_attrib_f
!! See \ref H5Write#Group#AttribFloat32
!! \return 0 on success or error code
!<
INTEGER*8 FUNCTION h5bl_writefileattrib_r4 ( filehandle, name, buffer, nelems )
    INTEGER*8, INTENT(IN) :: filehandle		!< the handle returned at file open
    CHARACTER(LEN=*), INTENT(IN) :: name	!< the name of the attribute
    REAL*4, INTENT(IN) :: buffer(*)		!< the buffer to write from
    INTEGER*8, INTENT(IN) :: nelems		!< the number of elements in the array
END FUNCTION

!> \ingroup h5hut_attrib_f
!! See \ref H5Write#Group#AttribFloat32
!! \return 0 on success or error code
!<
INTEGER*8 FUNCTION h5_readfileattrib_r4 ( filehandle, name, buffer )
    INTEGER*8, INTENT(IN) :: filehandle		!< the handle returned at file open
    CHARACTER(LEN=*), INTENT(IN) :: name	!< the name of the attribute
    REAL*4, INTENT(IN) :: buffer(*)		!< the buffer to read to
END FUNCTION

!> \ingroup h5hut_attrib_f
!! See \ref H5Write#Group#AttribInt64
!! \return 0 on success or error code
!<
INTEGER*8 FUNCTION h5bl_writefileattrib_i8 ( filehandle, name, buffer, nelems )
    INTEGER*8, INTENT(IN) :: filehandle		!< the handle returned at file open
    CHARACTER(LEN=*), INTENT(IN) :: name	!< the name of the attribute
    INTEGER*8, INTENT(IN) :: buffer(*)		!< the buffer to write from
    INTEGER*8, INTENT(IN) :: nelems		!< the number of elements in the array
END FUNCTION

!> \ingroup h5hut_attrib_f
!! See \ref H5Write#Group#AttribInt64
!! \return 0 on success or error code
!<
INTEGER*8 FUNCTION h5_readfileattrib_i8 ( filehandle, name, buffer )
    INTEGER*8, INTENT(IN) :: filehandle		!< the handle returned at file open
    CHARACTER(LEN=*), INTENT(IN) :: name	!< the name of the attribute
    INTEGER*8, INTENT(IN) :: buffer(*)		!< the buffer to read to
END FUNCTION

!> \ingroup h5hut_attrib_f
!! See \ref H5Write#Group#AttribInt32
!! \return 0 on success or error code
!<
INTEGER*8 FUNCTION h5bl_writefileattrib_i4 ( filehandle, name, buffer, nelems )
    INTEGER*8, INTENT(IN) :: filehandle		!< the handle returned at file open
    CHARACTER(LEN=*), INTENT(IN) :: name	!< the name of the attribute
    INTEGER*4, INTENT(IN) :: buffer(*)		!< the buffer to write from
    INTEGER*8, INTENT(IN) :: nelems		!< the number of elements in the array
END FUNCTION

!> \ingroup h5hut_attrib_f
!! See \ref H5Write#Group#AttribInt32
!! \return 0 on success or error code
!<
INTEGER*8 FUNCTION h5_readfileattrib_i4 ( filehandle, name, buffer )
    INTEGER*8, INTENT(IN) :: filehandle		!< the handle returned at file open
    CHARACTER(LEN=*), INTENT(IN) :: name	!< the name of the attribute
    INTEGER*4, INTENT(IN) :: buffer(*)		!< the buffer to read to
END FUNCTION

!> \ingroup h5hut_attrib_f
!! See \ref H5Write#Group#AttribFloat64
!! \return 0 on success or error code
!<
INTEGER*8 FUNCTION h5bl_writestepattrib_r8 ( filehandle, name, buffer, nelems )
    INTEGER*8, INTENT(IN) :: filehandle		!< the handle returned at file open
    CHARACTER(LEN=*), INTENT(IN) :: name	!< the name of the attribute
    REAL*8, INTENT(IN) :: buffer(*)		!< the buffer to write from
    INTEGER*8, INTENT(IN) :: nelems		!< the number of elements in the array
END FUNCTION

!> \ingroup h5hut_attrib_f
!! See \ref H5Write#Group#AttribFloat64
!! \return 0 on success or error code
!<
INTEGER*8 FUNCTION h5_readstepattrib_r8 ( filehandle, name, buffer )
    INTEGER*8, INTENT(IN) :: filehandle		!< the handle returned at file open
    CHARACTER(LEN=*), INTENT(IN) :: name	!< the name of the attribute
    REAL*8, INTENT(IN) :: buffer(*)		!< the buffer to read to
END FUNCTION

!> \ingroup h5hut_attrib_f
!! See \ref H5Write#Group#AttribFloat32
!! \return 0 on success or error code
!<
INTEGER*8 FUNCTION h5bl_writestepattrib_r4 ( filehandle, name, buffer, nelems )
    INTEGER*8, INTENT(IN) :: filehandle		!< the handle returned at file open
    CHARACTER(LEN=*), INTENT(IN) :: name	!< the name of the attribute
    REAL*4, INTENT(IN) :: buffer(*)		!< the buffer to write from
    INTEGER*8, INTENT(IN) :: nelems		!< the number of elements in the array
END FUNCTION

!> \ingroup h5hut_attrib_f
!! See \ref H5Write#Group#AttribFloat32
!! \return 0 on success or error code
!<
INTEGER*8 FUNCTION h5_readstepattrib_r4 ( filehandle, name, buffer )
    INTEGER*8, INTENT(IN) :: filehandle		!< the handle returned at file open
    CHARACTER(LEN=*), INTENT(IN) :: name	!< the name of the attribute
    REAL*4, INTENT(IN) :: buffer(*)		!< the buffer to read to
END FUNCTION

!> \ingroup h5hut_attrib_f
!! See \ref H5Write#Group#AttribInt64
!! \return 0 on success or error code
!<
INTEGER*8 FUNCTION h5bl_writestepattrib_i8 ( filehandle, name, buffer, nelems )
    INTEGER*8, INTENT(IN) :: filehandle		!< the handle returned at file open
    CHARACTER(LEN=*), INTENT(IN) :: name	!< the name of the attribute
    INTEGER*8, INTENT(IN) :: buffer(*)		!< the buffer to write from
    INTEGER*8, INTENT(IN) :: nelems		!< the number of elements in the array
END FUNCTION

!> \ingroup h5hut_attrib_f
!! See \ref H5Write#Group#AttribInt64
!! \return 0 on success or error code
!<
INTEGER*8 FUNCTION h5_readstepattrib_i8 ( filehandle, name, buffer )
    INTEGER*8, INTENT(IN) :: filehandle		!< the handle returned at file open
    CHARACTER(LEN=*), INTENT(IN) :: name	!< the name of the attribute
    INTEGER*8, INTENT(IN) :: buffer(*)		!< the buffer to read to
END FUNCTION

!> \ingroup h5hut_attrib_f
!! See \ref H5Write#Group#AttribInt32
!! \return 0 on success or error code
!<
INTEGER*8 FUNCTION h5bl_writestepattrib_i4 ( filehandle, name, buffer, nelems )
    INTEGER*8, INTENT(IN) :: filehandle		!< the handle returned at file open
    CHARACTER(LEN=*), INTENT(IN) :: name	!< the name of the attribute
    INTEGER*4, INTENT(IN) :: buffer(*)		!< the buffer to write from
    INTEGER*8, INTENT(IN) :: nelems		!< the number of elements in the array
END FUNCTION

!> \ingroup h5hut_attrib_f
!! See \ref H5Write#Group#AttribInt32
!! \return 0 on success or error code
!<
INTEGER*8 FUNCTION h5_readstepattrib_i4 ( filehandle, name, buffer )
    INTEGER*8, INTENT(IN) :: filehandle		!< the handle returned at file open
    CHARACTER(LEN=*), INTENT(IN) :: name	!< the name of the attribute
    INTEGER*4, INTENT(IN) :: buffer(*)		!< the buffer to read to
END FUNCTION
