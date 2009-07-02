
INTERFACE

INTEGER*8 FUNCTION h5bl_3d_write_scalar_field_r8 ( filehandle, name, data )
  INTEGER*8, INTENT(IN) :: filehandle
  CHARACTER(LEN=*), INTENT(IN) :: name
  REAL*8, INTENT(IN) :: data(*)
END FUNCTION

INTEGER*8 FUNCTION h5bl_3d_read_scalar_field_r8 ( filehandle, name, data )
  INTEGER*8, INTENT(IN) :: filehandle
  CHARACTER(LEN=*), INTENT(IN) :: name
  REAL*8, INTENT(OUT) :: data(*)
END FUNCTION

INTEGER*8 FUNCTION h5bl_3d_write_3dvector_field_r8 ( filehandle, name, x, y, z )
  INTEGER*8, INTENT(IN) :: filehandle
  CHARACTER(LEN=*), INTENT(IN) :: name
  REAL*8, INTENT(IN) :: x(*)
  REAL*8, INTENT(IN) :: y(*)
  REAL*8, INTENT(IN) :: z(*)
END FUNCTION

INTEGER*8 FUNCTION h5bl_3d_read_3dvector_field_r8 ( filehandle, name, x, y, z )
  INTEGER*8, INTENT(IN) :: filehandle
  CHARACTER(LEN=*), INTENT(IN) :: name
  REAL*8, INTENT(OUT) :: x(*)
  REAL*8, INTENT(OUT) :: y(*)
  REAL*8, INTENT(OUT) :: z(*)
END FUNCTION

INTEGER*8 FUNCTION h5bl_3d_write_scalar_field_r4 ( filehandle, name, data )
  INTEGER*8, INTENT(IN) :: filehandle
  CHARACTER(LEN=*), INTENT(IN) :: name
  REAL*4, INTENT(IN) :: data(*)
END FUNCTION

INTEGER*8 FUNCTION h5bl_3d_read_scalar_field_r4 ( filehandle, name, data )
  INTEGER*8, INTENT(IN) :: filehandle
  CHARACTER(LEN=*), INTENT(IN) :: name
  REAL*4, INTENT(OUT) :: data(*)
END FUNCTION

INTEGER*8 FUNCTION h5bl_3d_write_3dvector_field_r4 ( filehandle, name, x, y, z )
  INTEGER*8, INTENT(IN) :: filehandle
  CHARACTER(LEN=*), INTENT(IN) :: name
  REAL*4, INTENT(IN) :: x(*)
  REAL*4, INTENT(IN) :: y(*)
  REAL*4, INTENT(IN) :: z(*)
END FUNCTION

INTEGER*8 FUNCTION h5bl_3d_read_3dvector_field_r4 ( filehandle, name, x, y, z )
  INTEGER*8, INTENT(IN) :: filehandle
  CHARACTER(LEN=*), INTENT(IN) :: name
  REAL*4, INTENT(OUT) :: x(*)
  REAL*4, INTENT(OUT) :: y(*)
  REAL*4, INTENT(OUT) :: z(*)
END FUNCTION

INTEGER*8 FUNCTION h5bl_3d_write_scalar_field_i8 ( filehandle, name, data )
  INTEGER*8, INTENT(IN) :: filehandle
  CHARACTER(LEN=*), INTENT(IN) :: name
  INTEGER*8, INTENT(IN) :: data(*)
END FUNCTION

INTEGER*8 FUNCTION h5bl_3d_read_scalar_field_i8 ( filehandle, name, data )
  INTEGER*8, INTENT(IN) :: filehandle
  CHARACTER(LEN=*), INTENT(IN) :: name
  INTEGER*8, INTENT(OUT) :: data(*)
END FUNCTION

INTEGER*8 FUNCTION h5bl_3d_write_3dvector_field_i8 ( filehandle, name, x, y, z )
  INTEGER*8, INTENT(IN) :: filehandle
  CHARACTER(LEN=*), INTENT(IN) :: name
  INTEGER*8, INTENT(IN) :: x(*)
  INTEGER*8, INTENT(IN) :: y(*)
  INTEGER*8, INTENT(IN) :: z(*)
END FUNCTION

INTEGER*8 FUNCTION h5bl_3d_read_3dvector_field_i8 ( filehandle, name, x, y, z )
  INTEGER*8, INTENT(IN) :: filehandle
  CHARACTER(LEN=*), INTENT(IN) :: name
  INTEGER*8, INTENT(OUT) :: x(*)
  INTEGER*8, INTENT(OUT) :: y(*)
  INTEGER*8, INTENT(OUT) :: z(*)
END FUNCTION

INTEGER*8 FUNCTION h5bl_3d_write_scalar_field_i4 ( filehandle, name, data )
  INTEGER*8, INTENT(IN) :: filehandle
  CHARACTER(LEN=*), INTENT(IN) :: name
  INTEGER*4, INTENT(IN) :: data(*)
END FUNCTION

INTEGER*8 FUNCTION h5bl_3d_read_scalar_field_i4 ( filehandle, name, data )
  INTEGER*8, INTENT(IN) :: filehandle
  CHARACTER(LEN=*), INTENT(IN) :: name
  INTEGER*4, INTENT(OUT) :: data(*)
END FUNCTION

INTEGER*8 FUNCTION h5bl_3d_write_3dvector_field_i4 ( filehandle, name, x, y, z )
  INTEGER*8, INTENT(IN) :: filehandle
  CHARACTER(LEN=*), INTENT(IN) :: name
  INTEGER*4, INTENT(IN) :: x(*)
  INTEGER*4, INTENT(IN) :: y(*)
  INTEGER*4, INTENT(IN) :: z(*)
END FUNCTION

INTEGER*8 FUNCTION h5bl_3d_read_3dvector_field_i4 ( filehandle, name, x, y, z )
  INTEGER*8, INTENT(IN) :: filehandle
  CHARACTER(LEN=*), INTENT(IN) :: name
  INTEGER*4, INTENT(OUT) :: x(*)
  INTEGER*4, INTENT(OUT) :: y(*)
  INTEGER*4, INTENT(OUT) :: z(*)
END FUNCTION

INTEGER*8 FUNCTION h5bl_writefieldattrib_r8 ( filehandle, field_name, attrib_name, attrib_value, attrib_nelem)
  INTEGER*8, INTENT(IN) :: filehandle
  CHARACTER(LEN=*), INTENT(IN) :: field_name    ! The name of the field
  CHARACTER(LEN=*), INTENT(IN) :: attrib_name   ! The name of the attribute
  REAL*8, INTENT(IN) :: attrib_value(*) ! The array of data to write into the attribute
  INTEGER*8, INTENT(IN) :: attrib_nelem ! Number of elements in the attrib array
END FUNCTION

INTEGER*8 FUNCTION h5bl_writefieldattrib_r4 ( filehandle, field_name, attrib_name, attrib_value, attrib_nelem)
  INTEGER*8, INTENT(IN) :: filehandle
  CHARACTER(LEN=*), INTENT(IN) :: field_name    ! The name of the field
  CHARACTER(LEN=*), INTENT(IN) :: attrib_name   ! The name of the attribute
  REAL*4, INTENT(IN) :: attrib_value(*) ! The array of data to write into the attribute
  INTEGER*8, INTENT(IN) :: attrib_nelem ! Number of elements in the attrib array
END FUNCTION

INTEGER*8 FUNCTION h5bl_writefieldattrib_i8 ( filehandle, field_name, attrib_name, attrib_value, attrib_nelem)
  INTEGER*8, INTENT(IN) :: filehandle
  CHARACTER(LEN=*), INTENT(IN) :: field_name    ! The name of the field
  CHARACTER(LEN=*), INTENT(IN) :: attrib_name   ! The name of the attribute
  INTEGER*8, INTENT(IN) :: attrib_value(*) ! The array of data to write into the attribute
  INTEGER*8, INTENT(IN) :: attrib_nelem ! Number of elements in the attrib array
END FUNCTION

INTEGER*8 FUNCTION h5bl_writefieldattrib_i4 ( filehandle, field_name, attrib_name, attrib_value, attrib_nelem)
  INTEGER*8, INTENT(IN) :: filehandle
  CHARACTER(LEN=*), INTENT(IN) :: field_name    ! The name of the field
  CHARACTER(LEN=*), INTENT(IN) :: attrib_name   ! The name of the attribute
  INTEGER*4, INTENT(IN) :: attrib_value(*) ! The array of data to write into the attribute
  INTEGER*8, INTENT(IN) :: attrib_nelem ! Number of elements in the attrib array
END FUNCTION

END INTERFACE
