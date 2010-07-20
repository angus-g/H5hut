!!!!!!!! Setting up the Data Model !!!!!!!!

!> \ingroup h5block_model_f
!! See \ref H5Block3dSetView
!! \return 0 on success or error code
!<
INTEGER*8 FUNCTION h5bl_3d_setview ( filehandle, i_start, i_end, j_start, j_end, k_start, k_end )
    INTEGER*8, INTENT(IN) :: filehandle
    INTEGER*8, INTENT(IN) :: i_start
    INTEGER*8, INTENT(IN) :: i_end
    INTEGER*8, INTENT(IN) :: j_start
    INTEGER*8, INTENT(IN) :: j_end
    INTEGER*8, INTENT(IN) :: k_start
    INTEGER*8, INTENT(IN) :: k_end
END FUNCTION

!> \ingroup h5block_model_f
!! See \ref H5Block3dGetView
!! \return 0 on success or error code
!<
INTEGER*8 FUNCTION h5bl_3d_getview ( filehandle, i_start, i_end, j_start, j_end, k_start, k_end )
    INTEGER*8, INTENT(IN) :: filehandle
    INTEGER*8, INTENT(OUT) :: i_start
    INTEGER*8, INTENT(OUT) :: i_end
    INTEGER*8, INTENT(OUT) :: j_start
    INTEGER*8, INTENT(OUT) :: j_end
    INTEGER*8, INTENT(OUT) :: k_start
    INTEGER*8, INTENT(OUT) :: k_end
END FUNCTION

  
!> \ingroup h5block_model_f
!! See \ref H5Block3dSetChunk
!! \return 0 on success or error code
!<
INTEGER*8 FUNCTION h5bl_3d_setchunk ( filehandle, i, j, k )
    INTEGER*8, INTENT(IN) :: filehandle
    INTEGER*8, INTENT(IN) :: i
    INTEGER*8, INTENT(IN) :: j
    INTEGER*8, INTENT(IN) :: k
END FUNCTION
 
!> \ingroup h5block_model_f
!! See \ref H5Block3dGetReducedView
!! \return 0 on success or error code
!<
INTEGER*8 FUNCTION h5bl_3d_getreducedview ( filehandle, i_start, i_end, j_start, j_end, k_start, k_end )
    INTEGER*8, INTENT(IN) :: filehandle
    INTEGER*8, INTENT(OUT) :: i_start
    INTEGER*8, INTENT(OUT) :: i_end
    INTEGER*8, INTENT(OUT) :: j_start
    INTEGER*8, INTENT(OUT) :: j_end
    INTEGER*8, INTENT(OUT) :: k_start
    INTEGER*8, INTENT(OUT) :: k_end
END FUNCTION
  
!> \ingroup h5block_model_f
!! See \ref H5Block3dHasView
!! \return rank of processor error code
!<
INTEGER*8 FUNCTION h5bl_3d_hasview ( filehandle )
    INTEGER*8, INTENT(IN) :: filehandle
END FUNCTION
  
!> \ingroup h5block_model_f
!! See \ref H5BlockGetNumFields
!! \return number of fields or error code
!<
INTEGER*8 FUNCTION h5bl_getnumfields ( filehandle )
    INTEGER*8, INTENT(IN) :: filehandle
END FUNCTION
  
!> \ingroup h5block_model_f
!! See \ref H5BlockGetFieldInfo
!! \return 0 on success or error code
!<
INTEGER*8 FUNCTION h5bl_getfieldinfo ( filehandle, idx, field_name, grid_rank, grid_dims, field_dims )
    INTEGER*8, INTENT(IN) :: filehandle
    INTEGER*8, INTENT(IN) :: idx
    CHARACTER(LEN=*), INTENT(OUT) :: field_name
    INTEGER*8, INTENT(OUT) :: grid_rank
    INTEGER*8, INTENT(OUT) :: grid_dims(*)
    INTEGER*8, INTENT(OUT) :: field_dims
    INTEGER*8, INTENT(OUT) :: type
END FUNCTION

!!!!!!!! Reading and Writing Attributes !!!!!!!!
 
!> \ingroup h5block_attrib_f
!! See \ref H5BlockWriteFieldAttribString
!! \return 0 on success or error code
!<
INTEGER*8 FUNCTION h5bl_writefieldattrib_string ( filehandle, field_name, attrib_name, attrib_value )
    INTEGER*8, INTENT(IN) :: filehandle
    CHARACTER(LEN=*), INTENT(IN) :: field_name    ! The name of the field
    CHARACTER(LEN=*), INTENT(IN) :: attrib_name   ! The name of the attribute
    CHARACTER(LEN=*), INTENT(IN) :: attrib_value  ! The array of data to write into the attribute
END FUNCTION

!> \ingroup h5block_attrib_f
!! See \ref H5BlockGetNumFieldAttribs
!! \return number of attributes or error code
!<
INTEGER*8 FUNCTION h5bl_getnfieldattribs ( filehandle, field_name )
    INTEGER*8, INTENT(IN) :: filehandle
    CHARACTER(LEN=*), INTENT(IN) :: field_name    ! The name of the field
END FUNCTION
  
!> \ingroup h5block_attrib_f
!! See \ref H5BlockGetFieldAttribInfo
!! \return 0 on success or error code
!<
INTEGER*8 FUNCTION h5bl_getfieldattribinfo ( filehandle, field_name, idx, attrib_name, attrib_nelem)
    INTEGER*8,INTENT(IN) :: filehandle
    CHARACTER(LEN=*), INTENT(IN) :: field_name   ! The name of the field
    INTEGER*8,INTENT(IN) :: idx  ! index of the attribute being queried
    CHARACTER(LEN=*), INTENT(OUT):: attrib_name  ! The name of the attribute
    INTEGER*8,INTENT(OUT):: attrib_nelem ! Number of elements in the attrib array
END FUNCTION
  
!> \ingroup h5block_attrib_f
!! See \ref H5BlockReadFieldAttribString
!! \return 0 on success or error code
!<
INTEGER*8 FUNCTION h5bl_readfieldattrib_string ( filehandle, field_name, attrib_name, attrib_value )
    INTEGER*8, INTENT(IN) :: filehandle
    CHARACTER(LEN=*), INTENT(IN) :: field_name    ! The name of the field
    CHARACTER(LEN=*), INTENT(IN) :: attrib_name   ! name of the attribute to read
    CHARACTER(LEN=*), INTENT(IN) :: attrib_value  ! The array of data to write into the attribute
END FUNCTION
 
