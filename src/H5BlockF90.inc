      INTERFACE
        INTEGER*8 FUNCTION h5bl_define_3d_layout ( filehandle, i_start, i_end, j_start, j_end, k_start, k_end )
          INTEGER*8, INTENT(IN) :: filehandle
          INTEGER*8, INTENT(IN) :: i_start
          INTEGER*8, INTENT(IN) :: i_end
          INTEGER*8, INTENT(IN) :: j_start
          INTEGER*8, INTENT(IN) :: j_end
          INTEGER*8, INTENT(IN) :: k_start
          INTEGER*8, INTENT(IN) :: k_end
        END FUNCTION

        INTEGER*8 FUNCTION h5bl_3d_read_scalar ( filehandle, name, data )
          INTEGER*8, INTENT(IN) :: filehandle
          CHARACTER(LEN=*), INTENT(IN) :: name
	  INTEGER*8, INTENT(OUT) :: data(:,:,:)
	END FUNCTION

        INTEGER*8 FUNCTION h5bl_3d_read_3dvector ( filehandle, name, x, y, z )
          INTEGER*8, INTENT(IN) :: filehandle
          CHARACTER(LEN=*), INTENT(IN) :: name
	  INTEGER*8, INTENT(OUT) :: x(:,:,:)
	  INTEGER*8, INTENT(OUT) :: y(:,:,:)
	  INTEGER*8, INTENT(OUT) :: z(:,:,:)
	END FUNCTION
	
        INTEGER*8 FUNCTION h5bl_3d_write_scalar ( filehandle, name, data )
          INTEGER*8, INTENT(IN) :: filehandle
          CHARACTER(LEN=*), INTENT(IN) :: name
	  INTEGER*8, INTENT(IN) :: data(:,:,:)
	END FUNCTION

        INTEGER*8 FUNCTION h5bl_3d_write_3dvector ( filehandle, name, x, y, z )
          INTEGER*8, INTENT(IN) :: filehandle
          CHARACTER(LEN=*), INTENT(IN) :: name
	  INTEGER*8, INTENT(IN) :: x(:,:,:)
	  INTEGER*8, INTENT(IN) :: y(:,:,:)
	  INTEGER*8, INTENT(IN) :: z(:,:,:)
	END FUNCTION
	
        INTEGER*8 FUNCTION h5bl_getnumfields ( filehandle )
          INTEGER*8, INTENT(IN) :: filehandle
        END FUNCTION

        INTEGER*8 FUNCTION h5bl_getfieldinfo ( filehandle, idx, field_name, len_field_name, grid_rank, grid_dims, field_dims )
          INTEGER*8, INTENT(IN) :: filehandle
          INTEGER*8, INTENT(IN) :: idx,
          CHARACTER(LEN=*), INTENT(OUT) :: field_name
          INTEGER*8, INTENT(IN) :: len_field_name
          INTEGER*8, INTENT(OUT) :: grid_rank
          INTEGER*8, INTENT(OUT) :: grid_dims(:)
          INTEGER*8, INTENT(OUT) :: field_dims
        END FUNCTION


