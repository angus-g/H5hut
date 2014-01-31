Program H5BlockParTestScalarFieldF
   IMPLICIT   NONE
#ifdef PARALLEL_IO
   INCLUDE 'mpif.h'
#endif
   INCLUDE 'H5hutF.h'

   INTEGER :: nargs = 0
   INTEGER :: myproc = 0
   INTEGER :: nprocs = 1
   INTEGER :: comm = 0
   INTEGER :: mpi_err
   INTEGER*8 :: h5_err
   INTEGER :: i
   CHARACTER(LEN=32) :: arg_str
   INTEGER :: opt_read = 0
   INTEGER :: opt_write = 0
   INTEGER :: opt_with_ghosts = 0
   CHARACTER(LEN=128) :: fname
   INTEGER*8 :: layout   (6)
   INTEGER*8 :: layout1  (6,1)
   INTEGER*8 :: layout8  (6,8)
   INTEGER*8 :: layout8g (6,8)
   INTEGER*8 :: layout16 (6,16)
   INTEGER*8 :: layout16g(6,16)
   INTEGER*8 :: layout32 (6,32)
   INTEGER*8 :: layout32g(6,32)

   DATA layout1  / 1,64,  1,64,   1,512 /

   DATA layout8  / 1,64,  1,64,   1, 64, &
 		   1,64,  1,64,  65,128, &
		   1,64,  1,64, 129,192, &
		   1,64,  1,64, 193,256, &
		   1,64,  1,64, 257,320, &
		   1,64,  1,64, 321,384, &
		   1,64,  1,64, 385,448, &
		   1,64,  1,64, 449,512 /

   DATA layout8g / 1,64,  1,64,   1, 65, &
		   1,64,  1,64,  64,129, &
		   1,64,  1,64, 128,193, &
		   1,64,  1,64, 192,257, &
		   1,64,  1,64, 256,321, &
		   1,64,  1,64, 320,385, &
		   1,64,  1,64, 384,449, &
		   1,64,  1,64, 448,512  /

   DATA layout16 / 1,64,  1,32,   1, 64, &
		   1,64, 33,64,   1, 64, &
		   1,64,  1,32,  65,128, &
		   1,64, 33,64,  65,128, &
		   1,64,  1,32, 129,192, &
		   1,64, 33,64, 129,192, &
		   1,64,  1,32, 193,256, &
		   1,64, 33,64, 193,256, &
		   1,64,  1,32, 257,320, &
		   1,64, 33,64, 257,320, &
		   1,64,  1,32, 321,384, &
		   1,64, 33,64, 321,384, &
		   1,64,  1,32, 385,448, &
		   1,64, 33,64, 385,448, &
		   1,64,  1,32, 449,512, &
		   1,64, 33,64, 449,512 /

   DATA layout16g/ 1,64,  1,33,   1, 65, &
		   1,64, 32,64,   1, 65, &
		   1,64,  1,33,  64,129, &
		   1,64, 32,64,  64,129, &
		   1,64,  1,33, 128,193, &
		   1,64, 32,64, 128,193, &
		   1,64,  1,33, 192,257, &
		   1,64, 32,64, 192,257, &
		   1,64,  1,33, 256,321, &
		   1,64, 32,64, 256,321, &
		   1,64,  1,33, 320,385, &
		   1,64, 32,64, 320,385, &
		   1,64,  1,33, 384,449, &
		   1,64, 32,64, 384,449, &
		   1,64,  1,33, 448,512, &
		   1,64, 32,64, 448,512  /

   DATA layout32 / 1,32,  1,32,   1, 64, &
		   1,32, 33,64,   1, 64, &
		  33,64,  1,32,   1, 64, &
		  33,64, 33,64,   1, 64, &
		   1,32,  1,32,  65,128, &
		   1,32, 33,64,  65,128, &
		  33,64,  1,32,  65,128, &
		  33,64, 33,64,  65,128, &
		   1,32,  1,32, 129,192, &
		   1,32, 33,64, 129,192, &
		  33,64,  1,32, 129,192, &
		  33,64, 33,64, 129,192, &
		   1,32,  1,32, 193,256, &
		   1,32, 33,64, 193,256, &
		  33,64,  1,32, 193,256, &
		  33,64, 33,64, 193,256, &
		   1,32,  1,32, 257,320, &
		   1,32, 33,64, 257,320, &
		  33,64,  1,32, 257,320, &
		  33,64, 33,64, 257,320, &
		   1,32,  1,32, 321,384, &
		   1,32, 33,64, 321,384, &
		  33,64,  1,32, 321,384, &
		  33,64, 33,64, 321,384, &
		   1,32,  1,32, 385,448, &
		   1,32, 33,64, 385,448, &
		  33,64,  1,32, 385,448, &
		  33,64, 33,64, 385,448, &
		   1,32,  1,32, 449,512, &
		   1,32, 33,64, 449,512, &
		  33,64,  1,32, 449,512, &
		  33,64, 33,64, 449,512  /

   DATA layout32G/ 1,33,  1,33,   1, 65, &
		   1,33, 32,64,   1, 65, &
		  32,64,  1,33,   1, 65, &
		  32,64, 32,64,   1, 65, &
		   1,33,  1,33,  64,129, &
		   1,33, 32,64,  64,129, &
		  32,64,  1,33,  64,129, &
		  32,64, 32,64,  64,129, &
		   1,33,  1,33, 128,193, &
		   1,33, 32,64, 128,193, &
		  32,64,  1,33, 128,193, &
		  32,64, 32,64, 128,193, &
		   1,33,  1,33, 192,257, &
		   1,33, 32,64, 192,257, &
		  32,64,  1,33, 192,257, &
		  32,64, 32,64, 192,257, &
		   1,33,  1,33, 256,321, &
		   1,33, 32,64, 256,321, &
		  32,64,  1,33, 256,321, &
		  32,64, 32,64, 256,321, &
		   1,33,  1,33, 320,385, &
		   1,33, 32,64, 320,385, &
		  32,64,  1,33, 320,385, &
		  32,64, 32,64, 320,385, &
		   1,33,  1,33, 384,449, &
		   1,33, 32,64, 384,449, &
		  32,64,  1,33, 384,449, &
		  32,64, 32,64, 384,449, &
		   1,33,  1,33, 448,512, &
		   1,33, 32,64, 448,512, &
		  32,64,  1,33, 448,512, &
		  32,64, 32,64, 448,512  /
   nargs = IARGC ()
   IF (nargs == 0) THEN
      PRINT *, "Usage: H5BlockExampleF -w | -r [-g]"      
      CALL EXIT (1)
   END IF
   DO i = 1, nargs
      CALL GETARG ( i, arg_str )
      IF ( arg_str == "-r" ) THEN
         opt_read = 1
      ELSE IF ( arg_str == "-w" ) THEN
         opt_write = 1
      ELSE IF ( arg_str == "-g" ) THEN
         opt_with_ghosts = 1
      ELSE
         PRINT *, "Illegal option ", arg_str, "\n"
         PRINT *, "Usage: H5BlockExampleF -w | -r [-g]"
         CALL EXIT (1)
      END IF
   END DO

#ifdef PARALLEL_IO
   comm = MPI_COMM_WORLD
   CALL MPI_Init ( mpi_err )
   comm = MPI_COMM_WORLD
   CALL MPI_Comm_rank ( comm, myproc, mpi_err)
   CALL MPI_Comm_size ( comm, nprocs, mpi_err)
#endif
   SELECTCASE ( nprocs )
   CASE ( 1 )
      fname = "blockfile1.h5"
      layout = layout1 ( :, myproc+1 )

   CASE ( 8 )
      IF ( opt_with_ghosts == 1 ) THEN
         fname = "blockfile8G.h5"
         layout = layout8g ( :, myproc+1 )
      ELSE
         fname = "blockfile8.h5"
         layout = layout8 ( :, myproc+1 )
      END IF

   CASE ( 16 )
      IF ( opt_with_ghosts == 1 ) THEN
         fname = "blockfile16G.h5"
         layout = layout16g ( :, myproc+1 )
      ELSE
         fname = "blockfile16.h5"
         layout = layout16 ( :, myproc+1 )
      END IF

   CASE ( 32 )
      IF ( opt_with_ghosts == 1 ) THEN
         fname = "blockfile32G.h5"
         layout = layout32g ( :, myproc+1 )
      ELSE
         fname = "blockfile32.h5"
         layout = layout32 ( :, myproc+1 )
      END IF

   CASE DEFAULT
      PRINT *, "Run this test on 1, 8, 16 or 32 cores!"
#ifdef PARALLEL_IO
      CALL MPI_Finalize
#endif
      CALL EXIT (1)
   END SELECT

   h5_err = h5_set_verbosity_level ( 511_8 )

   IF ( opt_write == 1 ) THEN
      h5_err = write_file ( fname, myproc, comm, layout )
      IF ( h5_err < 0 ) THEN
         PRINT "('[proc ', I3, ']: Faild to write file ', A, '!')", myproc, fname
      END IF

   ELSE IF ( opt_read == 1 ) THEN
      h5_err = read_file ( fname, myproc, comm, layout )
      IF ( h5_err < 0 ) THEN
         PRINT "('[proc ', I3, ']: Faild to read file ', A, '!')", myproc, fname
      END IF

   ENDIF
#ifdef PARALLEL_IO
   PRINT "('[proc ', I3, ']: Cleanup.')", myproc
   CALL MPI_Finalize
#endif
   PRINT "('[proc ', I3, ']: Done.')", myproc
   CALL EXIT (0)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
   CONTAINS

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !!
   INTEGER*8 FUNCTION write_file ( fname, myproc, comm, layout )
     IMPLICIT   NONE

     CHARACTER(LEN=*), INTENT(IN) :: fname
     INTEGER, INTENT(IN) ::          myproc
     INTEGER, INTENT(IN) ::          comm
     INTEGER*8, INTENT(IN) ::        layout(6)

     INCLUDE 'H5hutF.h'

     INTEGER*8 :: file
     INTEGER*8 :: timestep = 1

     PRINT "('[proc ', I3, ']: Open file for writing ...')", myproc
#ifdef PARALLEL_IO
     file = h5_openw_par (fname, comm)
#else
     file = h5_openw_par (fname)
#endif
     IF ( file == 0 ) THEN
        write_file = -1
        RETURN
     ENDIF

     h5_err = h5_setstep ( file, timestep )
     IF ( h5_err < 0 ) THEN
        write_file = h5_err
        RETURN
     ENDIF

     h5_err =  write_field ( file, myproc, layout )
     IF ( h5_err < 0 ) THEN
        write_file = h5_err
        RETURN
     ENDIF

     h5_err =  write_attributes ( file )
     IF ( h5_err < 0 ) THEN
        write_file = h5_err
        RETURN
     ENDIF

     h5_err = h5_close ( file )
     IF ( h5_err < 0 ) THEN
        write_file = h5_err
        RETURN
     ENDIF

     write_file = 0
   END FUNCTION write_file

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !!
   INTEGER*8 FUNCTION write_field ( file, myproc, layout )
     IMPLICIT   NONE

     INTEGER*8, INTENT(IN) :: file
     INTEGER, INTENT(IN) ::   myproc
     INTEGER*8, INTENT(IN) :: layout(6)

     INCLUDE 'H5hutF.h'

     INTEGER*8 :: i, j, k
     INTEGER*8 :: i_start
     INTEGER*8 :: i_end
     INTEGER*8 :: j_start
     INTEGER*8 :: j_end
     INTEGER*8 :: k_start
     INTEGER*8 :: k_end
     INTEGER*8 :: i_dims
     INTEGER*8 :: j_dims
     INTEGER*8 :: k_dims
     REAL*8 :: value
  
     REAL*8, DIMENSION(:,:,:), ALLOCATABLE :: data

     i_start = layout(1)
     i_end   = layout(2)
     j_start = layout(3)
     j_end   = layout(4)
     k_start = layout(5)
     k_end   = layout(6)
     i_dims  = i_end - i_start + 1
     j_dims  = j_end - j_start + 1
     k_dims  = k_end - k_start + 1

     ALLOCATE ( data (i_dims,j_dims, k_dims) )

     PRINT "('[proc ', I3, ']: Defining layout for writing ...')", myproc
     PRINT "('[proc ', I3, ']: ', I3, ':', I3, ', ', I3, ':', I3,', ', I3, ':', I3)", &
          myproc, &
          i_start, i_end, &
          j_start, j_end, &
          k_start, k_end

     h5_err = h5bl_3d_setview ( file, i_start, i_end, j_start, j_end, k_start, k_end )
     IF ( h5_err < 0 ) THEN
        write_field = h5_err
        RETURN
     END IF

     DO i = 1, i_dims
        DO j = 1, j_dims
           DO k = 1, k_dims
              value = (k-1) + 1000*(j-1) + 100000*(i-1) + 10000000*myproc
              data(i,j,k) = value
           END DO
        END DO
     END DO

     PRINT "('[proc ', I3, ']: Writing field ...')", myproc
     h5_err = h5bl_3d_write_scalar_field_r8 ( file, "TestField", data )
     IF ( h5_err < 0 ) THEN
        write_field = h5_err
        RETURN
     END IF

     write_field = 0
   END FUNCTION write_field

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !!
   INTEGER*8 FUNCTION write_attributes ( file )
     IMPLICIT   NONE
     INTEGER*8, INTENT(IN) :: file
     
     INCLUDE 'H5hutF.h'

     INTEGER*8 :: h5_err = 0
     CHARACTER(LEN=128) :: s_val
     INTEGER*8 :: i8_val(1)
     INTEGER*4 :: i4_val(1)
     REAL*8 :: r8_val(1)
     REAL*4 :: r4_val(1)

     PRINT "('[proc ', I3, ']: Writing string attribute ...')", myproc
     s_val = "42"
     h5_err = h5bl_writefieldattrib_string ( file, "TestField", "TestString", s_val ) 
     IF ( h5_err < 0 ) THEN
        write_attributes = h5_err
        RETURN
     END IF

     PRINT "('[proc ', I3, ']: Writing int64 attribute ...')", myproc
     i8_val(1) = 42
     h5_err = h5bl_writefieldattrib_i8 ( file, "TestField", "TestInt64", i8_val, 1_8 )
     IF ( h5_err < 0 ) THEN
        write_attributes = h5_err
        RETURN
     END IF

     PRINT "('[proc ', I3, ']: Writing int32 attribute ...')", myproc
     i4_val(1) = 42
     h5_err = h5bl_writefieldattrib_i4 ( file, "TestField", "TestInt32", i4_val, 1_8 )
     IF ( h5_err < 0 ) THEN
        write_attributes = h5_err
        RETURN
     END IF

     PRINT "('[proc ', I3, ']: Writing float64 attribute ...')", myproc
     r8_val(1) = 42.0
     h5_err = h5bl_writefieldattrib_r8 ( file, "TestField", "TestFloat64", r8_val, 1_8 )
     IF ( h5_err < 0 ) THEN
        write_attributes = h5_err
        RETURN
     END IF

     PRINT "('[proc ', I3, ']: Writing float32 attribute ...')", myproc
     r4_val(1) = 42.0
     h5_err = h5bl_writefieldattrib_r4 ( file, "TestField", "TestFloat32", r4_val, 1_8 )
     IF ( h5_err < 0 ) THEN
        write_attributes = h5_err
        RETURN
     END IF
 
     write_attributes = 0
   END FUNCTION write_attributes

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !!
   INTEGER*8 FUNCTION read_file ( fname, myproc, comm, layout )
     IMPLICIT   NONE

     CHARACTER(LEN=*), INTENT(IN) :: fname
     INTEGER, INTENT(IN) ::          myproc
     INTEGER, INTENT(IN) ::          comm
     INTEGER*8, INTENT(IN) ::        layout(6)

     INCLUDE 'H5hutF.h'

     INTEGER*8 :: file
     INTEGER*8 :: timestep = 1

     PRINT "('[proc ', I3, ']: Open file for reading ...')", myproc
#ifdef PARALLEL_IO
     file = h5_openr_par ( fname, comm )
#else
     file = h5_openr (fname
#endif
     if ( file == 0 ) THEN
        read_file = -1
        RETURN
     ENDIF

     h5_err = h5_setstep ( file, timestep )
     IF ( h5_err < 0 ) THEN
        read_file = -1
        RETURN
     ENDIF

     h5_err =  read_field ( file, myproc, layout )
     IF ( h5_err < 0 ) THEN
        read_file = -1
        RETURN
     ENDIF

     h5_err = read_attributes ( file )
     IF ( h5_err < 0 ) THEN
        read_file = -1
        RETURN
     ENDIF

     h5_err = h5_close ( file )
     IF ( h5_err < 0 ) THEN
        read_file = -1
        RETURN
     ENDIF

     read_file = 0
   END FUNCTION read_file

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !!
   INTEGER*8 FUNCTION read_field ( file, myproc, layout )
     IMPLICIT   NONE

     INTEGER*8, INTENT(IN) :: file
     INTEGER, INTENT(IN) ::          myproc
     INTEGER*8, INTENT(IN) ::        layout(6)

     INCLUDE 'H5hutF.h'

     INTEGER*8 :: i, j, k
     INTEGER*8 :: i_start, i_start2
     INTEGER*8 :: i_end, i_end2
     INTEGER*8 :: j_start, j_start2
     INTEGER*8 :: j_end, j_end2
     INTEGER*8 :: k_start, k_start2
     INTEGER*8 :: k_end, k_end2
     INTEGER*8 :: i_dims
     INTEGER*8 :: j_dims
     INTEGER*8 :: k_dims
     INTEGER*8 :: ri, rj, rk, proc
     REAL*8 :: value
  
     REAL*8, DIMENSION(:,:,:), ALLOCATABLE :: data

     i_start = layout(1)
     i_end   = layout(2)
     j_start = layout(3)
     j_end   = layout(4)
     k_start = layout(5)
     k_end   = layout(6)
     i_dims  = i_end - i_start + 1
     j_dims  = j_end - j_start + 1
     k_dims  = k_end - k_start + 1

     ALLOCATE ( data (i_dims, j_dims, k_dims) )

     PRINT "('[proc ', I3, ']: Defining layout for reading ...')", myproc
     PRINT "('[proc ', I3, ']: ', I3, ':', I3, ', ', I3, ':', I3,', ', I3, ':', I3)", &
          myproc, &
          i_start, i_end, &
          j_start, j_end, &
          k_start, k_end

     h5_err = h5bl_3d_setview ( file, i_start, i_end, j_start, j_end, k_start, k_end )
     IF ( h5_err < 0 ) THEN
        read_field = -1
        RETURN
     END IF

     PRINT "('[proc ', I3, ']: Reading field ...')", myproc
     h5_err = h5bl_3d_read_scalar_field_r8 ( file, "TestField", data )
     IF ( h5_err < 0 ) THEN
        read_field = -1
        RETURN
     END IF

     DO i = 1, i_dims
        DO j = 1, j_dims
           DO k = 1, k_dims
              value = (k-1) + 1000*(j-1) + 100000*(i-1) + 10000000*myproc
              if ( data(i,j,k) /= value ) THEN
                 PRINT "('[proc ', I3, ']: error: data(',I4,',',I4,',',I4,') = ',F10.2,' /= ',F10.2)", &
                      i, j, k, data(i,j,k), value
                 read_field = -2
                 RETURN
              END IF
           END DO
        END DO
     END DO
     read_field = 0
   END FUNCTION read_field

   INTEGER*8 FUNCTION read_attributes ( file )
     IMPLICIT NONE
     INTEGER*8, INTENT(IN) :: file

     INCLUDE 'H5hutF.h'

     INTEGER*8 :: h5_err = 0
     CHARACTER(LEN=128) :: s_val
     INTEGER*8 :: i8_val(1)
     INTEGER*4 :: i4_val(1)
     REAL*8 :: r8_val(1)
     REAL*4 :: r4_val(1)

     PRINT "('[proc ', I3, ']: Reading string attribute ...')", myproc
     h5_err = h5bl_readfieldattrib_string ( file, "TestField", "TestString", s_val ) 
     IF ( h5_err < 0 ) THEN
        PRINT "('[proc ', I3, ']: Oops ...')", myproc
        read_attributes = h5_err
        RETURN
     END IF
     IF ( s_val /= "42" ) THEN
        PRINT "('[proc ', I3, ']: Error reading string attribute: Value is ', A, ' but should be 42')", &
             myproc, s_val
     END IF

     PRINT "('[proc ', I3, ']: Reading int64 attribute ...')", myproc
     h5_err = h5bl_readfieldattrib_i8 ( file, "TestField", "TestInt64", i8_val )
     IF ( h5_err < 0 ) THEN
        PRINT "('[proc ', I3, ']: Oops ...')", myproc
        read_attributes = h5_err
        RETURN
     END IF
     IF ( i8_val(1) /= 42 ) THEN
        PRINT "('[proc ', I3, ']: Error reading int64 attribute: Value is ', I8, ' but should be 42')", &
             myproc, i8_val(1)
     END IF

     PRINT "('[proc ', I3, ']: Reading int32 attribute ...')", myproc
     h5_err = h5bl_readfieldattrib_i4 ( file, "TestField", "TestInt32", i4_val )
     IF ( h5_err < 0 ) THEN
        PRINT "('[proc ', I3, ']: Oops ...')", myproc
        read_attributes = h5_err
        RETURN
     END IF
     IF ( i4_val(1) /= 42 ) THEN
        PRINT "('[proc ', I3, ']: Error reading int32 attribute: Value is ', I8, ' but should be 42')", &
             myproc, i4_val(1)
     END IF

     PRINT "('[proc ', I3, ']: Reading float64 attribute ...')", myproc
     h5_err = h5bl_readfieldattrib_r8 ( file, "TestField", "TestFloat64", r8_val )
     IF ( h5_err < 0 ) THEN
        PRINT "('[proc ', I3, ']: Oops ...')", myproc
        read_attributes = h5_err
        RETURN
     END IF
     IF ( r8_val(1) /= 42.0 ) THEN
        PRINT "('[proc ', I3, ']: Error reading float64 attribute: Value is ', F10.2, ' but should be 42.0')", &
             myproc, r8_val(1)
     END IF

     PRINT "('[proc ', I3, ']: Reading float32 attribute ...')", myproc
     h5_err = h5bl_readfieldattrib_r4 ( file, "TestField", "TestFloat32", r4_val )
     IF ( h5_err < 0 ) THEN
        PRINT "('[proc ', I3, ']: Oops ...')", myproc
        read_attributes = h5_err
        RETURN
     END IF
     IF ( r4_val(1) /= 42.0 ) THEN
        PRINT "('[proc ', I3, ']: Error reading float32 attribute: Value is ', F10.2, ' but should be 42.0')", &
             myproc, r4_val(1)
     END IF

     read_attributes = h5_err
   END FUNCTION read_attributes

 END PROGRAM
