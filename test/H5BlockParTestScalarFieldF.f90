PROGRAM H5BlockParTestScalarFieldF
   IMPLICIT   NONE

   INCLUDE 'mpif.h'
   INCLUDE 'H5PartF90.inc'
   INCLUDE 'H5BlockF90.inc'

   INTEGER :: myproc = 0
   INTEGER :: nprocs = 1
   INTEGER :: comm = MPI_COMM_WORLD
   INTEGER :: mpi_err
   INTEGER*8 :: h5pt_err
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
		   1,64,  1,64, 257,321, &
		   1,64,  1,64, 321,384, &
		   1,64,  1,64, 385,448, &
		   1,64,  1,64, 449,512 /

   DATA layout8g / 1,64,  1,64,   1, 65, &
		   1,64,  1,64,  64,129, &
		   1,64,  1,64, 128,193, &
		   1,64,  1,64, 192,257, &
		   1,64,  1,64, 256,321, &
		   1,64,  1,64, 321,385, &
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
		   1,64,  1,32, 257,321, &
		   1,64, 33,64, 257,321, &
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
		   1,64,  1,33, 321,385, &
		   1,64, 32,64, 321,385, &
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
		   1,32,  1,32, 257,321, &
		   1,32, 33,64, 257,321, &
		  33,64,  1,32, 257,321, &
		  33,64, 33,64, 257,321, &
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
		   1,33,  1,33, 321,385, &
		   1,33, 32,64, 321,385, &
		  32,64,  1,33, 321,385, &
		  32,64, 32,64, 331,385, &
		   1,32,  1,32, 384,449, &
		   1,32, 32,64, 384,449, &
		  32,64,  1,32, 384,449, &
		  32,64, 32,64, 384,449, &
		   1,33,  1,33, 448,512, &
		   1,33, 32,64, 448,512, &
		  32,64,  1,33, 448,512, &
		  32,64, 32,64, 448,512  /

   CALL MPI_Init ( mpi_err )
   comm = MPI_COMM_WORLD
   CALL MPI_Comm_rank ( comm, myproc, mpi_err)
   CALL MPI_Comm_size ( comm, nprocs, mpi_err)

   DO i = 1, IARGC ()
      CALL GETARG ( i, arg_str )
      PRINT *, arg_str
      IF ( arg_str == "-r" ) THEN
         PRINT *, "Reading file"
         opt_read = 1
      ELSE IF ( arg_str == "-w" ) THEN
         opt_write = 1
      ELSE IF ( arg_str == "-g" ) THEN
         opt_with_ghosts = 1
      ELSE
         PRINT *, "Illegal option ", arg_str, "\n"
         PRINT *, "Usage: H5BlockTestAttributesF -w | -r [-g]"
      END IF

   END DO

   SELECTCASE ( nprocs )
   CASE ( 1 )
      fname = "Fblockfile1.h5"
      layout = layout1 ( :, myproc+1 )

   CASE ( 8 )
      IF ( opt_with_ghosts == 1 ) THEN
         fname = "Fblockfile8G.h5"
         layout = layout8g ( :, myproc+1 )
      ELSE
         fname = "Fblockfile8.h5"
         layout = layout8 ( :, myproc+1 )
      END IF

   CASE ( 16 )
      IF ( opt_with_ghosts == 1 ) THEN
         fname = "Fblockfile16G.h5"
         layout = layout16g ( :, myproc+1 )
      ELSE
         fname = "Fblockfile16.h5"
         layout = layout16 ( :, myproc+1 )
      END IF

   CASE ( 32 )
      IF ( opt_with_ghosts == 1 ) THEN
         fname = "Fblockfile32G.h5"
         layout = layout32g ( :, myproc+1 )
      ELSE
         fname = "Fblockfile32.h5"
         layout = layout32 ( :, myproc+1 )
      END IF

   CASE DEFAULT
      print *, "Run this test with 1, 8, 16 or 32 procs!"

   END SELECT

   h5pt_err = h5pt_set_verbosity_level ( 3_8 )

   IF ( opt_write == 1 ) THEN
      h5pt_err = write_file ( fname, myproc, comm, layout )
      IF ( h5pt_err < 0 ) THEN
         PRINT *, "Faild to write file ", fname, "!"
      END IF

   ELSE IF ( opt_read == 1 ) THEN
      h5pt_err = read_file ( fname, myproc, comm, layout )
      IF ( h5pt_err < 0 ) THEN
         PRINT *, "Faild to write file ", fname, "!"
      END IF

   ENDIF

   CALL MPI_Finalize

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

     INTEGER*8 :: file
     INTEGER*8 :: timestep = 1

     PRINT *, "PROC[",myproc,"]: Open file ",fname," for writing ..."

     file = h5pt_openw_par ( fname, comm )
     if ( file == 0 ) THEN
        write_file = -1
        RETURN
     ENDIF
     PRINT *, "file: ", file

     h5pt_err = h5pt_setstep ( file, timestep )
     IF ( h5pt_err < 0 ) THEN
        write_file = h5pt_err
        RETURN
     ENDIF

     h5pt_err =  write_field ( file, myproc, layout )
     IF ( h5pt_err < 0 ) THEN
        write_file = h5pt_err
        RETURN
     ENDIF

     h5pt_err = h5pt_close ( file )
     IF ( h5pt_err < 0 ) THEN
        write_file = h5pt_err
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

     PRINT *, "Reading field ..."
     i_start = layout(1)
     i_end   = layout(2)
     j_start = layout(3)
     j_end   = layout(4)
     k_start = layout(5)
     k_end   = layout(6)
     i_dims  = i_end - i_start + 1
     j_dims  = j_end - j_start + 1
     k_dims  = k_end - k_start + 1

     PRINT *, "dims: (", i_dims, j_dims, k_dims, ")"
     ALLOCATE ( data (i_dims,j_dims, k_dims) )

     PRINT *, "Defining Layout ..."
     h5pt_err = h5bl_define3dlayout ( file, i_start, i_end, j_start, j_end, k_start, k_end )
     IF ( h5pt_err < 0 ) THEN
        write_field = h5pt_err
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

     PRINT *, "Writing field ..."
     h5pt_err = h5bl_3d_write_scalar_field ( file, "TestField", data )
     IF ( h5pt_err < 0 ) THEN
        write_field = h5pt_err
        RETURN
     END IF

     write_field = 0
   END FUNCTION write_field

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !!
   INTEGER*8 FUNCTION read_file ( fname, myproc, comm, layout )
     IMPLICIT   NONE

     CHARACTER(LEN=*), INTENT(IN) :: fname
     INTEGER, INTENT(IN) ::          myproc
     INTEGER, INTENT(IN) ::          comm
     INTEGER*8, INTENT(IN) ::        layout(6)
     
     INTEGER*8 :: file
     INTEGER*8 :: timestep = 1

     PRINT *, "PROC[",myproc,"]: Open file ",fname," for reading ..."

     file = h5pt_openr_par ( fname, comm )
     if ( file == 0 ) THEN
        read_file = -1
        RETURN
     ENDIF

     h5pt_err = h5pt_setstep ( file, timestep )
     IF ( h5pt_err < 0 ) THEN
        read_file = -1
        RETURN
     ENDIF

     h5pt_err =  read_field ( file, myproc, layout )
     IF ( h5pt_err < 0 ) THEN
        read_file = -1
        RETURN
     ENDIF

     h5pt_err = h5pt_close ( file )
     IF ( h5pt_err < 0 ) THEN
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
     INTEGER*8 :: ri, rj, rk, proc
     REAL*8 :: value
  
     REAL*8, DIMENSION(:,:,:), ALLOCATABLE :: data

     PRINT *, "Reading field ..."
     i_start = layout(1)
     i_end   = layout(2)
     j_start = layout(3)
     j_end   = layout(4)
     k_start = layout(5)
     k_end   = layout(6)
     i_dims  = i_end - i_start + 1
     j_dims  = j_end - j_start + 1
     k_dims  = k_end - k_start + 1

     PRINT *, "dims: (", i_dims, j_dims, k_dims, ")"
     ALLOCATE ( data (i_dims,j_dims, k_dims) )

     PRINT *, "Defining Layout ..."
     h5pt_err = h5bl_define3dlayout ( file, i_start, i_end, j_start, j_end, k_start, k_end )
     IF ( h5pt_err < 0 ) THEN
        read_field = -1
        RETURN
     END IF

     PRINT *, "Reading field ..."
     h5pt_err = h5bl_3d_read_scalar_field ( file, "TestField", data )
     IF ( h5pt_err < 0 ) THEN
        read_field = -1
        RETURN
     END IF

     DO i = 1, i_dims
        DO j = 1, j_dims
           DO k = 1, k_dims
              ri = i + layout(1)
              rj = j + layout(3)
              rk = k + layout(5)

              proc = h5bl_get_proc_of ( file, ri, rj, rk )

              h5pt_err = h5bl_get_reduced_partition_of_proc ( file, proc, i_start, i_end, j_start, j_end, k_start, k_end )

              ri = ri - i_start;
              rj = rj - j_start;
              rk = rk - k_start;

              value = (k-1) + 1000*(j-1) + 100000*(i-1)
              if ( data(i,j,k) /= value ) THEN
                 PRINT *, "data(",i,",",j,",",k,") = ",data(i,j,k), " /= ",value
              END IF
           END DO
        END DO
     END DO
     read_field = 0
   END FUNCTION read_field


 END PROGRAM
