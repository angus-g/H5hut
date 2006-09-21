PROGRAM H5BlockTestAttributesF
   IMPLICIT   NONE

   INCLUDE 'H5PartF90.inc'
   INCLUDE 'H5BlockF90.inc'

   INTEGER :: myproc = 0
   INTEGER :: nprocs = 1
   INTEGER*8 :: h5pt_err
   INTEGER :: i
   CHARACTER(LEN=32) :: arg_str
   INTEGER :: opt_read = 0
   INTEGER :: opt_write = 0
   INTEGER*8 :: layout(6)
   DATA layout / 1, 64, 1, 64, 1, 512 /

   DO i = 1, IARGC ()
      CALL GETARG ( i, arg_str )
      PRINT *, arg_str
      IF ( arg_str == "-r" ) THEN
         PRINT *, "Reading file"
         opt_read = 1
      ELSE IF ( arg_str == "-w" ) THEN
         opt_write = 1
      ELSE
         PRINT *, "Illegal option ", arg_str, "\n"
         PRINT *, "Usage: H5BlockTestAttributesF -w | -r"
      END IF

   END DO

   h5pt_err = h5pt_set_verbosity_level ( 4_8 )

   IF ( opt_write == 1 ) THEN
      h5pt_err = write_file ( "blockfile0.h5", myproc, layout )
   ELSE IF ( opt_read == 1 ) THEN
      PRINT *, "Calling read_file"
      h5pt_err = read_file ( "blockfile0.h5", myproc, layout )
   ENDIF


   CONTAINS

   INTEGER*8 FUNCTION write_file ( fname, myproc, layout )
     CHARACTER(LEN=*), INTENT(IN) :: fname
     INTEGER, INTENT(IN) ::          myproc
     INTEGER*8, INTENT(IN) ::        layout(6)

     write_file = 0
   END FUNCTION write_file

   INTEGER*8 FUNCTION read_file ( fname, myproc, layout )
     CHARACTER(LEN=*), INTENT(IN) :: fname
     INTEGER, INTENT(IN) ::          myproc
     INTEGER*8, INTENT(IN) ::        layout(6)
     
     INTEGER*8 :: file
     INTEGER*8 :: timestep = 1

     PRINT *, "PROC[",myproc,"]: Open file ",fname," for reading ..."

     file = h5pt_openr ( fname )
     if ( file == 0 ) THEN
        read_file = -1
        RETURN
     ENDIF
     PRINT *, "file: ", file

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

   INTEGER*8 FUNCTION read_field ( file, myproc, layout )
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
     REAL*8 :: value
  
     INTEGER*8 data(64,64,512)

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
     !ALLOCATE ( data ( i_dims, j_dims, k_dims ) )

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
              value = k + 1000*j + 100000*i
              if ( data(i,j,k) /= value ) THEN
                 PRINT *, "data(",i,",",j,",",k,") = ",data(i,j,k), " /= ",value
              END IF
           END DO
        END DO
     END DO
     read_field = 0
   END FUNCTION read_field


END PROGRAM
