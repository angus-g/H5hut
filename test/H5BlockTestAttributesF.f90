PROGRAM H5BlockTestAttributesF
   IMPLICIT   NONE

   INCLUDE 'mpif.h'
   INCLUDE 'H5PartF90.inc'

   INTERFACE
      INTEGER*8 FUNCTION write_file ( fname, myproc, comm, layout )
        CHARACTER(LEN=*), INTENT(IN) :: fname
        INTEGER, INTENT(IN) ::          myproc
        INTEGER, INTENT(IN) ::          comm
        INTEGER*8, INTENT(IN) ::        layout(6)
      END FUNCTION

      INTEGER*8 FUNCTION read_file ( fname, myproc, comm, layout )
        CHARACTER(LEN=*), INTENT(IN) :: fname
        INTEGER, INTENT(IN) ::          myproc
        INTEGER, INTENT(IN) ::          comm
        INTEGER*8, INTENT(IN) ::        layout(6)
      END FUNCTION

   END INTERFACE

   INTEGER :: myproc
   INTEGER :: nprocs
   INTEGER :: mpi_err
   INTEGER*8 :: h5pt_err
   INTEGER :: comm
   INTEGER :: i
   CHARACTER(LEN=32) :: arg_str
   LOGICAL :: opt_read
   LOGICAL :: opt_write
   INTEGER*8 :: layout(6) ! = ( 0, 64, 0, 64, 0, 512 )

   CALL MPI_Init ( mpi_err)
   comm = MPI_COMM_WORLD
   CALL MPI_Comm_rank ( comm, myproc, mpi_err)
   CALL MPI_Comm_size ( comm, nprocs, mpi_err)

   DO i = 1, IARGC ()
      CALL GETARG ( i, arg_str, 32)
      IF ( arg_str == "-r" ) THEN
         opt_read = .TRUE.
      ELSE IF ( arg_str == "-w" ) THEN
         opt_write = .TRUE.
      ELSE
         PRINT *, "Illegal option ", arg_str, "\n"
         PRINT *, "Usage: H5BlockTestAttributesF -w | -r"
      END IF

   END DO

   h5pt_err = h5pt_set_verbosity_level ( 4_8 )

   IF ( opt_write ) THEN
      h5pt_err = write_file ( "blockfile0.h5", myproc, comm, layout )
   ELSE IF ( opt_read ) THEN
      h5pt_err = read_file ( "blockfile0.h5", myproc, comm, layout )
   ENDIF

   CALL MPI_Finalize()
END PROGRAM
