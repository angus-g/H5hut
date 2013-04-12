PROGRAM H5PartExampleF
   IMPLICIT   NONE
#ifdef PARALLEL_IO
   INCLUDE 'mpif.h'
#endif
   INCLUDE 'H5hutF.h'

   INTEGER*8 :: h5_err = 0
   INTEGER :: mpi_err
   INTEGER :: num_args = 0
   INTEGER :: i
   CHARACTER(LEN=32) :: arg_str
   INTEGER :: opt_read = 0
   INTEGER :: opt_write = 0
   INTEGER*8 :: opt_debug = 2
   INTEGER :: comm = 0
   INTEGER :: myproc = 0
   INTEGER :: num_procs = 0
   CHARACTER(LEN=128) :: fname = "testfile.h5"
   INTEGER*8 :: file

   REAL*8    :: r8_attrib (2)
   REAL*4    :: r4_attrib (2)
   INTEGER*8 :: i8_attrib (2)
   INTEGER*4 :: i4_attrib (2)
   CHARACTER (LEN=H5_MAX_NAME_LEN) :: string_attrib
   LOGICAL skip

   DATA r8_attrib / 42.0, 43.0 /
   DATA r4_attrib / 42.0, 43.0 /
   DATA i8_attrib / 42, 43 /
   DATA i4_attrib / 42, 43 /
   DATA string_attrib / "The answer is 42." /

   num_args = IARGC ()
   IF (num_args == 0) THEN
      PRINT *, "Usage: H5PartExampleF -w | -r [-g]"      
      CALL EXIT (1)
   END IF
   DO i = 1, num_args
      if (skip .EQV. .TRUE.) THEN
         CYCLE
      END IF
      CALL GETARG (i, arg_str)
      IF (arg_str == "-r") THEN
         opt_read = 1
      ELSE IF (arg_str == "-w") THEN
         opt_write = 1
      ELSE IF (arg_str == "-d") THEN
         CALL GETARG (i+1, arg_str)
         arg_str = TRIM (arg_str)
         READ (arg_str, *, ERR=999) opt_debug
         skip = .TRUE.
         CYCLE
999      PRINT *, "Debug level must be an integer: ", arg_str
         CALL EXIT (1)
      ELSE
         PRINT *, "Illegal option ", arg_str, "\n"
         PRINT *, "Usage: H5BlockExampleF -w | -r [-d LEVEL]"
         CALL EXIT (1)
      END IF
   END DO

#ifdef PARALLEL_IO
   CALL MPI_Init (mpi_err)
   comm = MPI_COMM_WORLD
   CALL MPI_Comm_rank (comm, myproc, mpi_err)
   CALL MPI_Comm_size (comm, num_procs, mpi_err)

   h5_err = h5_set_verbosity_level (opt_debug)

   IF (opt_write == 1) THEN
     PRINT "('[proc ', I3, ']: Open file for writing ...')", myproc
     file = h5_openw_par (fname, comm)
     IF (file == 0) THEN
        PRINT "('[proc ', I3, ']: Error opening file ...')", myproc
        h5_err = -2
        GOTO 911
     ENDIF
     h5_err = write_file (file, myproc, num_procs)
     IF (h5_err < 0) THEN
        PRINT "('[proc ', I3, ']: Failed to write file ', A, '!')", myproc, fname
        GOTO 911
     END IF
  ELSE IF (opt_read == 1) THEN
     PRINT "('[proc ', I3, ']: Open file for reading ...')", myproc
     file = h5_openr_par (fname, comm)
     IF (file == 0) THEN
        PRINT "('[proc ', I3, ']: Error opening file ...')", myproc
        GOTO 911
     ENDIF
     h5_err = read_file (file, myproc, num_procs)
     IF (h5_err < 0) THEN
        PRINT "('[proc ', I3, ']: Failed to read file ', A, '!')", myproc, fname
        GOTO 911
     END IF
  ENDIF

  PRINT "('[proc ', I3, ']: Done.')", myproc
911 CONTINUE  

  h5_err = h5_close (file)
  IF (h5_err < 0) THEN
     PRINT "('[proc ', I3, ']: Error closing file ...')", myproc
  ENDIF

  h5_err = h5_finalize ()
  IF (h5_err < 0) THEN
     PRINT "('[proc ', I3, ']: Error closing H5hut library ...')", myproc
  ENDIF

  CALL MPI_Finalize (mpi_error)

#else
  !! SERIAL CODE
   h5_err = h5_set_verbosity_level (opt_debug)
  IF (opt_write == 1) THEN
     PRINT "('[proc ', I3, ']: Open file for writing ...')", myproc
     file = h5_openw (fname)
     IF (file == 0) THEN
        PRINT "('[proc ', I3, ']: Error opening file ...')", myproc
        h5_err = -2
        GOTO 911
     ENDIF
     h5_err = write_file (file, myproc, num_procs)
     IF (h5_err < 0) THEN
        PRINT "('[proc ', I3, ']: Failed to write file ', A, '!')", myproc, fname
        GOTO 911
     END IF

  ELSE IF (opt_read == 1) THEN
     file = h5_openr (fname)
     IF (file == 0) THEN
        PRINT "('[proc ', I3, ']: Error opening file ...')", myproc
        GOTO 911
     ENDIF
     h5_err = read_file (file, myproc, num_procs)
     IF (h5_err < 0) THEN
        PRINT "('[proc ', I3, ']: Failed to read file ', A, '!')", myproc, fname
        GOTO 911
     END IF
  ENDIF
  PRINT "('[proc ', I3, ']: Done.')", myproc
911 CONTINUE

  h5_err = h5_close (file)
  IF (h5_err < 0) THEN
     PRINT "('[proc ', I3, ']: Error closing file ...')", myproc
  ENDIF

#endif

  CALL EXIT (h5_err)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
   CONTAINS

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !!
   INTEGER*8 FUNCTION write_file (file, myproc, num_procs)
     IMPLICIT   NONE

     INTEGER*8, INTENT(IN) ::        file
     INTEGER, INTENT(IN) ::          myproc
     INTEGER, INTENT(IN) ::          num_procs

     INCLUDE 'H5hutF.h'

     INTEGER   :: mpi_err
     INTEGER*8 :: step = 1
     INTEGER*8 :: num_steps = 2

     h5_err = write_file_attribs (file, myproc)
     IF (h5_err < 0) THEN
        PRINT "('[proc ', I3, ']: Error writing file attributes ...')", myproc
        write_file = h5_err
        RETURN
     ENDIF

     DO step = 1, num_steps
        h5_err = h5_setstep (file, step)
        IF (h5_err < 0) THEN
           PRINT "('[proc ', I3, ']: Error setting step to ', I2)", myproc, step
           write_file = h5_err
           RETURN
        ENDIF

        h5_err = write_step_attribs (file, myproc, num_procs)
        IF (h5_err < 0) THEN
           PRINT "('[proc ', I3, ']: Error writing step attributes ...')", myproc
           write_file = h5_err
           RETURN
        ENDIF

        h5_err = write_data (file, myproc, num_procs)
        IF (h5_err < 0) THEN
           PRINT "('[proc ', I3, ']: Error writing step data ...')", myproc
           write_file = h5_err
           RETURN
        ENDIF

     ENDDO

     write_file = 0
   END FUNCTION write_file

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !!
   INTEGER*8 FUNCTION write_file_attribs (file, myproc)
     IMPLICIT   NONE

     INTEGER*8, INTENT(IN) :: file
     INTEGER, INTENT(IN) :: myproc

     INCLUDE 'H5hutF.h'

     PRINT "('[proc ', I3, ']: Writing file attribute of type H5_FLOAT64_T ...')", myproc
     h5_err = h5_writefileattrib_r8 (file, "r8_attrib", r8_attrib, 2_8)
     IF (h5_err < 0) THEN
        PRINT "('[proc ', I3, ']: Error writing file attribute of type H5_FLOAT64_T')", myproc
        write_file_attribs = h5_err
        RETURN
     ENDIF

     PRINT "('[proc ', I3, ']: Writing file attribute of type H5_FLOAT32_T ...')", myproc
     h5_err = h5_writefileattrib_r4 (file, "r4_attrib", r4_attrib, 2_8)
     IF (h5_err < 0) THEN
        PRINT "('[proc ', I3, ']: Error writing file attribute of type H5_FLOAT32_T')", myproc
        write_file_attribs = h5_err
        RETURN
     ENDIF

     PRINT "('[proc ', I3, ']: Writing file attribute of type H5_INT64_T ...')", myproc
     h5_err = h5_writefileattrib_i8 (file, "i8_attrib", i8_attrib, 2_8)
     IF (h5_err < 0) THEN
        PRINT "('[proc ', I3, ']: Error writing file attribute of type H5_INT64_T')", myproc
        write_file_attribs = h5_err
        RETURN
     ENDIF

     PRINT "('[proc ', I3, ']: Writing file attribute of type H5_INT32_T ...')", myproc
     h5_err = h5_writefileattrib_i4 (file, "i4_attrib", i4_attrib, 2_8)
     IF (h5_err < 0) THEN
        PRINT "('[proc ', I3, ']: Error writing file attribute of type H5_INT32_T')", myproc
        write_file_attribs = h5_err
        RETURN
     ENDIF

     PRINT "('[proc ', I3, ']: Writing file attribute of type H5_STRING_T ...')", myproc
     h5_err = h5_writefileattrib_string (file, "string_attrib", string_attrib)
     IF (h5_err < 0) THEN
        PRINT "('[proc ', I3, ']: Error writing file attribute of type H5_STRING_T')", myproc
        write_file_attribs = h5_err
        RETURN
     ENDIF

     write_file_attribs = 0
   END FUNCTION write_file_attribs

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !!
   INTEGER*8 FUNCTION write_step_attribs (file, myproc, num_procs)
     IMPLICIT   NONE

     INTEGER*8, INTENT(IN) :: file
     INTEGER, INTENT(IN) :: myproc
     INTEGER, INTENT(IN) :: num_procs

     INCLUDE 'H5hutF.h'

     PRINT "('[proc ', I3, ']: Writing step attribute of type H5_FLOAT64_T ...')", myproc
     h5_err = h5_writestepattrib_r8 (file, "r8_attrib", r8_attrib, 2_8)
     IF (h5_err < 0) THEN
        PRINT "('[proc ', I3, ']: Error writing step attribute of type H5_FLOAT64_T')", myproc
        write_step_attribs = h5_err
        RETURN
     ENDIF

     PRINT "('[proc ', I3, ']: Writing step attribute of type H5_FLOAT32_T ...')", myproc
     h5_err = h5_writestepattrib_r4 (file, "r4_attrib", r4_attrib, 2_8)
     IF (h5_err < 0) THEN
        PRINT "('[proc ', I3, ']: Error writing step attribute of type H5_FLOAT32_T')", myproc
        write_step_attribs = h5_err
        RETURN
     ENDIF

     PRINT "('[proc ', I3, ']: Writing step attribute of type H5_INT64_T ...')", myproc
     h5_err = h5_writestepattrib_i8 (file, "i8_attrib", i8_attrib, 2_8)
     IF (h5_err < 0) THEN
        PRINT "('[proc ', I3, ']: Error writing step attribute of type H5_INT64_T')", myproc
        write_step_attribs = h5_err
        RETURN
     ENDIF

     PRINT "('[proc ', I3, ']: Writing step attribute of type H5_INT32_T ...')", myproc
     h5_err = h5_writestepattrib_i4 (file, "i4_attrib", i4_attrib, 2_8)
     IF (h5_err < 0) THEN
        PRINT "('[proc ', I3, ']: Error writing step attribute of type H5_INT32_T')", myproc
        write_step_attribs = h5_err
        RETURN
     ENDIF

     PRINT "('[proc ', I3, ']: Writing step attribute of type H5_STRING_T ...')", myproc
     h5_err = h5_writestepattrib_string (file, "string_attrib", string_attrib)
     IF (h5_err < 0) THEN
        PRINT "('[proc ', I3, ']: Error writing step attribute of type H5_STRING_T')", myproc
        write_step_attribs = h5_err
        RETURN
     ENDIF

     write_step_attribs = 0
   END FUNCTION write_step_attribs

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !!
   INTEGER*8 FUNCTION write_data (file, myproc, num_procs)
     IMPLICIT   NONE

     INTEGER*8, INTENT(IN) :: file
     INTEGER, INTENT(IN) :: myproc
     INTEGER, INTENT(IN) :: num_procs

     INCLUDE 'H5hutF.h'

     INTEGER*8 :: h5_err = 0

     REAL*8,    ALLOCATABLE :: r8_data (:)
     REAL*4,    ALLOCATABLE :: r4_data (:)
     INTEGER*8, ALLOCATABLE :: i8_data (:)
     INTEGER*4, ALLOCATABLE :: i4_data (:)

     INTEGER*8 :: num_points = 1024
     INTEGER*8 :: offset = 0
     INTEGER*8 :: step = 1

     ALLOCATE (r8_data (num_points))
     ALLOCATE (r4_data (num_points))
     ALLOCATE (i8_data (num_points))
     ALLOCATE (i4_data (num_points))

     step = h5_getstep (file)

     offset = num_points * myproc
     DO i = 1, num_points
        r8_data (i) = REAL (i + offset + step)
        r4_data (i) = REAL (i + offset + step)
        i8_data (i) = i + offset + step
        i4_data (i) = i + offset + step
     ENDDO

     h5_err = h5pt_setnpoints (file, num_points)

     PRINT "('[proc ', I3, ']: Writing file data of type H5_FLOAT64_T ...')", myproc
     h5_err = h5pt_writedata_r8 (file,"r8_data", r8_data)
     IF (h5_err < 0) THEN
        PRINT "('[proc ', I3, ']: Error writing r8 data')", myproc
        write_data = h5_err
        RETURN
     ENDIF

     PRINT "('[proc ', I3, ']: Writing file data of type H5_FLOAT32_T ...')", myproc
     h5_err = h5pt_writedata_r4 (file,"r4_data", r4_data)
     IF (h5_err < 0) THEN
        PRINT "('[proc ', I3, ']: Error writing r4 data')", myproc
        write_data = h5_err
        RETURN
     ENDIF
     
     PRINT "('[proc ', I3, ']: Writing file data of type H5_INT64_T ...')", myproc
     h5_err = h5pt_writedata_i8 (file,"i8_data", i8_data)
     IF (h5_err < 0) THEN
        PRINT "('[proc ', I3, ']: Error writing i8 data')", myproc
        write_data = h5_err
        RETURN
     ENDIF
     
     PRINT "('[proc ', I3, ']: Writing file data of type H5_INT32_T ...')", myproc
     h5_err = h5pt_writedata_i4 (file,"i4_data", i4_data)
     IF (h5_err < 0) THEN
        PRINT "('[proc ', I3, ']: Error writing i4 data')", myproc
        write_data = h5_err
        RETURN
     ENDIF

     write_data = 0
   END FUNCTION write_data

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !!
   INTEGER*8 FUNCTION read_file (file, myproc, num_procs)
     IMPLICIT   NONE

     INTEGER*8, INTENT(IN) :: file
     INTEGER, INTENT(IN) :: myproc
     INTEGER, INTENT(IN) :: num_procs

     INCLUDE 'H5hutF.h'

     INTEGER   :: mpi_err
     INTEGER*8 :: step = 1
     INTEGER*8 :: num_steps = 0

     h5_err = read_file_attribs (file, myproc, num_procs)
     IF (h5_err < 0) THEN
        PRINT "('[proc ', I3, ']: Error reading file attributes ...')", myproc
        read_file = h5_err
        RETURN
     ENDIF

     num_steps = h5_getnsteps (file)
     PRINT "('[proc ', I3, ']: Number of steps = ', I3)", myproc, num_steps
     DO step = 1, num_steps
        PRINT "('[proc ', I3, ']: Set step = ', I3)", myproc, step
        h5_err = h5_setstep (file, step)
        IF (h5_err < 0) THEN
           PRINT "('[proc ', I3, ']: Error setting step to ', I2)", myproc, step
           read_file = h5_err
           RETURN
        ENDIF

        h5_err = read_step_attribs (file, myproc, num_procs)
        IF (h5_err < 0) THEN
           PRINT "('[proc ', I3, ']: Error reading step data ...')", myproc
           read_file = h5_err
           RETURN
        ENDIF

        h5_err = read_data (file, myproc, num_procs)
        IF (h5_err < 0) THEN
           PRINT "('[proc ', I3, ']: Error reading step attributes ...')", myproc
           read_file = h5_err
           RETURN
        ENDIF

     ENDDO

     read_file = 0
911  CONTINUE
   END FUNCTION read_file

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !!
   INTEGER*8 FUNCTION cmp_i4 (x, y, size)
     IMPLICIT   NONE
     INTEGER*4, DIMENSION (1:size), INTENT (IN) :: x, y
     INTEGER*8, INTENT (IN) :: size
     INTEGER*8  i
     DO i = 1, size
        IF (x (i) /= y (i)) THEN
           PRINT "('[proc ', I3, ']: Value error ', I6, ' /= ', I6)", &
                myproc, x (i), y (i)
           cmp_i4 = -2
           RETURN
        END IF
     END DO
     cmp_i4 = 0
   END FUNCTION cmp_i4

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !!
   INTEGER*8 FUNCTION cmp_i8 (x, y, size)
     IMPLICIT   NONE
     INTEGER*8, DIMENSION (1:size), INTENT (IN) :: x, y
     INTEGER*8, INTENT (IN) :: size
     INTEGER*8  i
     DO i = 1, size
        IF (x (i) /= y (i)) THEN
           PRINT "('[proc ', I3, ']: Value error ', I6, ' /= ', I6)", &
                myproc, x (i), y (i)
           cmp_i8 = -2
           RETURN
        END IF
     END DO
     cmp_i8 = 0
   END FUNCTION cmp_i8


   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !!
   INTEGER*8 FUNCTION cmp_r4 (x, y, size)
     IMPLICIT   NONE
     REAL*4, DIMENSION (1:size), INTENT (IN) :: x, y
     INTEGER*8, INTENT (IN) :: size
     INTEGER*8  i
     DO i = 1, size
        IF (x (i) /= y (i)) THEN
           PRINT "('[proc ', I3, ']: Value error ', F6.2, ' /= ', F6.2)", &
                myproc, x (i), y (i)
           cmp_r4 = -2
           RETURN
        END IF
     END DO
     cmp_r4 = 0
   END FUNCTION cmp_r4

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !!
   INTEGER*8 FUNCTION cmp_r8 (x, y, size)
     IMPLICIT   NONE
     REAL*8, DIMENSION (1:size), INTENT (IN) :: x, y
     INTEGER*8, INTENT (IN) :: size
     INTEGER*8  i
     DO i = 1, size
        IF (x (i) /= y (i)) THEN
           PRINT "('[proc ', I3, ']: Value error ', F6.2, ' /= ', F6.2)", &
                myproc, x (i), y (i)
           cmp_r8 = -2
           RETURN
        END IF
     END DO
     cmp_r8 = 0
   END FUNCTION cmp_r8

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !!
   INTEGER*8 FUNCTION cmp_string (x, y, size)
     IMPLICIT   NONE
     CHARACTER (LEN=size), INTENT (IN) :: x, y
     INTEGER*8, INTENT (IN) :: size
     INTEGER*8  i
     DO i = 1, size
        IF (x /= y) THEN
           PRINT "('[proc ', I3, ']: Value error ', A, ' /= ', A)", &
                myproc, x, y
           cmp_string = -2
           RETURN
        END IF
     END DO
     cmp_string = 0
   END FUNCTION cmp_string

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !!
   INTEGER*8 FUNCTION read_file_attribs (file, myproc, num_procs)
     IMPLICIT   NONE

     INTEGER*8, INTENT(IN) :: file
     INTEGER, INTENT(IN) :: myproc
     INTEGER, INTENT(IN) :: num_procs

     INCLUDE 'H5hutF.h'

     INTEGER*8 :: num_file_attribs
     INTEGER*8 :: idx
     CHARACTER(LEN=128) :: name
     INTEGER*8 :: type
     INTEGER*8 :: num_elems
     INTEGER*8 :: h5_err

     INTEGER*4, ALLOCATABLE :: i4 (:)
     INTEGER*8, ALLOCATABLE :: i8 (:)
     REAL*4,    ALLOCATABLE :: r4 (:)
     REAL*8,    ALLOCATABLE :: r8 (:)
     CHARACTER (LEN=H5_MAX_NAME_LEN) :: str

     num_file_attribs = h5_getnfileattribs (file)
     IF (num_file_attribs == H5_FAILURE) THEN
        PRINT "('[proc ', I3, ']: Cannot read number of file attributes ...')", myproc
        read_file_attribs = h5_err
        RETURN
     END IF
     PRINT "('[proc ', I3, ']: Number of file attributes: ', I3)", myproc, num_file_attribs

     DO idx = 1, num_file_attribs
        h5_err = h5_getfileattribinfo (file, idx, name, type, num_elems)
        IF (h5_err < 0) THEN
           PRINT "('[proc ', I3, ']: Cannot get info about step attribute ', I2)", myproc, idx
           RETURN
        END IF
        SELECT CASE (type)
        CASE (H5_INT32_T)
           PRINT "('[proc ', I3, ']: Reading file attribute of type H5_INT32_T ...')", myproc
           ALLOCATE (i4 (num_elems))
           h5_err = h5_readfileattrib_i4 (file, name, i4)
           IF (h5_err < 0) THEN
              PRINT "('[proc ', I3, ']: Error reading attribute')", myproc
              read_file_attribs = h5_err
              RETURN
            END IF
           h5_err = cmp_i4 (i4, i4_attrib, num_elems)
           IF (h5_err < 0) THEN
              read_file_attribs = h5_err
              RETURN
            END IF
           
        CASE (H5_INT64_T)
           PRINT "('[proc ', I3, ']: Reading file attribute of type H5_INT64_T ...')", myproc
           ALLOCATE (i8 (num_elems))
           h5_err = h5_readfileattrib_i8 (file, name, i8)
           IF (h5_err < 0) THEN
              PRINT "('[proc ', I3, ']: Error reading attribute')", myproc
              read_file_attribs = h5_err
              RETURN
            END IF
           h5_err = cmp_i8 (i8, i8_attrib, num_elems)
           IF (h5_err < 0) THEN
              read_file_attribs = h5_err
              RETURN
            END IF

        CASE (H5_FLOAT32_T)
           PRINT "('[proc ', I3, ']: Reading file attribute of type H5_FLOAT32_T ...')", myproc
           ALLOCATE (r4 (num_elems))
           h5_err = h5_readfileattrib_r4 (file, name, r4)
           IF (h5_err < 0) THEN
              PRINT "('[proc ', I3, ']: Error reading attribute')", myproc
              read_file_attribs = h5_err
              RETURN
            END IF
           h5_err = cmp_r4 (r4, r4_attrib, num_elems)
           IF (h5_err < 0) THEN
              read_file_attribs = h5_err
              RETURN
            END IF

        CASE (H5_FLOAT64_T)
           PRINT "('[proc ', I3, ']: Reading file attribute of type H5_FLOAT64_T ...')", myproc
           ALLOCATE (r8 (num_elems))
           h5_err = h5_readfileattrib_r8 (file, name, r8)
           IF (h5_err < 0) THEN
              PRINT "('[proc ', I3, ']: Error reading attribute')", myproc
              read_file_attribs = h5_err
              RETURN
            END IF
           h5_err = cmp_r8 (r8, r8_attrib, num_elems)
           IF (h5_err < 0) THEN
              read_file_attribs = h5_err
              RETURN
           END IF

        CASE (H5_STRING_T)
           PRINT "('[proc ', I3, ']: Reading file attribute of type H5_STRING_T ...')", myproc
           h5_err = h5_readfileattrib_string (file, name, str)
           IF (h5_err < 0) THEN
              PRINT "('[proc ', I3, ']: Error reading attribute')", myproc
              read_file_attribs = h5_err
              RETURN
           END IF
           h5_err = cmp_string (str, string_attrib, H5_MAX_NAME_LEN)
           IF (h5_err < 0) THEN
              read_file_attribs = h5_err
              RETURN
           END IF
        END SELECT
     END DO
     read_file_attribs = 0
   END FUNCTION read_file_attribs

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !!
   INTEGER*8 FUNCTION read_step_attribs (file, myproc, num_procs)
     IMPLICIT   NONE

     INTEGER*8, INTENT(IN) :: file
     INTEGER, INTENT(IN) :: myproc
     INTEGER, INTENT(IN) :: num_procs

     INCLUDE 'H5hutF.h'


     INTEGER*8 :: num_step_attribs
     INTEGER*8 :: idx
     CHARACTER(LEN=128) :: name
     INTEGER*8 :: type
     INTEGER*8 :: num_elems
     INTEGER*8 :: h5_err

     INTEGER*4, ALLOCATABLE :: i4 (:)
     INTEGER*8, ALLOCATABLE :: i8 (:)
     REAL*4,    ALLOCATABLE :: r4 (:)
     REAL*8,    ALLOCATABLE :: r8 (:)
     CHARACTER (LEN=H5_MAX_NAME_LEN) :: str

     num_step_attribs = h5_getnstepattribs (file)
     IF (num_step_attribs == H5_FAILURE) THEN
        PRINT "('[proc ', I3, ']: Cannot read number of step attributes ...')", myproc
        read_step_attribs = h5_err
     END IF
     PRINT "('[proc ', I3, ']: Number of step attributes: ', I3)", myproc, num_step_attribs

     DO idx = 1, num_step_attribs
        h5_err = h5_getstepattribinfo (file, idx, name, type, num_elems)
        IF (h5_err < 0) THEN
           PRINT "('[proc ', I3, ']: Cannot get info about step attribute ', I2)", myproc, idx
           RETURN
        END IF
        SELECT CASE (type)
        CASE (H5_INT32_T)
           PRINT "('[proc ', I3, ']: Reading step attribute of type H5_INT32_T ...')", myproc
           ALLOCATE (i4 (num_elems))
           h5_err = h5_readstepattrib_i4 (file, name, i4)
           IF (h5_err < 0) THEN
              PRINT "('[proc ', I3, ']: Error reading attribute')", myproc
              read_step_attribs = h5_err
              RETURN
           END IF
           h5_err = cmp_i4 (i4, i4_attrib, num_elems)
           IF (h5_err < 0) THEN
              read_step_attribs = h5_err
              RETURN
           END IF
           
        CASE (H5_INT64_T)
           PRINT "('[proc ', I3, ']: Reading step attribute of type H5_INT64_T ...')", myproc
           ALLOCATE (i8 (num_elems))
           h5_err = h5_readstepattrib_i8 (file, name, i8)
           IF (h5_err < 0) THEN
              PRINT "('[proc ', I3, ']: Error reading attribute')", myproc
              read_step_attribs = h5_err
              RETURN
           END IF
           h5_err = cmp_i8 (i8, i8_attrib, num_elems)
           IF (h5_err < 0) THEN
              read_step_attribs = h5_err
              RETURN
           END IF

        CASE (H5_FLOAT32_T)
           PRINT "('[proc ', I3, ']: Reading step attribute of type H5_FLOAT32_T ...')", myproc
           ALLOCATE (r4 (num_elems))
           h5_err = h5_readstepattrib_r4 (file, name, r4)
           IF (h5_err < 0) THEN
              PRINT "('[proc ', I3, ']: Error reading attribute')", myproc
              read_step_attribs = h5_err
              RETURN
           END IF
           h5_err = cmp_r4 (r4, r4_attrib, num_elems)
           IF (h5_err < 0) THEN
              read_step_attribs = h5_err
              RETURN
           END IF

        CASE (H5_FLOAT64_T)
           PRINT "('[proc ', I3, ']: Reading step attribute of type H5_FLOAT64_T ...')", myproc
           ALLOCATE (r8 (num_elems))
           h5_err = h5_readstepattrib_r8 (file, name, r8)
           IF (h5_err < 0) THEN
              PRINT "('[proc ', I3, ']: Error reading attribute')", myproc
              read_step_attribs = h5_err
              RETURN
           END IF
           h5_err = cmp_r8 (r8, r8_attrib, num_elems)
           IF (h5_err < 0) THEN
              read_step_attribs = h5_err
              RETURN
           END IF

        CASE (H5_STRING_T)
           PRINT "('[proc ', I3, ']: Reading step attribute of type H5_STRING_T ...')", myproc
           h5_err = h5_readstepattrib_string (file, name, str)
           IF (h5_err < 0) THEN
              PRINT "('[proc ', I3, ']: Error reading attribute')", myproc
              read_step_attribs = h5_err
              RETURN
           END IF
           h5_err = cmp_string (str, string_attrib, H5_MAX_NAME_LEN)
           IF (h5_err < 0) THEN
              read_step_attribs = h5_err
              RETURN
           END IF

        END SELECT
     END DO

     read_step_attribs = 0
   END FUNCTION read_step_attribs

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !!
   INTEGER*8 FUNCTION read_data (file, myproc, num_procs)
     IMPLICIT   NONE

     INTEGER*8, INTENT(IN) :: file
     INTEGER, INTENT(IN) :: myproc
     INTEGER, INTENT(IN) :: num_procs

     INCLUDE 'H5hutF.h'

     INTEGER*8 :: h5_err = 0
     INTEGER*8 :: num_points = 0
     INTEGER*8 :: num = 0
     INTEGER*8 :: num_points_total = 0
     INTEGER*8 :: num_datasets = 0
     INTEGER*8 :: offset = 0
     INTEGER*8 :: idx = 0
     INTEGER*8 :: step = 0

     CHARACTER(LEN=128) :: name
     INTEGER*8 :: type

     REAL*8,    ALLOCATABLE :: r8_data (:)
     REAL*4,    ALLOCATABLE :: r4_data (:)
     INTEGER*8, ALLOCATABLE :: i8_data (:)
     INTEGER*4, ALLOCATABLE :: i4_data (:)
     REAL*8,    ALLOCATABLE :: r8 (:)
     REAL*4,    ALLOCATABLE :: r4 (:)
     INTEGER*8, ALLOCATABLE :: i8 (:)
     INTEGER*4, ALLOCATABLE :: i4 (:)

     num_datasets = h5pt_getndatasets (file)
     IF (num_datasets < 0) THEN
        PRINT "('[proc ', I3, ']: Cannot read number of datasets ...')", myproc
        read_data = h5_err
        RETURN
     END IF
     PRINT "('[proc ', I3, ']: Number of datasets: ', I3)", myproc, num_datasets

     h5_err = h5pt_resetview (file)
     IF (h5_err < 0) THEN
        PRINT "('[proc ', I3, ']: Cannot reset view ...')", myproc
        read_data = h5_err
        RETURN
     END IF

     num_points_total = h5pt_getnpoints (file)
     IF (num_points_total < 0) THEN
        PRINT "('[proc ', I3, ']: Cannot read total number of points ...')", myproc
        read_data = h5_err
        RETURN
     END IF
     PRINT "('[proc ', I3, ']: Total number of points: ', I6)", myproc, num_points_total

     ! select subset
     num_points = num_points_total / num_procs
     offset = num_points * myproc + 1;
     h5_err = h5pt_setview (file, offset, offset + num_points - 1)
     IF (h5_err < 0) THEN
        PRINT "('[proc ', I3, ']: Cannot set view ...')", myproc
        read_data = h5_err
     END IF
     PRINT "('[proc ', I3, ']: View: ', I4, ':', I4)", myproc, offset, offset+num_points-1

     ALLOCATE (r8_data (num_points))
     ALLOCATE (r4_data (num_points))
     ALLOCATE (i8_data (num_points))
     ALLOCATE (i4_data (num_points))
     ALLOCATE (r8 (num_points))
     ALLOCATE (r4 (num_points))
     ALLOCATE (i8 (num_points))
     ALLOCATE (i4 (num_points))

     step = h5_getstep (file)

     offset = num_points * myproc
     DO i = 1, num_points
        r8_data (i) = REAL (i + offset + step)
        r4_data (i) = REAL (i + offset + step)
        i8_data (i) = i + offset + step
        i4_data (i) = i + offset + step
     ENDDO

     DO idx = 1, num_datasets
        h5_err = h5pt_getdatasetinfo (file, idx, name, type, num)
        IF (h5_err < 0) THEN
           PRINT "('[proc ', I3, ']: Cannot get info about datset ', I2)", myproc, idx
        END IF
        SELECT CASE (type)
        CASE (H5_INT32_T)
           PRINT "('[proc ', I3, ']: Reading dataset of type H5_INT32_T ...')", myproc
           h5_err = h5pt_readdata_i4 (file, name, i4)
           IF (h5_err < 0) THEN
              PRINT "('[proc ', I3, ']: Error reading datset')", myproc
              read_data = h5_err
              RETURN
           END IF
           h5_err = cmp_i4 (i4, i4_data, num_points)
           IF (h5_err < 0) THEN
              read_data = h5_err
              RETURN
           END IF

        CASE (H5_INT64_T)
           PRINT "('[proc ', I3, ']: Reading dataset of type H5_INT64_T ...')", myproc
           h5_err = h5pt_readdata_i8 (file, name, i8)
           IF (h5_err < 0) THEN
              PRINT "('[proc ', I3, ']: Error reading datset')", myproc
              read_data = h5_err
              RETURN
           END IF
           h5_err = cmp_i8 (i8, i8_data, num_points)
           IF (h5_err < 0) THEN
              read_data = h5_err
              RETURN
           END IF

        CASE (H5_FLOAT32_T)
           PRINT "('[proc ', I3, ']: Reading dataset of type H5_FLOAT32_T ...')", myproc
           h5_err = h5pt_readdata_r4 (file, name, r4)
           IF (h5_err < 0) THEN
              PRINT "('[proc ', I3, ']: Error reading datset')", myproc
              read_data = h5_err
              RETURN
           END IF
           h5_err = cmp_r4 (r4, r4_data, num_points)
           IF (h5_err < 0) THEN
              read_data = h5_err
              RETURN
           END IF

        CASE (H5_FLOAT64_T)
           PRINT "('[proc ', I3, ']: Reading dataset of type H5_FLOAT64_T ...')", myproc
           h5_err = h5pt_readdata_r8 (file, name, r8)
           IF (h5_err < 0) THEN
              PRINT "('[proc ', I3, ']: Error reading datset')", myproc
              read_data = h5_err
              RETURN
           END IF
           h5_err = cmp_r8 (r8, r8_data, num_points)
           IF (h5_err < 0) THEN
              read_data = h5_err
              RETURN
           END IF
           
        END SELECT
     END DO

     read_data = 0
   END FUNCTION read_data
END PROGRAM
