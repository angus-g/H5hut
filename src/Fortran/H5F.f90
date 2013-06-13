! Declaration of subroutines for Fortran Bindings

!!!!!!!! File Opening and Closing !!!!!!!!


!>
!! \ingroup h5hut_file_f
!! Opens a file for reading. See \ref H5OpenFile
!! \return 0 on success or error code
!<
INTEGER*8 FUNCTION h5_openr ( filename )
    CHARACTER(LEN=*), INTENT(IN) :: filename    !< the filename to open for reading
END FUNCTION

!>
!! \ingroup h5hut_file_f
!! Opens a file for writing in truncate mode. See \ref H5OpenFile
!! \return 0 on success or error code
!<
INTEGER*8 FUNCTION h5_openw ( filename )
    CHARACTER(LEN=*), INTENT(IN) :: filename    !< the filename to open for writing
END FUNCTION

!>
!! \ingroup h5hut_file_f
!! Opens a file for writing in append mode. See \ref H5OpenFile
!! \return 0 on success or error code
!<
INTEGER*8 FUNCTION h5_opena ( filename )
    CHARACTER(LEN=*), INTENT(IN) :: filename    !< the filename to open for appending
END FUNCTION

!>
!! \ingroup h5hut_file_f
!! Opens a parallel file for reading. See \ref H5OpenFile
!! \return 0 on success or error code
!<
INTEGER*8 FUNCTION h5_openr_par ( filename, mpi_communicator )
    CHARACTER(LEN=*), INTENT(IN) :: filename    !< the filename to open for reading
    INTEGER, INTENT(IN) :: mpi_communicator     !< the MPI communicator used by the program
END FUNCTION

!>
!! \ingroup h5hut_file_f
!! Opens a parallel file for writing in truncate mode. See \ref H5OpenFile
!! \return 0 on success or error code
!<
INTEGER*8 FUNCTION h5_openw_par ( filename, mpi_communicator )
    CHARACTER(LEN=*), INTENT(IN) :: filename    !< the filename to open for writing
    INTEGER, INTENT(IN) :: mpi_communicator     !< the MPI_Communicator used by the program
END FUNCTION

!>
!! \ingroup h5hut_file_f
!! Opens a parallel file for writing in append mode. See \ref H5OpenFile
!! \return 0 on success or error code
!<
INTEGER*8 FUNCTION h5_opena_par ( filename, mpi_communicator )
    CHARACTER(LEN=*), INTENT(IN) :: filename    !< the filename to open for appending
    INTEGER, INTENT(IN) :: mpi_communicator     !< the MPI_Communicator used by the program
END FUNCTION

!>
!! \ingroup h5hut_file_f
!! Opens a file for reading and specifies an HDF5 alignment.
!! See \ref H5OpenFileAlign
!! \return 0 on success or error code
!<
INTEGER*8 FUNCTION h5_openr_align ( filename, align )
    CHARACTER(LEN=*), INTENT(IN) :: filename    !< the filename to open for reading
    INTEGER*8, INTENT(IN) :: align              !< alignment value in bytes
END FUNCTION

!>
!! \ingroup h5hut_file_f
!! Opens a file for writing in truncate mode and specifies an HDF5 alignment.
!! See \ref H5OpenFileAlign
!! \return 0 on success or error code
!<
INTEGER*8 FUNCTION h5_openw_align ( filename, align )
    CHARACTER(LEN=*), INTENT(IN) :: filename    !< the filename to open for writing
    INTEGER*8, INTENT(IN) :: align              !< alignment value in bytes
END FUNCTION
 
!>
!! \ingroup h5hut_file_f
!! Opens a file for writing in append mode and specifies an HDF5 alignment.
!! See \ref H5OpenFileAlign
!! \return 0 on success or error code
!<
INTEGER*8 FUNCTION h5_opena_align ( filename, align )
    CHARACTER(LEN=*), INTENT(IN) :: filename    !< the filename to open for appending
    INTEGER*8, INTENT(IN) :: align              !< alignment value in bytes
END FUNCTION

!>
!! \ingroup h5hut_file_f
!! Opens a parallel file for reading and specifies an HDF5 alignment.
!! See \ref H5OpenFileAlign
!!
!! Flags are specified as a comma separated string that can include:
!!
!! - \c fs_lustre - enable optimizations for the Lustre file system
!! - \c vfd_mpiposix - use the HDF5 MPI-POSIX virtual file driver
!! - \c vfd_mpio_ind - use MPI-IO in indepedent mode
!!
!! See \ref H5OpenFileAlign
!! \return 0 on success or error code
!<
INTEGER*8 FUNCTION h5_openr_par_align ( filename, mpi_communicator, align, flags )
    CHARACTER(LEN=*), INTENT(IN) :: filename    !< the filename to open for reading
    INTEGER, INTENT(IN) :: mpi_communicator     !< the MPI_Communicator used by the program
    INTEGER*8, INTENT(IN) :: align              !< alignment value in bytes
    CHARACTER(LEN=*), INTENT(IN) :: flags       !< additional flags
END FUNCTION

!>
!! \ingroup h5hut_file_f
!! Opens a parallel file for writing in truncate mode and specifies
!! an HDF5 alignment.
!!
!! Flags are specified as a comma separated string that can include:
!!
!! - \c fs_lustre - enable optimizations for the Lustre file system
!! - \c vfd_mpiposix - use the HDF5 MPI-POSIX virtual file driver
!! - \c vfd_mpio_ind - use MPI-IO in indepedent mode
!!
!! See \ref H5OpenFileAlign
!! \return 0 on success or error code
!<
INTEGER*8 FUNCTION h5_openw_par_align ( filename, mpi_communicator, align, flags )
    CHARACTER(LEN=*), INTENT(IN) :: filename    !< the filename to open for writing
    INTEGER, INTENT(IN) :: mpi_communicator     !< the MPI_Communicator used by the program
    INTEGER*8, INTENT(IN) :: align              !< alignment value in bytes
    CHARACTER(LEN=*), INTENT(IN) :: flags       !< additional flags
END FUNCTION

!>
!! \ingroup h5hut_file_f
!! Opens a parallel file for writing in append mode and specifies
!! an HDF5 alignment.
!!
!! Flags are specified as a comma separated string that can include:
!!
!! - \c fs_lustre - enable optimizations for the Lustre file system
!! - \c vfd_mpiposix - use the HDF5 MPI-POSIX virtual file driver
!! - \c vfd_mpio_ind - use MPI-IO in indepedent mode
!!
!! See \ref H5OpenFileAlign
!! \return 0 on success or error code
!<
INTEGER*8 FUNCTION h5_opena_par_align ( filename, mpi_communicator, align, flags )
    CHARACTER(LEN=*), INTENT(IN) :: filename    !< the filename to open for appending
    INTEGER, INTENT(IN) :: mpi_communicator     !< the MPI_Communicator used by the program
    INTEGER*8, INTENT(IN) :: align              !< alignment value in bytes
    CHARACTER(LEN=*), INTENT(IN) :: flags       !< additional flags
END FUNCTION

!>
!! \ingroup h5hut_file_f
!! Closes a file. See \ref H5CloseFile
!! \return 0 on success or error code
INTEGER*8 FUNCTION h5_close ( filehandle )
    INTEGER*8, INTENT(IN) :: filehandle !< the handle returned during file open
END FUNCTION

!>
!! \ingroup h5hut_file_f
!! Close HDF5 library. See \ref H5Finalize
!! \return \c H5_SUCCESS or \c H5_FAILURE
INTEGER*8 FUNCTION h5_finalize ()
END FUNCTION

!>
!! \ingroup h5hut_file_f
!! Checks that a file is valid. See \ref H5CheckFile
!! \return 0 on success or error code
INTEGER*8 FUNCTION h5_check ( filehandle )
    INTEGER*8, INTENT(IN) :: filehandle !< the handle returned during file open
END FUNCTION


!>
!! \ingroup h5hut_model_f
!! See \ref H5SetStep
!! \return 0 on success or error code
INTEGER*8 FUNCTION h5_setstep (filehandle,step)
    INTEGER*8, INTENT(IN) :: filehandle !< the handle returned during file open
    INTEGER*8, INTENT(IN) :: step       !< a timestep value >= 1
END FUNCTION

!>
!! \ingroup h5hut_model_f
!! See \ref H5GetStep
!! \return the the current step or \c H5_FAILURE
INTEGER*8 FUNCTION h5_getstep (filehandle)      
    INTEGER*8, INTENT(IN) :: filehandle         !< the handle returned during file open
END FUNCTION

!>
!! \ingroup h5hut_model_f
!! See \ref H5GetNumSteps
!! \return the number of steps or error code
INTEGER*8 FUNCTION h5_getnsteps (filehandle)      
    INTEGER*8, INTENT(IN) :: filehandle         !< the handle returned during file open
END FUNCTION

!>
!! \ingroup h5hut_error_f
!! See \ref H5SetVerbosityLevel
!! \return 0 on success or error code
INTEGER*8 FUNCTION h5_set_verbosity_level ( level )
    INTEGER*8, INTENT(IN) :: level      !< the level from 0 (no output) to 5 (most detailed)
END FUNCTION

