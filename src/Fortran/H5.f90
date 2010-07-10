! Declaration of subroutines for Fortran Bindings

!> \defgroup h5_f90_api H5hut F90 API

!> \ingroup h5_f90_api
!! \defgroup h5_open_f       File Handling
!<

!> \ingroup h5_f90_api
!! \defgroup h5_model_f      Setting up the Data Model
!<

!> \ingroup h5_f90_api
!! \defgroup h5_data_f       Reading and Writing Datasets
!<

!> \ingroup h5_f90_api
!! \defgroup h5_attrib_f     Reading and Writing Attributes
!<


!!!!!!!! File Opening and Closing !!!!!!!!

!> \ingroup h5_open_f
!! Opens a file for reading. See \ref H5OpenFile
!! \return 0 on success or error code
!<
INTEGER*8 FUNCTION h5_openr ( filename )
    CHARACTER(LEN=*), INTENT(IN) :: filename    !< the filename to open for reading
END FUNCTION

!> \ingroup h5_open_f
!! Opens a file for writing in truncate mode. See \ref H5OpenFile
!! \return 0 on success or error code
!<
INTEGER*8 FUNCTION h5_openw ( filename )
    CHARACTER(LEN=*), INTENT(IN) :: filename    !< the filename to open for writing
END FUNCTION

!> \ingroup h5_open_f
!! Opens a file for writing in append mode. See \ref H5OpenFile
!! \return 0 on success or error code
!<
INTEGER*8 FUNCTION h5_opena ( filename )
    CHARACTER(LEN=*), INTENT(IN) :: filename    !< the filename to open for appending
END FUNCTION

!> \ingroup h5_open_f
!! Opens a parallel file for reading.
!! See \ref H5OpenFile
!!
!! Flags are specified as a comma separated string that can include:
!!
!! - \c vfd_mpiposix - use the HDF5 MPI-POSIX virtual file driver
!! - \c vfd_indendent - use MPI-IO in indepedent mode
!!
!! \return 0 on success or error code
!<
INTEGER*8 FUNCTION h5_openr_par_align ( filename, mpi_communicator, align )
    CHARACTER(LEN=*), INTENT(IN) :: filename    !< the filename to open for reading
    INTEGER, INTENT(IN) :: mpi_communicator     !< the MPI_Communicator used by the program
    CHARACTER(LEN=*), INTENT(IN) :: flags       !< additional flags
END FUNCTION

!> \ingroup h5_open_f
!! Opens a parallel file for writing.
!! See \ref H5OpenFile
!!
!! Flags are specified as a comma separated string that can include:
!!
!! - \c vfd_mpiposix - use the HDF5 MPI-POSIX virtual file driver
!! - \c vfd_indendent - use MPI-IO in indepedent mode
!! - \c vfd_mpio_ind - use MPI-IO in indepedent mode
!!
!! See \ref H5PartOpenFileParallelAlign
!! \return 0 on success or error code
!<
INTEGER*8 FUNCTION h5_openw_par_align ( filename, mpi_communicator, align, flags )
    CHARACTER(LEN=*), INTENT(IN) :: filename    !< the filename to open for writing
    INTEGER, INTENT(IN) :: mpi_communicator     !< the MPI_Communicator used by the program
    CHARACTER(LEN=*), INTENT(IN) :: flags       !< additional flags
END FUNCTION

!> \ingroup h5_open_f
!! Opens a parallel file for writing in append mode.
!! See \ref H5OpenFile
!!
!! Flags are specified as a comma separated string that can include:
!!
!! - \c vfd_mpiposix - use the HDF5 MPI-POSIX virtual file driver
!! - \c vfd_indendent - use MPI-IO in indepedent mode
!!
!! See \ref H5PartOpenFileParallelAlign
!! \return 0 on success or error code
!<
INTEGER*8 FUNCTION h5_opena_par_align ( filename, mpi_communicator, align, flags )
    CHARACTER(LEN=*), INTENT(IN) :: filename    !< the filename to open for appending
    INTEGER, INTENT(IN) :: mpi_communicator     !< the MPI_Communicator used by the program
    CHARACTER(LEN=*), INTENT(IN) :: flags       !< additional flags
END FUNCTION

!> \ingroup h5_open_f
!! Closes a file. See \ref H5CloseFile
!! \return 0 on success or error code
!<
INTEGER*8 FUNCTION h5_close ( filehandle )
    INTEGER*8, INTENT(IN) :: filehandle !< the handle returned during file open
END FUNCTION

!> \ingroup h5_open_f
!! Checks that a file is valid. See \ref H5CheckFile
!! \return 0 on success or error code
!<
INTEGER*8 FUNCTION h5_check ( filehandle )
    INTEGER*8, INTENT(IN) :: filehandle !< the handle returned during file open
END FUNCTION

!> \ingroup h5_open_f
!! See \ref H5SetVerbosityLevel
!! \return 0 on success or error code
!<
INTEGER*8 FUNCTION h5_set_verbosity_level ( level )
    INTEGER*8, INTENT(IN) :: level      !< the level from 0 (no output) to 5 (most detailed)
END FUNCTION

!> \ingroup h5_model_f
!! See \ref H5SetStep
!! \return 0 on success or error code
!<
INTEGER*8 FUNCTION h5_setstep (filehandle,step)
    INTEGER*8, INTENT(IN) :: filehandle !< the handle returned during file open
    INTEGER*8, INTENT(IN) :: step       !< a timestep value >= 1
END FUNCTION

!> \ingroup h5_model_f
!! See \ref H5GetNumSteps
!! \return the number of steps or error code
!<
INTEGER*8 FUNCTION h5_getnsteps (filehandle)      
    INTEGER*8, INTENT(IN) :: filehandle         !< the handle returned during file open
END FUNCTION


