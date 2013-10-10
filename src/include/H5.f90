!
!  Copyright (c) 2006-2013, The Regents of the University of California,
!  through Lawrence Berkeley National Laboratory (subject to receipt of any
!  required approvals from the U.S. Dept. of Energy) and the Paul Scherrer
!  Institut (Switzerland).  All rights reserved.!
!
!  License: see file COPYING in top level of source distribution.
!
  INTERFACE

     !>
     !! \ingroup h5hut_file_f
     !! Create file property. See \ref H5CreateProp
     !! \return 0 on success or -2 on error
     !<
     INTEGER*8 FUNCTION h5_createprop_file ()
     END FUNCTION h5_createprop_file

     !>
     !! \ingroup h5hut_file_f
     !! Store MPI IO comminicator information in file property list.
     !! See \ref H5SetPropFileMPIO
     !! \return 0 on success or -2 on error
     !<
     INTEGER*8 FUNCTION h5_setprop_file_mpio (prop, comm)
       INTEGER*8, INTENT(IN) :: prop               !< property
       INTEGER, INTENT(IN) :: comm                 !< the MPI communicator used by the program
     END FUNCTION h5_setprop_file_mpio

     !>
     !! \ingroup h5hut_file_f
     !! Store MPI IO comminicator information in file property list.
     !! See \ref H5SetPropFileMPIOCollective
     !! \return 0 on success or -2 on error
     !<
     INTEGER*8 FUNCTION h5_setprop_file_mpio_collective (prop, comm)
       INTEGER*8, INTENT(IN) :: prop               !< property
       INTEGER, INTENT(IN) :: comm                 !< the MPI communicator used by the program
     END FUNCTION h5_setprop_file_mpio_collective

     !>
     !! \ingroup h5hut_file_f
     !! Store MPI IO comminicator information in file property list.
     !! See \ref H5SetPropFileMPIOIndependent
     !! \return 0 on success or -2 on error
     !<
     INTEGER*8 FUNCTION h5_setprop_file_mpio_independent (prop, comm)
       INTEGER*8, INTENT(IN) :: prop               !< property
       INTEGER, INTENT(IN) :: comm                 !< the MPI communicator used by the program
     END FUNCTION h5_setprop_file_mpio_independent

     !>
     !! \ingroup h5hut_file_f
     !! Store MPI IO comminicator information in file property list.
     !! See \ref H5SetPropFileMPIOPosix
     !! \return 0 on success or -2 on error
     !<
     INTEGER*8 FUNCTION h5_setprop_file_mpio_posix (prop, comm)
       INTEGER*8, INTENT(IN) :: prop               !< property
       INTEGER, INTENT(IN) :: comm                 !< the MPI communicator used by the program
     END FUNCTION h5_setprop_file_mpio_posix

     !>
     !! \ingroup h5hut_file_f
     !! Store core VFD.
     !! See \ref H5SetPropCoreVFD
     !! \return 0 on success or -2 on error
     !<
     INTEGER*8 FUNCTION h5_setprop_file_core_vfd (prop)
       INTEGER*8, INTENT(IN) :: prop               !< property
     END FUNCTION h5_setprop_file_core_vfd

     !>
     !! \ingroup h5hut_file_f
     !! Set alignment. See \ref H5SetPropFileAlign
     !! \return 0 on success or -2 on error
     !<
     INTEGER*8 FUNCTION h5_setprop_file_align (prop, align)
       INTEGER*8, INTENT(IN) :: prop               !< property
       INTEGER*8, INTENT(IN) :: align              !< alignment
     END FUNCTION h5_setprop_file_align

     !>
     !! \ingroup h5hut_file_f
     !! Set throttle. See \ref H5SetPropFileThrottle
     !! \return 0 on success or -2 on error
     !<
     INTEGER*8 FUNCTION h5_setprop_file_throttle (prop, throttle)
       INTEGER*8, INTENT(IN) :: prop               !< property
       INTEGER*8, INTENT(IN) :: throttle           !< throttle
     END FUNCTION h5_setprop_file_throttle

     !>
     !! \ingroup h5hut_file_f
     !! Close property. See \ref H5CloseProp
     !! \return 0 on success or -2 on error
     !<
     INTEGER*8 FUNCTION h5_closeprop (prop)
       INTEGER*8, INTENT(IN) :: prop               !< property
     END FUNCTION h5_closeprop

     !>
     !! \ingroup h5hut_file_f
     !! Opens a file for reading. See \ref H5OpenFile
     !! \return 0 on success or error code
     !<
     INTEGER*8 FUNCTION h5_openfile (fname, mode, props)
       CHARACTER(LEN=*), INTENT(IN) :: fname       !< the filename to open for reading
       INTEGER*8, INTENT(IN) :: mode               !< file mode
       INTEGER*8, INTENT(IN) :: props              !< properties
     END FUNCTION h5_openfile

     !>
     !! \ingroup h5hut_file_f
     !! Closes a file. See \ref H5CloseFile
     !! \return 0 on success or error code
     INTEGER*8 FUNCTION h5_closefile (filehandle)
       INTEGER*8, INTENT(IN) :: filehandle !< the handle returned during file open
     END FUNCTION h5_closefile

     !>
     !! \ingroup h5hut_file_f
     !! Checks that a file is valid. See \ref H5CheckFile
     !! \return H5_SUCCESS or H5_FAILURE
     !<
     INTEGER*8 FUNCTION h5_checkfile ( filehandle )
       INTEGER*8, INTENT(IN) :: filehandle !< the handle returned during file open
     END FUNCTION h5_checkfile

     !>
     !! \ingroup h5hut_file_f
     !! Flush file data to disk.
     !! \return H5_SUCCESS or H5_FAILURE
     !<
     INTEGER*8 FUNCTION h5_flushfile (filehandle)
       INTEGER*8, INTENT(IN) :: filehandle !< the handle returned during file open
     END FUNCTION h5_flushfile

     !>
     !! \ingroup h5hut_file_f
     !! Flush step data to disk.
     !! \return H5_SUCCESS or H5_FAILURE
     !<
     INTEGER*8 FUNCTION h5_flushstep (filehandle)
       INTEGER*8, INTENT(IN) :: filehandle !< the handle returned during file open
     END FUNCTION h5_flushstep

     !>
     !! \ingroup h5hut_file_f
     !! Close HDF5 library. See \ref H5Finalize
     !! \return \c H5_SUCCESS or \c H5_FAILURE
     !<
     INTEGER*8 FUNCTION h5_finalize ()
     END FUNCTION h5_finalize


     !>
     !! \ingroup h5hut_error_f
     !! See \ref H5SetVerbosityLevel
     !! \return 0 on success or error code
     !<
     SUBROUTINE h5_set_verbosity_level ( level )
       INTEGER*8, INTENT(IN) :: level      !< the level from 0 (no output) to 5 (most detailed)
     END SUBROUTINE h5_set_verbosity_level

     !>
     !! \ingroup h5hut_error_f
     !! Abort program on error.
     !! \return 0 on success or error code
     !<
     SUBROUTINE h5_abort_on_error ()
     END SUBROUTINE h5_abort_on_error

  END INTERFACE
