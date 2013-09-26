!
!  Copyright (c) 2006-2013, The Regents of the University of California,
!  through Lawrence Berkeley National Laboratory (subject to receipt of any
!  required approvals from the U.S. Dept. of Energy) and the Paul Scherrer
!  Institut (Switzerland).  All rights reserved.!
!
!  License: see file COPYING in top level of source distribution.
!
include 'H5hut.f90'

program openclose

  use H5hut

  implicit none

  include 'mpif.h'
  
  integer :: comm, rank, ierr
  integer*8 :: file_id, status
  integer*8 :: props

  comm = MPI_COMM_WORLD
  call mpi_init(ierr)
  call mpi_comm_rank(comm, rank, ierr)

  props = h5_createprop_file ()
  status = h5_setprop_filempio (props, comm)
  file_id = h5_openfile ("testfile.h5", H5_O_WRONLY, props)
  status = h5_closeprop (props)
  status = h5_closefile (file_id);

  call mpi_finalize(ierr)

end program openclose
