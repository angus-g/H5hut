!
!  Copyright (c) 2006-2013, The Regents of the University of California,
!  through Lawrence Berkeley National Laboratory (subject to receipt of any
!  required approvals from the U.S. Dept. of Energy) and the Paul Scherrer
!  Institut (Switzerland).  All rights reserved.!
!
!  License: see file COPYING in top level of source distribution.
!
include 'H5hut.f90'

program write_core_vfd
  use H5hut
  implicit none
  include 'mpif.h'

  ! the file name we want to read
  character (len=*), parameter :: FNAME =       "example_core_vfd"
  integer*8, parameter :: DIM =                 99

  integer :: comm, rank, ierr
  integer*8 :: file, status
  integer*4, allocatable :: data(:)

  ! init MPI & H5hut
  comm = MPI_COMM_WORLD
  call mpi_init(ierr)
  call mpi_comm_rank(comm, rank, ierr)
  call h5_abort_on_error ()


  ! cleanup
  status = h5_closefile (file)
  deallocate (data)
  call mpi_finalize (ierr)

end program write_core_vfd
