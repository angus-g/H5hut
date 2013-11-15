!
!  Copyright (c) 2006-2013, The Regents of the University of California,
!  through Lawrence Berkeley National Laboratory (subject to receipt of any
!  required approvals from the U.S. Dept. of Energy) and the Paul Scherrer
!  Institut (Switzerland).  All rights reserved.!
!
!  License: see file COPYING in top level of source distribution.
!
include 'H5hut.f90'

program read_canonicalview
  use H5hut
  implicit none
  include 'mpif.h'

  ! the file name we want to read
  character (len=*), parameter :: FNAME =       "example_setnparticles.h5"
  integer*8, parameter :: NPOINTS =             99

  integer :: comm, rank, ierr
  integer*8 :: file, i, status
  integer*4, allocatable :: data(:)

  ! init MPI & H5hut
  comm = MPI_COMM_WORLD
  call mpi_init(ierr)
  call mpi_comm_rank(comm, rank, ierr)
  call h5_abort_on_error ()

  ! create fake data
  allocate (data (NPOINTS))
  do i = 1, NPOINTS
    data (i) = i + NPOINTS*rank
  enddo

  ! open the a file for parallel writing and ceate step #0
  file = h5_openfile (FNAME, H5_O_WRONLY, H5_PROP_DEFAULT)
  status = h5_setstep(file, 0_8)

  ! set the size of the 1D array
  status = h5pt_setnpoints (file, npoints)

  ! write the particles
  status = h5pt_writedata_i4 (file, "data", data)

  ! cleanup
  status = h5_closefile (file)
  deallocate (data)
  call mpi_finalize (ierr)

end program write_setnparticles
