!
!  Copyright (c) 2006-2015, The Regents of the University of California,
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
  character (len=*), parameter :: FNAME =       "example_setview.h5"

  integer :: comm, rank, ierr
  integer*8 :: file, status, num_particles

  ! init MPI & H5hut
  comm = MPI_COMM_WORLD
  call mpi_init(ierr)
  call mpi_comm_rank(comm, rank, ierr)
  call h5_abort_on_error ()

  ! open the a file for parallel writing and ceate step #0
  file = h5_openfile (FNAME, H5_O_RDONLY, H5_PROP_DEFAULT)
  status = h5_setstep(file, 1_8)

  status = h5pt_setcanonicalview (file)
  num_particles = h5pt_getnpoints (file)
  write (*, "('[proc ', i4, '] particles in view: ', i8)") rank, num_particles

  ! cleanup
  status = h5_closefile (file)
  call mpi_finalize (ierr)

end program read_canonicalview
