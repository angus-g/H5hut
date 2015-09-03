!
!  Copyright (c) 2006-2015, The Regents of the University of California,
!  through Lawrence Berkeley National Laboratory (subject to receipt of any
!  required approvals from the U.S. Dept. of Energy) and the Paul Scherrer
!  Institut (Switzerland).  All rights reserved.!
!
!  License: see file COPYING in top level of source distribution.
!
include 'H5hut.f90'

program read_setnparticles
  use H5hut
  implicit none
  include 'mpif.h'

  ! the file name we want to read
  character (len=*), parameter :: FNAME =       "example_setnparticles.h5"

  integer :: comm, comm_rank, comm_size, ierr
  integer*8 :: file, status
  integer*8 :: nparticels, nparticels_total
  integer*4, allocatable :: data(:)

  ! initialize MPI & H5hut
  comm = MPI_COMM_WORLD
  call mpi_init(ierr)
  call mpi_comm_rank (comm, comm_rank, ierr)
  call mpi_comm_size (comm, comm_size, ierr)
  call h5_abort_on_error ()

  ! open file and go to first step
  file = h5_openfile (FNAME, H5_O_RDONLY, H5_PROP_DEFAULT)
  status = h5_setstep(file, 1_8)

  ! compute number of particles this process has to read
  nparticels_total = h5pt_getnpoints (file)
  nparticels = nparticels_total / comm_size

  if (comm_rank+1 == comm_size) then
     nparticels = nparticels + mod (nparticels_total, comm_size)
  end if

  write (*, "('Total number of particles: ', i8)") nparticels_total
  write (*, "('Number of particles on this core: ', i8)") nparticels

  ! read data
  status = h5pt_setnpoints (file, nparticels)
  allocate (data (nparticels))
  status = h5pt_readdata_i4 (file, "data", data)

  ! cleanup
  status = h5_closefile (file)
  deallocate (data)
  call mpi_finalize (ierr)

end program read_setnparticles
