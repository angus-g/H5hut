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

  ! name of input file
  character (len=*), parameter :: fname = "example_setview.h5"

  ! H5hut verbosity level
  integer*8, parameter         :: h5_verbosity = H5_VERBOSE_DEFAULT
  
  integer   :: comm, comm_size, comm_rank, mpi_ierror
  integer*8 :: file, h5_ierror
  integer*8 :: num_particles
  integer*8 :: i
  integer*4, allocatable :: data(:)

  ! initialize MPI & H5hut
  comm = MPI_COMM_WORLD
  call mpi_init (mpi_error)
  call mpi_comm_size (comm, comm_size, mpi_ierror)
  call mpi_comm_rank (comm, comm_rank, mpi_ierror)
  call h5_abort_on_error ()
  call h5_set_verbosity_level (h5_verbosity)

  ! open file and go to first step
  file = h5_openfile (fname, H5_O_RDONLY, H5_PROP_DEFAULT)
  h5_ierror = h5_setstep(file, 1_8)

  ! set canonical view
  h5_ierror = h5pt_setcanonicalview (file)
  num_particles = h5pt_getnpoints (file)
  write (*, "('[proc ', i4, '] particles in view: ', i8)") comm_rank, num_particles

  ! read and print data
  allocate (data (num_particles))
  h5_ierror = h5pt_readdata_i4 (file, "data", data);
  do i = 1, num_particles
     write (*, "('[proc ', i4, ']: local index = ', i4, ', value = ', i4)") &
          comm_rank, i, data(i)
  end do

  ! cleanup
  deallocate (data)
  h5_ierror = h5_closefile (file)
  call mpi_finalize (mpi_ierror)

end program read_canonicalview
