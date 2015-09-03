!
!  Copyright (c) 2006-2015, The Regents of the University of California,
!  through Lawrence Berkeley National Laboratory (subject to receipt of any
!  required approvals from the U.S. Dept. of Energy) and the Paul Scherrer
!  Institut (Switzerland).  All rights reserved.!
!
!  License: see file COPYING in top level of source distribution.
!
include 'H5hut.f90'

program read_stridedf
  use H5hut
  implicit none
  include 'mpif.h'

  ! the file name we want to read
  character (len=*), parameter :: FNAME =       "example_strided.h5"
  integer*8, parameter :: DEFAULT_VERBOSITY = H5_VERBOSE_DEFAULT

  integer*8 :: verbosity = DEFAULT_VERBOSITY
  integer :: comm, comm_rank, comm_size, ierr
  integer*8 :: file, status
  real*8, allocatable :: data(:)
  integer*8 :: nparticles

  ! initialize MPI & H5hut
  comm = MPI_COMM_WORLD
  call mpi_init (ierr)
  call mpi_comm_rank (comm, comm_rank, ierr)
  call mpi_comm_size (comm, comm_size, ierr)
  call h5_abort_on_error ()
  call h5_set_verbosity_level (verbosity)

  ! open file and go to first step
  file = h5_openfile (FNAME, H5_O_RDONLY, H5_PROP_DEFAULT)
  status = h5_setstep (file, 1_8)

  ! Get number of particles in datasets and allocate memory
  nparticles = h5pt_getnpoints (file)
  allocate (data (6*nparticles))
    
  ! set number of particles and memory stride
  status = h5pt_setnpoints_strided (file, nparticles, 6_8)

  ! read data
  status = h5pt_readdata_r8 (file, "x",  data(1:))
  status = h5pt_readdata_r8 (file, "y",  data(2:))
  status = h5pt_readdata_r8 (file, "z",  data(3:))
  status = h5pt_readdata_r8 (file, "px", data(4:))
  status = h5pt_readdata_r8 (file, "py", data(5:))
  status = h5pt_readdata_r8 (file, "pz", data(6:))
  
  ! cleanup
  status = h5_closefile (file)
  deallocate (data)
  call mpi_finalize (ierr)

end program read_stridedf
