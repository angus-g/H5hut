!
!  Copyright (c) 2006-2015, The Regents of the University of California,
!  through Lawrence Berkeley National Laboratory (subject to receipt of any
!  required approvals from the U.S. Dept. of Energy) and the Paul Scherrer
!  Institut (Switzerland).  All rights reserved.!
!
!  License: see file COPYING in top level of source distribution.
!
include 'H5hut.f90'

program read_setviewf
  use H5hut
  implicit none
  include 'mpif.h'

  ! the file name we want to read
  character (len=*), parameter :: FNAME =       "example_setview.h5"
  integer*8, parameter :: DEFAULT_VERBOSITY = H5_VERBOSE_DEFAULT

  integer*8 :: verbosity = DEFAULT_VERBOSITY
  integer :: comm, comm_rank, comm_size, ierr
  integer*8 :: file, status
  integer*8 :: i
  integer*4, allocatable :: data(:)
  integer*8 :: total_particles, nparticles, remainder
  integer*8 :: start, end

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

  ! compute a "canonical" view: all cores get almost the same number of
  ! particles
  total_particles = h5pt_getnpoints (file);
  nparticles = total_particles / comm_size;
  remainder = mod (total_particles, comm_size);
  start = comm_rank * nparticles;

  ! adjust number of local particles
  if (comm_rank < remainder) then
     nparticles = nparticles + 1
  end if

  ! adjust start
  if (comm_rank < remainder) then
     start = start + comm_rank
  else
     start = start + remainder
  end if
  
  ! Note: setting end = start - 1 forces the 
  ! selection of zero particles!
  end = start + nparticles - 1;

  ! in Fortran we start at 1 not 0
  start = start + 1
  end = end + 1

  write (*, "('[proc ', i4, ']: set view to [', i4, '..', i4, ']')") comm_rank, start, end
  status = h5pt_setview (file, start, end);
  allocate (data (nparticles))

  status = h5pt_readdata_i4 (file, "data", data);
  do i = 1, nparticles
     write (*, "('[proc ', i4, ']: global index = ', i4, '; local index = ', i4, ', value = ', i4)") &
          comm_rank, start+i-1, i, data(i)
  end do

  ! cleanup
  status = h5_closefile (file)
  deallocate (data)
  call mpi_finalize (ierr)

end program read_setviewf
