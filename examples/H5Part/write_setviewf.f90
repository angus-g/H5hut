!
!  Copyright (c) 2006-2013, The Regents of the University of California,
!  through Lawrence Berkeley National Laboratory (subject to receipt of any
!  required approvals from the U.S. Dept. of Energy) and the Paul Scherrer
!  Institut (Switzerland).  All rights reserved.!
!
!  License: see file COPYING in top level of source distribution.
!
include 'H5hut.f90'

program write_setview
  use H5hut
  implicit none
  include 'mpif.h'

  ! the file name we want to read
  character (len=*), parameter :: FNAME =       "example_setview.h5"
  integer*8, parameter :: DATASIZE =            32
  integer*8, parameter :: ITERS =               4

  integer*4, parameter :: npoints =             ITERS*DATASIZE

  integer :: comm, rank, ierr
  integer*8 :: file, status
  integer*4 :: i
  integer*4, allocatable :: data(:)
  integer*8 start, end, offset

  ! init MPI & H5hut
  comm = MPI_COMM_WORLD
  call mpi_init(ierr)
  call mpi_comm_rank(comm, rank, ierr)
  call h5_abort_on_error ()
  call h5_set_verbosity_level (-1_8)


  ! create fake data
  allocate (data (npoints))
  do i = 1, npoints
    data (i) = i + rank*npoints
  enddo

  ! open the a file for parallel writing and ceate step #0
  file = h5_openfile (FNAME, H5_O_WRONLY, H5_PROP_DEFAULT)
  status = h5_setstep(file, 0_8)

  ! set the size of the 1D array
  status = h5pt_setnpoints (file, int8(npoints))

  offset = rank*npoints
  do i = 1, ITERS
     start = offset + 1 + (i-1)*DATASIZE
     end = offset + i*DATASIZE 
     status = h5pt_setview (file, start, end)
     ! write the particles
     status = h5pt_writedata_i4 (file, "data", data ((i-1)*DATASIZE+1))
  end do

  ! cleanup
  status = h5_closefile (file)
  deallocate (data)
  call mpi_finalize (ierr)

end program write_setview
