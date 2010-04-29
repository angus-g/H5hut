program H5PartTest
  implicit none

  include 'mpif.h'
  include 'H5PartF.h'

  integer :: comm, rank, ierr
  integer*8 :: file_id, status, npoints, i
  real*8, allocatable :: particles(:)
  integer*8, allocatable :: id(:)

  comm = MPI_COMM_WORLD
  call mpi_init(ierr)
  call mpi_comm_rank(comm, rank, ierr)

  ! open the a file for parallel writing
  file_id = h5pt_openw_par('test.h5', comm)

  ! in the Fortran API, time steps start at 1
  status = h5pt_setstep(file_id, 1_8)

  ! write an attribute to the file
  status = h5pt_writefileattrib_string(file_id, 'desc', 'This is a test.')

  ! create fake data
  npoints = 99
  allocate(particles(npoints), id(npoints))
  do i=1,npoints
    particles(i) = real(i+npoints*rank)
    id(i) = i+npoints*rank
  enddo

  ! set the size of the 1D array
  status = h5pt_setnpoints(file_id, npoints)

  ! write the particles
  status = h5pt_writedata_r8(file_id, "x",  particles)

  ! write the ids
  status = h5pt_writedata_i8(file_id, "id", id)

  ! close the file
  status = h5pt_close(file_id)

  deallocate(particles, id)

  call mpi_finalize(ierr)

end program H5PartTest
