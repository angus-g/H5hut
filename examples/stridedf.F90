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

  ! in the Fortran API, steps start at 1
  status = h5pt_setstep(file_id, 1_8)

  ! write an attribute to the file
  status = h5pt_writefileattrib_string(file_id, 'desc', 'This is a test.')

  ! create fake data
  npoints = 99
  allocate(particles(6*npoints), id(npoints))
  do i=0,npoints-1
    particles(6*i+1) = 0.0 + real(i+npoints*rank)
    particles(6*i+2) = 0.1 + real(i+npoints*rank)
    particles(6*i+3) = 0.2 + real(i+npoints*rank)
    particles(6*i+4) = 0.3 + real(i+npoints*rank)
    particles(6*i+5) = 0.4 + real(i+npoints*rank)
    particles(6*i+6) = 0.5 + real(i+npoints*rank)
    id(i+1) = i+npoints*rank
  enddo

  ! set the striding to 6
  status = h5pt_setnpoints_strided(file_id, npoints, 6_8)

  ! write the particles
  status = h5pt_writedata_r8(file_id, "x",  particles(1))
  status = h5pt_writedata_r8(file_id, "y",  particles(2))
  status = h5pt_writedata_r8(file_id, "z",  particles(3))
  status = h5pt_writedata_r8(file_id, "px", particles(4))
  status = h5pt_writedata_r8(file_id, "py", particles(5))
  status = h5pt_writedata_r8(file_id, "pz", particles(6))

  ! disable the striding to write the ids
  status = h5pt_setnpoints(file_id, npoints)
  status = h5pt_writedata_i8(file_id, "id", id)

  ! close the file
  status = h5pt_close(file_id)

  deallocate(particles, id)

  call mpi_finalize(ierr)

end program H5PartTest

