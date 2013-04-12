#include <stdlib.h>
#include <assert.h>
#include <mpi.h>
#include <H5Part.h>

#define DATASIZE 32

int main(int argc, char** argv)
{
    int rank, nprocs;
    h5part_float64_t x[DATASIZE];
    h5part_float64_t y[DATASIZE];
    h5part_float64_t z[DATASIZE];
    h5part_float64_t px[DATASIZE];
    h5part_float64_t py[DATASIZE];
    h5part_float64_t pz[DATASIZE];
    h5part_int64_t nparticles = DATASIZE;
    H5PartFile *file;

    // initialize MPI
    MPI_Init (&argc, &argv);
    MPI_Comm_rank (MPI_COMM_WORLD, &rank);
    MPI_Comm_size (MPI_COMM_WORLD, &nprocs);

    H5PartSetVerbosityLevel(H5PART_VERB_DEBUG);

    file = H5PartOpenFileParallel("particles.h5", H5PART_WRITE, MPI_COMM_WORLD);
    H5PartSetStep(file, 0);
    H5PartSetNumParticles(file, nparticles);
    H5PartWriteDataFloat64(file, "x", x);
    H5PartWriteDataFloat64(file, "y", y);
    H5PartWriteDataFloat64(file, "z", z);
    H5PartWriteDataFloat64(file, "px", px);
    H5PartWriteDataFloat64(file, "py", py);
    H5PartWriteDataFloat64(file, "pz", pz);
    H5PartCloseFile(file);

    MPI_Finalize();
    return EXIT_SUCCESS;
}

