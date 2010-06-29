#include <stdlib.h>
#include <assert.h>
#include <mpi.h>
#include <H5Part.h>

#define XSIZE 8
#define YSIZE 8
#define ZSIZE 8
#define DATASIZE XSIZE*YSIZE*ZSIZE
#define H5OpenFileParallel H5PartOpenFileParallel
#define H5SetStep H5PartSetStep
#define H5Block3dSetLayout H5BlockDefine3DFieldLayout
#define H5CloseFile H5PartCloseFile

int main(int argc, char** argv)
{
    int rank, nprocs;
    h5part_float64_t ex[DATASIZE];
    h5part_float64_t ey[DATASIZE];
    h5part_float64_t ez[DATASIZE];
    h5part_float64_t q[DATASIZE];
    h5part_int64_t nparticles = DATASIZE;
    H5PartFile *file;

    // initialize MPI
    MPI_Init (&argc, &argv);
    MPI_Comm_rank (MPI_COMM_WORLD, &rank);
    MPI_Comm_size (MPI_COMM_WORLD, &nprocs);

    H5PartSetVerbosityLevel(H5PART_VERB_DEBUG);

    file = H5OpenFileParallel("fields.h5", H5PART_WRITE, MPI_COMM_WORLD);
    H5SetStep(file, 0);
    H5Block3dSetLayout(file,
            rank*XSIZE, (rank+1)*XSIZE - 1,
            0, YSIZE - 1,
            0, ZSIZE - 1);
    H5Block3dWriteScalarFieldFloat64(file, "Q", q);
    H5Block3dWrite3dVectorFieldFloat64(file, "E", ex, ez, ey);
    H5CloseFile(file);

    MPI_Finalize();
    return EXIT_SUCCESS;
}

