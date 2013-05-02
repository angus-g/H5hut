#include <stdlib.h>
#include <assert.h>
#include <mpi.h>

#include <H5.h>
#include <H5Block.h>

#define XSIZE 8
#define YSIZE 8
#define ZSIZE 8
#define DATASIZE XSIZE*YSIZE*ZSIZE

int
main (
        int argc,
        char** argv
        ) {
        int rank, nprocs;
        h5_float64_t ex[DATASIZE];
        h5_float64_t ey[DATASIZE];
        h5_float64_t ez[DATASIZE];
        h5_float64_t q[DATASIZE];
        h5_file_t file;

        // initialize MPI
        MPI_Init (&argc, &argv);
        MPI_Comm_rank (MPI_COMM_WORLD, &rank);
        MPI_Comm_size (MPI_COMM_WORLD, &nprocs);

        H5SetVerbosityLevel(H5_VERBOSE_INFO);

        file = H5OpenFile ("fields.h5", H5_O_WRONLY, MPI_COMM_WORLD);
        H5SetStep(file, 0);
        H5Block3dSetView (file,
                           rank*XSIZE, (rank+1)*XSIZE - 1,
                           0, YSIZE - 1,
                           0, ZSIZE - 1);
        H5Block3dWriteScalarFieldFloat64(file, "Q", q);
        H5Block3dWriteVector3dFieldFloat64(file, "E", ex, ez, ey);
        H5CloseFile(file);

        MPI_Finalize();
        return H5_SUCCESS;
}

