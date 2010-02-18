#include <stdlib.h>
#include <assert.h>
#include <mpi.h>
#include <H5Part.h>

#define DATASIZE 32
#define ITERS 4

int main(int argc, char** argv)
{
    int i, rank, nprocs;
    h5part_int32_t data[ITERS*DATASIZE];
    h5part_int64_t stat;
    h5part_int64_t offset;
    H5PartFile *file;

    // initialize MPI
    MPI_Init (&argc, &argv);
    MPI_Comm_rank (MPI_COMM_WORLD, &rank);
    MPI_Comm_size (MPI_COMM_WORLD, &nprocs);

    H5PartSetVerbosityLevel(H5PART_VERB_DEBUG);

    file = H5PartOpenFileParallel("test.h5", H5PART_WRITE, MPI_COMM_WORLD);
    assert (file != NULL);

    stat = H5PartSetStep(file, 0);
    assert (stat == H5PART_SUCCESS);

    stat = H5PartSetNumParticles(file, ITERS*DATASIZE);
    assert (stat == H5PART_SUCCESS);

    // create fake data
    for (i=0; i<ITERS*DATASIZE; i++) {
        data[i] = i + rank * ITERS * DATASIZE;
    }

    offset = rank * ITERS * DATASIZE;

    // iterate over arrays
    for (i=0; i<ITERS; i++) {
        // set the "view" to select a subset of the dataset
        stat = H5PartSetView(file,
                offset + i*DATASIZE,
                offset + (i+1)*DATASIZE - 1);
        assert (stat == H5PART_SUCCESS);
        // write the data
        stat = H5PartWriteDataInt32(file, "data", data + i*DATASIZE);
        assert (stat == H5PART_SUCCESS);
    }

    H5PartCloseFile(file);

    MPI_Finalize();
    return EXIT_SUCCESS;
}

