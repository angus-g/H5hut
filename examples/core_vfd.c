#include <stdlib.h>
#include <assert.h>
#include <mpi.h>
#include <H5Part.h>

#define DATASIZE 32

int main(int argc, char** argv)
{
    int i, rank, nprocs;
    h5part_int32_t data[DATASIZE];
    h5part_int64_t stat;
    H5PartFile *file;

    // initialize MPI
    MPI_Init (&argc, &argv);
    MPI_Comm_rank (MPI_COMM_WORLD, &rank);
    MPI_Comm_size (MPI_COMM_WORLD, &nprocs);

    H5PartSetVerbosityLevel(H5PART_VERB_DEBUG);

    char filename[8];
    sprintf (filename, "%d.h5", rank);

    file = H5PartOpenFileParallel(
            filename,
            H5PART_WRITE | H5PART_VFD_CORE,
            MPI_COMM_SELF);
    assert (file != NULL);

    stat = H5PartSetStep(file, 0);
    assert (stat == H5PART_SUCCESS);

    stat = H5PartSetNumParticles(file, DATASIZE);
    assert (stat == H5PART_SUCCESS);

    // create fake data
    for (i=0; i<DATASIZE; i++) {
        data[i] = i + rank * DATASIZE;
    }

    // write the data
    stat = H5PartWriteDataInt32(file, "data", data);
    assert (stat == H5PART_SUCCESS);

    H5PartCloseFile(file);

    MPI_Finalize();
    return EXIT_SUCCESS;
}

