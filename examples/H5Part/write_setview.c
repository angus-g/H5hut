#include <stdlib.h>
#include <assert.h>
#include <mpi.h>
#include <H5hut.h>

#define DATASIZE 32
#define ITERS 4

int main (
        int argc, char** argv
        ) {
        int i, rank, nprocs;
        h5_int32_t data[ITERS*DATASIZE];
        h5_int64_t stat;
        h5_int64_t offset;
        h5_file_t file;
        
        // initialize MPI
        MPI_Init (&argc, &argv);
        MPI_Comm_rank (MPI_COMM_WORLD, &rank);
        MPI_Comm_size (MPI_COMM_WORLD, &nprocs);

        H5SetVerbosityLevel(1);

        file = H5OpenFile ("test.h5", H5_O_WRONLY, MPI_COMM_WORLD);
        assert (file != H5_FAILURE);

        stat = H5SetStep(file, 0);
        assert (stat == H5_SUCCESS);

        stat = H5PartSetNumParticles(file, ITERS*DATASIZE);
        assert (stat == H5_SUCCESS);

        // create fake data
        for (i=0; i<ITERS*DATASIZE; i++) {
                data[i] = i + rank * ITERS * DATASIZE;
        }

        offset = rank * ITERS * DATASIZE;

        // iterate over arrays
        for (i=0; i<ITERS; i++) {
                // set the "view" to select a subset of the dataset
                stat = H5PartSetView (file,
                                      offset + i * DATASIZE,
                                      offset + (i+1) * DATASIZE - 1);
                assert (stat == H5_SUCCESS);
                // write the data
                stat = H5PartWriteDataInt32 (file, "data", data + i*DATASIZE);
                assert (stat == H5_SUCCESS);
        }
        
        H5CloseFile(file);
        
        MPI_Finalize();
        return H5_SUCCESS;
}

