#include <stdlib.h>
#include <assert.h>
#include <mpi.h>
#include <H5hut.h>

#define DATASIZE 32

int main(
        int argc, char** argv
        ) {
        int i, rank, nprocs;
        h5_int32_t data[DATASIZE];
        h5_int64_t stat;
        h5_file_t file;

        // initialize MPI
        MPI_Init (&argc, &argv);
        MPI_Comm_rank (MPI_COMM_WORLD, &rank);
        MPI_Comm_size (MPI_COMM_WORLD, &nprocs);

        H5SetVerbosityLevel (1);

        char filename[8];
        sprintf (filename, "%d.h5", rank);
        file = H5OpenFile (filename, H5_O_WRONLY|H5_VFD_CORE, MPI_COMM_SELF);
        assert (file != H5_FAILURE);

        stat = H5SetStep (file, 0);
        assert (stat == H5_SUCCESS);

        stat = H5PartSetNumParticles(file, DATASIZE);
        assert (stat == H5_SUCCESS);

        // create fake data
        for (i=0; i<DATASIZE; i++) {
                data[i] = i + rank * DATASIZE;
        }

        // write the data
        stat = H5PartWriteDataInt32(file, "data", data);
        assert (stat == H5_SUCCESS);
        
        H5CloseFile(file);

        MPI_Finalize();
        return H5_SUCCESS;
}

