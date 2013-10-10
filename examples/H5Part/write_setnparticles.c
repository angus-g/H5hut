
#include <stdlib.h>
#include "H5hut.h"

#define NUM_PARTICLES 3

int
main (
        int argc,
        char* argv[]
        ){
        h5_file_t file;
        int nprocs = 0;
        int myproc = 0;
        int step = 0;

        MPI_Comm comm = MPI_COMM_WORLD;

        MPI_Init (&argc,&argv);
        MPI_Comm_size (comm,&nprocs);
        MPI_Comm_rank (comm,&myproc);

        h5_int64_t id[NUM_PARTICLES];
        h5_int64_t num_particles = NUM_PARTICLES;

        for (int i = 0; i < num_particles; i++) {
                id[i] = i + num_particles * myproc;
        }


        file = H5OpenFile ("parttest.h5", H5_O_WRONLY, comm);
        H5SetStep (file, step); 
        H5PartSetNumParticles (file,num_particles);
        H5PartWriteDataInt64 (file, "id", id);
        H5CloseFile(file);

        MPI_Finalize ();
}
