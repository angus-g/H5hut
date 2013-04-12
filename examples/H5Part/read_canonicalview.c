
#include <stdlib.h>
#include "H5hut.h"

int
main (
        int argc, char* argv[]
        ){
        h5_file_t file;
        int nprocs, myproc;
        MPI_Comm comm = MPI_COMM_WORLD;

        MPI_Init (&argc, &argv);
        MPI_Comm_size (comm, &nprocs);
        MPI_Comm_rank (comm, &myproc);
        H5SetVerbosityLevel (4);
        file = H5OpenFile ("parttest.h5", H5_O_RDONLY, comm);

        H5SetStep (file, 0);
  
        H5PartSetCanonicalView (file);
        h5_int64_t num_particles = H5PartGetNumParticles (file);
        printf ("[proc %d]: particles in view: %lld\n", myproc, num_particles);

        H5CloseFile(file);
        return MPI_Finalize();
}
