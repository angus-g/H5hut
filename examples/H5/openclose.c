#include "H5hut.h"

#if !defined (PARALLEL_IO)
#define MPI_Init(argc, argv)
#define MPI_Comm_size(comm, nprocs) { *nprocs = 1; }
#define MPI_Comm_rank(comm, myproc) { *myproc = 0; }
#define MPI_Finalize()
#define MPI_COMM_WORLD (0)
#endif

int
main (
	int argc,
	char** argv
	) {
	MPI_Comm comm = MPI_COMM_WORLD;

	int myproc;
	int nprocs;
	MPI_Init (&argc, &argv);
	MPI_Comm_size (comm, &nprocs);
	MPI_Comm_rank (comm, &myproc);

	h5_file_t f = H5OpenFile ("testfile.h5", H5_O_WRONLY, comm);
	H5CloseFile (f);

        h5_prop_t prop = H5CreateFileProp ();
        H5SetPropFileMPIO (prop, &comm);
        f = H5OpenFile2 ("testfile.h5", H5_O_WRONLY, prop);
        H5CloseProp (prop);

	MPI_Finalize ();
	return 0;
}
