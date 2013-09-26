#include "H5hut.h"


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
        H5WriteFileAttribString (f, "FileAttrString", "This is a string attribute bound to the file.");
	H5CloseFile (f);

        h5_prop_t prop = H5CreateFileProp ();
        H5SetPropFileMPIO (prop, &comm);
        f = H5OpenFile2 ("testfile.h5", H5_O_APPEND, prop);
        H5CloseProp (prop);
        int64_t id[] = {42, 43, 44, 45};
        H5WriteFileAttribInt64 (f, "FileAttrInt64", id, sizeof(id)/sizeof(id[0]));
	H5CloseFile (f);

	MPI_Finalize ();
	return 0;
}
