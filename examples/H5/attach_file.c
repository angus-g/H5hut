#include <H5hut.h>
#include <fcntl.h>
#include <stdlib.h>
#include <sys/stat.h>

#define FNAME           "attach_file.h5"
#define ATTACHMENT      "attach_file"

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
        char* argv[]
        ) {
	MPI_Init (&argc, &argv);

	H5SetErrorHandler (H5AbortErrorhandler);
	H5SetVerbosityLevel (255);
	h5_file_t f = H5OpenFile (FNAME, H5_O_WRONLY, H5_PROP_DEFAULT);
	H5AddAttachment (f, ATTACHMENT);
	H5CloseFile (f);
	f = H5OpenFile (FNAME, H5_O_RDONLY, H5_PROP_DEFAULT);
	h5_ssize_t num_attachments = H5GetNumAttachments (f);
	printf ("Number of attachments: %lld\n", (long long int)num_attachments);
	int i;
	char fname[FILENAME_MAX];
	h5_size_t fsize;
	for (i=0; i < num_attachments; i++) {
		H5GetAttachmentInfoByIdx (f, i, fname, sizeof(fname), &fsize);
		printf (
			"Attachment %d: Name: %s, Size: %llu\n",
			i, fname, (long long unsigned)fsize);
		H5GetAttachment (f, fname);
		H5DeleteAttachment (f, fname);
	}
	H5CloseFile (f);
	return 0;
}
