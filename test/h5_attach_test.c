#include <H5hut.h>
#include <fcntl.h>
#include <stdlib.h>
#include <sys/stat.h>

#define FNAME           "h5_attach.h5"
#define ATTACHMENT      "h5_attach_test"


int
main (
        int argc,
        char* argv[]
        ) {
	H5SetErrorHandler (H5AbortErrorhandler);
	H5SetVerbosityLevel (255);
	h5_file_t f = H5OpenFile (FNAME, H5_O_WRONLY, 0);
	H5AddAttachment (f, ATTACHMENT);
	H5CloseFile (f);
	f = H5OpenFile (FNAME, H5_O_RDONLY, 0);
	h5_ssize_t num_attachments = H5GetNumAttachments (f);
	printf ("Number of attachments: %lld\n", num_attachments);
	int i;
	char fname[FILENAME_MAX];
	h5_size_t fsize;
	for (i=0; i < num_attachments; i++) {
		H5GetAttachmentInfoByIdx (f, i, fname, sizeof(fname), &fsize);
		printf ("Attachment %d: Name: %s, Size: %llu\n", i, fname, fsize);
		H5GetAttachment (f, fname);
		H5DeleteAttachment (f, fname);
	}
	H5CloseFile (f);
	return 0;
}
