#include <math.h>
#include <stdlib.h>
#include <H5hut.h>

#include "testframe.h"
#include "params.h"

/* global */
h5_size_t layout[6];

/* from write.c */
void h5b_test_write1(void);

/* from read.c */
void h5b_test_read1(void);

static int
_nth_root_int_divisor (const int m, const int n)
{
	int i, root;
	double p;

	p = 1.0 / (double)n;
	root = (int) ceil ( pow ( (double)m, p ) );
	for (i=root; i<=m; i++)
	{
		if (m % i == 0) return i;
	}

	return i;
}

int main(int argc, char **argv)
{
	extern h5_size_t layout[6];
#ifdef PARALLEL_IO
	MPI_Init(&argc, &argv);

	int procs, rank;
	MPI_Comm_rank(MPI_COMM_WORLD, &rank);
	MPI_Comm_size(MPI_COMM_WORLD, &procs);

	if (procs > MAX_MPI_TASKS) {
		fprintf(stderr,
			"ERROR: please use <= %d MPI tasks for the test.\n",
			MAX_MPI_TASKS);
                exit(EXIT_FAILURE);
	}

	/* make up a 3D layout */
	h5_size_t decomp[3];
	decomp[0] = _nth_root_int_divisor (procs, 3);
	decomp[1] = _nth_root_int_divisor (procs / decomp[0], 2);
	decomp[2] = procs / decomp[0] / decomp[1];

	h5_size_t i,j,k;
	k = rank % decomp[2];
	j = (rank / decomp[2]) % decomp[1];
	i = rank / (decomp[2] * decomp[1]);

	layout[0] = i*NBLOCKX;
	layout[1] = (i+1)*NBLOCKX - 1;
	layout[2] = j*NBLOCKY;
	layout[3] = (j+1)*NBLOCKY - 1;
	layout[4] = k*NBLOCKZ;
	layout[5] = (k+1)*NBLOCKZ - 1;
#else // PARALLEL_IO
	layout[0] = 0;
	layout[1] = NBLOCKX - 1;
	layout[2] = 0;
	layout[3] = NBLOCKY - 1;
	layout[4] = 0;
	layout[5] = NBLOCKZ - 1;
#endif

        /* Initialize testing framework */
        TestInit(argv[0], NULL, NULL);

	/* Tests are generally arranged from least to most complexity... */
	AddTest("write1", h5b_test_write1, NULL, "Write 64-bit data", NULL);
	AddTest("read1", h5b_test_read1, NULL, "Read 64-bit data", NULL);

	/* Display testing information */
	TestInfo(argv[0]);

	/* Parse command line arguments */
	TestParseCmdLine(argc, argv);

	H5SetVerbosityLevel(GetTestVerbosity());

	/* Perform requested testing */
	PerformTests();

	/* Display test summary, if requested */
	if (GetTestSummary())
	TestSummary();

	/* Clean up test files, if allowed */
	//if (GetTestCleanup() && !getenv("HDF5_NOCLEANUP"))
	//    TestCleanup();

#ifdef PARALLEL_IO
	TestPrintf ("reached end\n");
        fflush(stdout);
        MPI_Finalize();
#endif
	return GetTestNumErrs();
}

