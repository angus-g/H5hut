#include <stdio.h>
#include <stdlib.h>
#include <hdf5.h>
#include "H5Part.h"
#include "H5Block.h"

#if 0
int ReadFile(const char *fn){
	char name[64];
	H5PartFile *file;
	h5part_int64_t i,t,nt,nds,myproc;
	unsigned int steps;
	printf ( "Open %s\n", fn );
	
	file = H5PartOpenFile (fn, H5PART_READ);
  
	nt=H5PartGetNumSteps(file);
	H5PartSetStep(file,0);
	nds=H5PartGetNumDatasets(file);
  
	printf ( "Timesteps = %d; dataSets per timestep = %d\n", nt, nds );
	printf ( "\n\n===============================" );
	for(i=0;i<nds;i++){ 
		H5PartGetDatasetName(file,i,name,64);
		printf("\tDataset[%u] name=[%s]\n",
		       i,name);
	}
	printf ( "===============================\n\n" );
  
	for (steps=0; steps<nt; steps++) {
		H5PartSetStep(file,steps);
		h5part_int64_t n = H5PartGetNumParticles(file);
		printf ( "number of particles this step = %d\n", n );
		double *x = malloc ( n * sizeof ( *x ) );
		double *y = malloc ( n * sizeof ( *y ) );
		double *z = malloc ( n * sizeof ( *z ) );
		double *px= malloc ( n * sizeof ( *px ) );
		double *py= malloc ( n * sizeof ( *py ) );
		double *pz= malloc ( n * sizeof ( *pz ) );
		h5part_int64_t *id = malloc ( n * sizeof ( *id ) );
		
		H5PartReadParticleStep(file,steps,x,y,z,px,py,pz,id);
    
		double sumx = 0.0;
		double sumpz = 0.0;
		for ( i=0; i<n; i++ ) {
			sumx += x[i];
			sumpz += pz[i];
		}
					      
		printf ("\tstep=%d sum(x)= %d sum(pz)=%d\n",
			steps, sumx, sumpz );
		printf ("\tfirst x is %d\tlast x is %d\n",
			x[0], x[n-1] );
		printf ("\tFor fake data, expect sumx to be = %f\n", 
			x[0]*((double)n) );
		free ( x );
		free ( y );
		free ( z );
		free ( px );
		free ( py );
		free ( pz );
		free ( id );
	}
	{
		h5part_float64_t *data = malloc ( 4*6*8* sizeof(*data) );
		h5part_int64_t herr;
		h5part_int64_t i, j, k;
		h5part_int64_t i_max, j_max, k_max;

		i_max = 3;
		j_max = 5;
		k_max = 7;

		herr = H5BlockOpen ( file );
		if ( herr < 0 ) {
			printf ("Ops!\n");
			exit ( 2 );
		}

		herr = H5BlockDefine3DFieldLayout ( file, 0, i_max, 0, j_max, 0, k_max );
		if ( herr < 0 ) {
			printf ("Ops!\n");
			exit ( 2 );
		}
  
		herr = H5Block3dReadScalarField ( file, "test", data );
		if ( herr < 0 ) {
			printf ("Ops!\n");
			exit ( 2 );
		}

		for ( i = 0; i <= i_max; i++ ) {
			for ( j = 0; j <= j_max; j++ ) {
				for ( k = 0; k <= k_max; k++ ) {
					if ( *(data+ i + j*(i_max+1) + k*(i_max+1)*(j_max+1)) != i + 100*j + 10000*k ) {
						printf ( "Block data error!\n" );
						exit ( 2 );
					}
				}
			}
		}

		i_max = 2;
		j_max = 2;
		k_max = 2;

		herr = H5BlockDefine3DFieldLayout ( file, 0, i_max, 0, j_max, 0, k_max );
		if ( herr < 0 ) {
			printf ("Ops!\n");
			exit ( 2 );
		}
		herr = H5Block3dReadScalarField ( file, "test", data );
		if ( herr < 0 ) {
			printf ("Ops!\n");
			exit ( 2 );
		}

		for ( i = 0; i <= i_max; i++ ) {
			for ( j = 0; j <= j_max; j++ ) {
				for ( k = 0; k <= k_max; k++ ) {
					printf ( "%f\n", *(data+ i + j*(i_max+1) + k*(i_max+1)*(j_max+1)) );
				}
			}
		}


		herr = H5BlockClose ( file );
		if ( herr < 0 ) {
			printf ("Ops!\n");
			exit ( 2 );
		}

		free ( data );
	}

	H5PartCloseFile(file);
	return 1;
}
#endif

int WriteFile(const char *fn){
	H5PartFile *f;
	h5part_int64_t i, j, k;
	int timestep;
	int timesteps = 5;
	h5part_int64_t herr;
	h5part_float64_t *data;
	h5part_int64_t i_dims, j_dims, k_dims;

	printf ("Open %s\n", fn );
  
	f = H5PartOpenFileParallel ( fn, H5PART_WRITE, MPI_COMM_WORLD );
	herr = H5BlockOpen ( f );
	if ( herr < 0 ) exit ( 2 );

	for ( timestep = 0; timestep < timesteps; timestep++){
		herr = H5PartSetStep ( f, timestep );
		if ( herr < 0 ) exit ( 2 );

		printf ( "Write Step %d\n", timestep );

		i_dims = 4;
		j_dims = 6;
		k_dims = 8;

		data = (h5part_float64_t*) malloc ( i_dims * j_dims * k_dims * sizeof(*data) );
		for ( i = 0; i < i_dims; i++ ) {
			for ( j = 0; j < j_dims; j++ ) {
				for ( k = 0; k < k_dims; k++ ) {
					*(data+ i + j*i_dims + k*i_dims*j_dims) = i+ 10*j + 100*k + 1000*f->myproc;
				}
			}
		}

		herr = H5BlockDefine3DFieldLayout ( f, f->myproc*2, f->myproc*2+i_dims-1, 0, j_dims-1, 0, k_dims-1 );
		if ( herr < 0 ) exit ( 2 );
		herr = H5Block3dWriteScalarField ( f, "test", data );
		if ( herr < 0 ) exit ( 2 );
		free ( data );
	}
	herr = H5BlockClose ( f );
	if ( herr < 0 ) exit ( 2 );

	H5PartCloseFile ( f );
	return 1;
}

int main(int argc,char **argv){
	char *fn = "testfile.h5";

	MPI_Init(&argc,&argv);
	H5PartSetVerbosityLevel ( 40 );

	if(!WriteFile(fn)){
		printf ("Failed to write file %s\n", fn );
		exit ( 2 );
	}
#if 0
	if(!ReadFile(fn)){
		printf ("Failed to read file %s\n", fn );
		exit ( 2 );
	}
#endif
	return 0;
}

