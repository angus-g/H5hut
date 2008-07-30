#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <hdf5.h>
#include "H5Part.h"


/*
  A simple regression test that shows how you use this API
  to write and read multi-timestep files of particle data.
*/
#ifdef PARALLEL_IO

int main(int argc,char *argv[]){
  int N = 10;
  int sz=0;
  double *x,*y,*z;
  h5part_int64_t *id;
  H5PartFile *file;
  int i,t,nt,nds;
  int nprocs,myproc;

  unsigned int np = 0;

  MPI_Comm comm=MPI_COMM_WORLD;

  MPI_Init(&argc,&argv);
  MPI_Comm_size(comm,&nprocs);
  MPI_Comm_rank(comm,&myproc);

  /* parallel file creation */
  file=H5PartOpenFileParallel("parttest.h5",H5PART_WRITE,comm);
  if(!file) {
    perror("File open failed:  exiting!");
    exit(0);
  }

  for(t=0;t<5;t++){

    MPI_Barrier(comm);

    sz = myproc*N;
    // proc[0] sz = 10, (next step N=10), sz=10
    // proc[1] sz = 20, (next step N=20), sz=40
    fprintf(stderr,"proc[%u] sz=%u\n",myproc,(unsigned)sz);
    x =(double*)malloc(1+sz*sizeof(double));
    y =(double*)malloc(1+sz*sizeof(double));
    z =(double*)malloc(1+sz*sizeof(double));
    id=(h5part_int64_t*)malloc(1+sz*sizeof(h5part_int64_t));

    for(i=0;i<sz;i++) {
      x[i]=(double)(i+t)+10.0*(double)myproc;
      y[i]=0.1 + (double)(i+t);
      z[i]=0.2 + (double)(i+t*10);
      id[i]=i+sz*myproc;
    }
    
    fprintf(stderr,"Proc[%u] Writing timestep %u Np=%u\n",myproc,t,sz);

    H5PartSetStep(file,t); /* must set the current timestep in file */
 
	fprintf(stderr,"Proc[%u]: setNumParticles start\n",myproc);
    H5PartSetNumParticles(file,sz); /* then set number of particles to store */
	fprintf(stderr,"Proc[%u]: setNumParticles done\n",myproc);

    /* now write different tuples of data into this timestep of the file */
	fprintf(stderr,"Proc[%u]: WriteX start\n",myproc);
    H5PartWriteDataFloat64(file,"x",x); 
	fprintf(stderr,"Proc[%u]: WriteX done\n",myproc);
    H5PartWriteDataFloat64(file,"y",y);
    H5PartWriteDataFloat64(file,"z",z);

    H5PartWriteDataFloat64(file,"px",x); 
    H5PartWriteDataFloat64(file,"py",y);
    H5PartWriteDataFloat64(file,"pz",z);

    H5PartWriteDataInt64(file,"id",id);

    if(x) 
      free(x); 
    if(y) 
      free(y);
    if(z) 
      free(z);
    if(id) 
      free(id);

    // remove the next line and everything is ok
    N = 1 + sz;
  }

  printf("AllDone p[%u]\n",myproc);
  H5PartCloseFile(file);
  MPI_Barrier(comm);

  unsigned int idStart = 0;
  unsigned int idEnd = myproc*10;
  printf("p[%u:%u] : OK, close file and reopen for reading idStart %u  idEnd %u \n",myproc,nprocs,idStart,idEnd);

  file=H5PartOpenFileParallel("parttest.h5",H5PART_READ,comm);
  H5PartSetStep(file,0);

  nt = H5PartGetNumSteps(file); /* get number of steps in file */
  nds=H5PartGetNumDatasets(file); /* get number of datasets in timestep 0 */

  MPI_Barrier(comm);

  H5PartSetView(file,idStart,idEnd);
  np = H5PartGetNumParticles(file);
  printf("steps= %u  datasets= %u particles= %u\n",nt,nds,np);

  H5PartCloseFile(file);
  MPI_Barrier(comm);
  fprintf(stderr,"proc[%u]:  done\n",myproc);
  return MPI_Finalize();
}

#endif
