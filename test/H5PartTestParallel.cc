#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <hdf5.h>
#include "H5Part.hh"

#ifdef PARALLEL_IO

/*
  This regression test is used to ensure parallel I/O is 
  working correctly and that Views are working for 
  parallel reads.
 */
int main(int argc,char *argv[]){
  const int sz=5000;
  double *x,*y,*z;
  h5part_int64_t *id;
  char name[64];
  H5PartFile *file;
  int i,t,nt,nds;
  int nprocs,myproc;
  hid_t gid;
  MPI_Comm comm=MPI_COMM_WORLD;

  MPI_Init(&argc,&argv);
  MPI_Comm_size(comm,&nprocs);
  MPI_Comm_rank(comm,&myproc);

  x=(double*)malloc(sz*nprocs*sizeof(double));
  y=(double*)malloc(sz*nprocs*sizeof(double));
  z=(double*)malloc(sz*nprocs*sizeof(double));
  id=(h5part_int64_t*)malloc(sz*nprocs*sizeof(h5part_int64_t));
  /* parallel file creation */
  file=H5PartOpenFileParallel("parttest.h5",H5PART_WRITE,comm);
  if(!file) {
    perror("File open failed:  exiting!");
    exit(0);
  }

  for(t=0;t<5;t++){
    MPI_Barrier(comm);
    for(i=0;i<sz;i++) {
      x[i]=(double)(i+t)+10.0*(double)myproc;
      y[i]=0.1 + (double)(i+t);
      z[i]=0.2 + (double)(i+t*10);
      id[i]=i+sz*myproc;
    }
    printf("Proc[%u] Writing timestep %u\n",myproc,t);
    if(t==0){
	printf("Proc[%u]: data values x[first,last]=%f:%f y[%u:%u]=%f:%f z[:]=%f:%f id[:]=%f:%f\n",
		myproc,x[0],x[sz-1],0,sz-1,y[0],y[sz-1],z[0],z[sz-1],(int)id[0],(int)id[sz-1]);
    }
    H5PartSetStep(file,t); /* must set the current timestep in file */
    H5PartSetNumParticles(file,sz); /* then set number of particles to store */
    /* now write different tuples of data into this timestep of the file */
    H5PartWriteDataFloat64(file,"x",x); 
    H5PartWriteDataFloat64(file,"y",y);
    H5PartWriteDataFloat64(file,"z",z);

    H5PartWriteDataFloat64(file,"px",x); 
    H5PartWriteDataFloat64(file,"py",y);
    H5PartWriteDataFloat64(file,"pz",z);

    H5PartWriteDataInt64(file,"id",id);
  }

  printf("AllDone p[%u]\n",myproc);
  H5PartCloseFile(file);
  MPI_Barrier(comm);
  
  printf("p[%u:%u] : OK, close file and reopen for reading\n",myproc,nprocs);

  file=H5PartOpenFileParallel("parttest.h5",H5PART_READ,comm);
  H5PartSetStep(file,0);
  unsigned int np,total_np = (int)H5PartGetNumParticles(file);
  nt=H5PartGetNumSteps(file); /* get number of steps in file */
  nds = H5PartGetNumDatasets(file);
  if(myproc==0){
    fprintf(stdout,"steps= %u\tdatasets=%u\tparticles= %u\n",
	    nt,nds,total_np);
  }
  MPI_Barrier(comm);

  /* now lets compute the appropriate idStart and idEnd 
     for this particular processor */

  unsigned h5part_int64_t idStart = sz*myproc;
  unsigned h5part_int64_t idEnd   = (sz-1)+sz*myproc;
  H5PartSetView(file,idStart,idEnd);
  np=H5PartGetNumParticles(file);
  printf("Proc[%u]: View=%u:%u : particles= %u\n",
	 myproc,(int)idStart,(int)idEnd,H5PartGetNumParticles(file));
  /* now lets read them and print some out */
  H5PartReadDataFloat64(file,"x",x); 
  H5PartReadDataFloat64(file,"y",y);
  H5PartReadDataFloat64(file,"z",z);
  H5PartReadDataInt64(file,"id",id);
	printf("Proc[%u]: data values x[first,last]=%f:%f y[%u:%u]=%f:%f z[:]=%f:%f id[:]=%f:%f\n",
		myproc,x[0],x[sz-1],(int)idStart,(int)idEnd,y[0],y[sz-1],z[0],z[sz-1],(int)id[0],(int)id[sz-1]);
  
  /* H5PartCloseFile(file); MPI_Finalize(); exit(0); */

  if(x) 
    free(x); 
  if(y) 
    free(y);
  if(z) 
    free(z);
  if(id) 
    free(id);

  H5PartCloseFile(file);
  MPI_Barrier(comm);
  fprintf(stderr,"proc[%u]:  done\n",myproc);
  return MPI_Finalize();
}

#else
#error This file only works when PARALLEL_IO is enabled.
#endif
