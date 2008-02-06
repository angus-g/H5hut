#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <hdf5.h>
#include "H5Part.hh"

#ifdef READTEST

#endif

#ifdef REGRESSIONTEST

/*
  A simple regression test that shows how you use this API
  to write and read multi-timestep files of particle data.
*/
#ifdef PARALLEL_IO

int main(int argc,char *argv[]){
  int sz=5;
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
    printf("Proc[%u] Writing timestep %u file=%u\n",myproc,t,file->file);
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

  unsigned int idStart = 0+sz*myproc;
  unsigned int idEnd = (sz-1)+sz*myproc;

  printf("AllDone p[%u]\n",myproc);
  H5PartCloseFile(file);
  MPI_Barrier(comm);
  
  printf("p[%u:%u] : OK, close file and reopen for reading idStart %u  idEnd %u \n",myproc,nprocs,idStart,idEnd);

  file=H5PartOpenFileParallel("parttest.h5",H5PART_READ,comm);
  H5PartSetStep(file,0);
  unsigned int np = 0;
  // unsigned int np = (int)H5PartGetNumParticles(file);
  // nt=H5PartGetNumSteps(file); /* get number of steps in file */
  
  //nds=H5PartGetNumDatasets(file); /* get number of datasets in timestep 0 */
  MPI_Barrier(comm);
  
  //  H5PartSetView(file,idStart,idEnd);

  printf("steps= %u  datasets= %u particles= %u\n",nt,nds,np);

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

#endif

#endif


