#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <hdf5.h>
#include "H5Part.hh"

#include <fstream>
#include <iomanip>
#include <iostream>
#include <string>
using namespace std;

#ifdef READTEST

/*
  A simple regression test that shows how you use this API
  to write and read multi-timestep files of particle data.
*/
#ifdef PARALLEL_IO


#else

int main(int argc,char **argv){
    const int sz=5;
    double x[sz],y[sz],z[sz];
    long long id[sz];
    char name[64];
    H5PartFile *file;
    int i,t,nt,nds,myproc;
    int nfattribs,nsattribs;

    const string fn = string(argv[1]);

    cout << "Open " << fn << endl;


    file= H5PartOpenFile(fn.c_str(),H5PART_READ);
    nt=H5PartGetNumSteps(file); /* get number of steps in file */
    H5PartSetStep(file,0);
    nds=H5PartGetNumDatasets(file); /* get number of datasets in timestep 0 */



    puts("\n\n===============================");
    for(i=0;i<nds;i++){ /* and print out those names */
        H5PartGetDatasetName(file,i,name,64);
        printf("\tDataset[%u] name=[%s]\n",
            i,name);
    }
    puts("===============================\n\n");

    nfattribs=H5PartGetNumFileAttribs(file);
    printf("Number of datasteps in file is %u num file attribs=%d\n",
        nt,nfattribs);

    H5PartCloseFile(file);
}
#endif

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
  long long *id;
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
  id=(long long*)malloc(sz*nprocs*sizeof(long long));
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
    H5PartWriteDataInt64(file,"id",id);
  }
  printf("AllDone p[%u]\n",myproc);
  H5PartCloseFile(file);
    MPI_Barrier(comm);
  printf("p[%u:%u] : OK, close file and reopen for reading \n",myproc,nprocs);
  if(myproc==0){ /* now only proc 0 reads the file serially */
    file= H5PartOpenFileSerial("parttest.h5",H5PART_READ);
    nt=H5PartGetNumSteps(file); /* get number of steps in file */
    H5PartSetStep(file,0);
    nds=H5PartGetNumDatasets(file); /* get number of datasets in timestep 0 */

    puts("\n\n===============================");
    for(i=0;i<nds;i++){ /* and print out those names */
      H5PartGetDatasetName(file,i,name,64);
      printf("\tDataset[%u] name=[%s]\n",
	     i,name);
    }
    puts("===============================\n\n");
    printf("Number of datasteps in file is %u\n",nt);
    for(t=0;t<nt;t++){
      int nparticles;
      H5PartSetStep(file,t); /* select a timestep */
      nparticles=(int)H5PartGetNumParticles(file);
      printf("Step[%u] nparticles this step=%u\n",
	     t,nparticles); /* get num particles this step */
      H5PartReadParticleStep(file,t, /* do a mongo read of all data this step */
			     x,y,z,x,y,z,id);
      printf("\tid\t\tx\t\ty\t\tz\n");
      puts("\t----------------------------------------------------");
      for(i=0;i<nparticles;i++) {
	printf("\t%llu\t%f\t%f\t%f\n\n",id[i],x[i],y[i],z[i]);
      }
    }
    H5PartCloseFile(file);
  }
  if(x) free(x); 
  if(y) free(y);
  if(z) free(z);
  if(id) free(id);
  MPI_Barrier(comm);
  fprintf(stderr,"proc[%u]:  done\n",myproc);
  return MPI_Finalize();
}

#else

  int main(int argc,char *argv){
    const int sz=5;
    double x[sz],y[sz],z[sz];
  long long id[sz];
  char name[64];
  H5PartFile *file;
  int i,t,nt,nds,myproc;
  int nfattribs,nsattribs;

  file=H5PartOpenFile("parttest.h5",H5PART_WRITE);
  if(!file) {
    perror("File open failed:  exiting!");
    exit(0);
  }
  for(t=0;t<5;t++){
    long long step=t;
    printf("Writing timestep %u\n",t);
    for(i=0;i<sz;i++) {
      x[i]=(double)(i+t);
      y[i]=0.1 + (double)(i+t);
      z[i]=0.2 + (double)(i+t);
      id[i]=i;
    }
    H5PartSetStep(file,t); /* must set the current timestep in file */
    H5PartSetNumParticles(file,sz); /* then set number of particles to store */
    /* now write different tuples of data into this timestep of the file */
    H5PartWriteDataFloat64(file,"x",x); 
    H5PartWriteDataFloat64(file,"y",y);
    H5PartWriteDataFloat64(file,"z",z);
    H5PartWriteDataInt64(file,"id",id);
    H5PartWriteStepAttrib(file,"Step",H5T_NATIVE_INT64,&step,1);
  }
  H5PartCloseFile(file);
  printf("OK, close file and reopen for reading\n");
  file= H5PartOpenFile("parttest.h5",H5PART_READ);
  nt=H5PartGetNumSteps(file); /* get number of steps in file */
  H5PartSetStep(file,0);
  nds=H5PartGetNumDatasets(file); /* get number of datasets in timestep 0 */

  puts("\n\n===============================");
  for(i=0;i<nds;i++){ /* and print out those names */
    H5PartGetDatasetName(file,i,name,64);
    printf("\tDataset[%u] name=[%s]\n",
	   i,name);
  }
  puts("===============================\n\n");

  nfattribs=H5PartGetNumFileAttribs(file);
  printf("Number of datasteps in file is %u num file attribs=%d\n",
	 nt,nfattribs);
  for(t=0;t<nt;t++){
    int nparticles;
    H5PartSetStep(file,t); /* select a timestep */
    nparticles=(int)H5PartGetNumParticles(file);
    nsattribs=H5PartGetNumStepAttribs(file);
    printf("Step[%u] nparticles this step=%u stepattribs=%u\n",
	   t,nparticles,nsattribs); /* get num particles this step */
    if(nsattribs>0){
      char attrname[32];
      H5PartGetStepAttribInfo(file,0,attrname,32,0,0);
      printf("First Attrib name is [%s]\n",attrname);
    }
    H5PartReadParticleStep(file,t,/* do a mongo read of all data this step */
			   x,y,z,x,y,z,id);
    printf("\tid\t\tx\t\ty\t\tz\n");
    puts("\t----------------------------------------------------");
    for(i=0;i<sz;i++) {
      printf("\t%llu\t%f\t%f\t%f\n\n",id[i],x[i],y[i],z[i]);
    }
  }
  H5PartCloseFile(file);
}
#endif

#endif
