#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <mpi.h>
/* #include <mpio.h> */
#include <unistd.h>
#include <sys/types.h>
#ifndef PARALLEL_IO
#define PARALLEL_IO
#endif

#ifndef DISABLE_H5PART
#include "H5Part.h"
#endif

#define FILENAME "testio"
/* normally 64 steps for real benchmark */
#define NSTEPS 5 
/* normally 51e6 for real benchmark */
#define NPARTICLES 51e4
#define NTRIALS 3

/*

bench <nParticles>


*/


int main(int argc,char *argv[]){
  printf("Start benchmarks...\n");
  MPI_Info info;
  int nprocs,rank;
  int trial;
  int i,j,n; /* iteration variables */
  double starttime,curtime, endtime;

  int nparticles = atoi(argv[1]);

  double *x,*y,*z,*px,*py,*pz;
  typedef double *ddouble;
  ddouble data[6];
  MPI_Datatype chunktype;
  int offset;
  int localnp;
  char filename[128]; /*= FILENAME; */
#ifndef DISABLE_H5PART
  H5PartFile *f;
#endif
  char newfilename[128];
  char lastfilename[128];
  FILE *fd;
  MPI_File file;
  MPI_Offset foffset;

  MPI_Comm dcomm = MPI_COMM_WORLD;
  
  MPI_Init(&argc,&argv);
  MPI_Comm_rank(dcomm,&rank);
  MPI_Comm_size(dcomm,&nprocs);

  localnp=nparticles/(int64_t)nprocs;
  for(offset=0,i=0;i<rank;i++){
    offset+=localnp;
  }
  
  data[0]=x=(double*)malloc(sizeof(double)*(size_t)localnp);
  data[1]=y=(double*)malloc(sizeof(double)*(size_t)localnp);
  data[2]=z=(double*)malloc(sizeof(double)*(size_t)localnp);
  data[3]=px=(double*)malloc(sizeof(double)*(size_t)localnp);
  data[4]=py=(double*)malloc(sizeof(double)*(size_t)localnp);
  data[5]=pz=(double*)malloc(sizeof(double)*(size_t)localnp);



  
  /* printf("about to call create subarray with nparticles=%u localnp=%u offset=%u\n",
     nparticles,localnp,offset); */
  MPI_Type_create_subarray(1, /* rank */
			   &nparticles, /* size of the global array */
			   &localnp, /* size of my local chunk */
			   &offset, /* offset of this chunk in global */
			   MPI_ORDER_FORTRAN, /* fortran storage order */
			   MPI_DOUBLE,
			   &chunktype);
  MPI_Type_commit(&chunktype);
  MPI_Info_create(&info);

  if(rank==0) printf("Nprocs=%u Particles=%u*6attribs*sizeof(double) Particles/proc=%u Nsteps=%u Ntrials=%u\n",
		     nprocs,nparticles,localnp,NSTEPS,NTRIALS);
	

  for(trial=0;trial<NTRIALS;trial++){
    if(rank==0) printf("---------------------- Trial %u of %u ---------------------\n",trial+1,NTRIALS);


    MPI_Barrier(MPI_COMM_WORLD); /* to prevent unlink from interfering with file open */
    sprintf(filename,"%s.%u.mpio.dat",FILENAME,nprocs);
    sprintf(lastfilename,"%s.%u.mpio.dat",FILENAME,nprocs);

    if(rank==0) unlink(filename);
    MPI_Barrier(MPI_COMM_WORLD); /* to prevent unlink from interfering with file open */

    MPI_File_open(MPI_COMM_WORLD,filename,
		  MPI_MODE_CREATE | MPI_MODE_RDWR,
		  info,&file);
	
    MPI_File_set_view(file,0,MPI_DOUBLE,chunktype,"native",info);
    /* now a barrier to get the start timers roughly synced*/
    MPI_Barrier(MPI_COMM_WORLD);
    curtime = starttime = MPI_Wtime();
    endtime = starttime+5.0*60.0; /* end in 5 minutes */
    MPI_Bcast(&endtime,1,MPI_DOUBLE,0,MPI_COMM_WORLD);
    /* must touch the entire array after each write */
    /* ensures cache-invalidation */
    foffset=0;
    i=0;
    curtime=starttime;
    for(i=0;i<NSTEPS;i++){
      int n;
      MPI_Status status;
      for(j=0;j<6;j++){
	/* touch data */
	for(n=0;n<localnp;n++)
	  (data[j])[n]=(double)rank;
	/* write to that file */
	/*  MPI_File_set_view(file,foffset,MPI_DOUBLE,chunktype,"native",info);*/
	MPI_File_write_at_all(file,
			      foffset,
			      data[j],
			      localnp,
			      MPI_DOUBLE,&status);
	foffset+=nparticles/nprocs;
      }
      curtime=MPI_Wtime(); /* ensure no race condition by broadcasting time */
      MPI_Bcast(&curtime,1,MPI_DOUBLE,0,MPI_COMM_WORLD);
    }
    MPI_File_close(&file);
    MPI_Barrier(MPI_COMM_WORLD);
    endtime=MPI_Wtime();
    sprintf(filename,"%s.%u.h5.dat",FILENAME,nprocs);
    /* foffset*=nprocs; if we want total megabytes written */
    if(rank==0){
      puts("*");
      unlink(filename);
      puts("======================================================");
      printf("Raw MPI-IO Total Duration %lf seconds, iterations=%u %lf Megabytes written per processor Nprocs= %u \n",
	     (endtime-starttime),i,((double)foffset)/(1024.0*1024.0),nprocs);
      printf("Raw MPI-IO Effective Data Rate = %lf Megabytes/sec global and %lf Megabytes/sec per task Nprocs= %u \n",
	     (double)(nprocs*localnp*sizeof(double))*((double)NSTEPS)*6.0/((endtime-starttime)*1024.0*1024.0),
	     (double)(localnp*sizeof(double))*((double)NSTEPS)*6.0/((endtime-starttime)*1024.0*1024.0),nprocs);
      puts("======================================================");
    }

    MPI_Barrier(MPI_COMM_WORLD); /* to prevent unlink from interfering with file open */
    /* OK, now we do this using H5Part */
    sprintf(newfilename,"testio%u.%u.dat",rank,nprocs);
    unlink(newfilename);
    MPI_Barrier(MPI_COMM_WORLD); /* to prevent unlink from interfering with file open */
    fd = fopen(newfilename,"w");
    /* start the timer */
    starttime=endtime=MPI_Wtime();
    for(i=0;i<NSTEPS;i++){
      for(j=0;j<6;j++){
	/* touch data */
	for(n=0;n<localnp;n++)
	  (data[j])[n]=(double)rank;
	fwrite(data[j],sizeof(double),localnp,fd);
      }
      curtime=MPI_Wtime(); /* ensure no race condition by broadcasting time */
      MPI_Bcast(&curtime,1,MPI_DOUBLE,0,MPI_COMM_WORLD);
    }
    fclose(fd);
    MPI_Barrier(MPI_COMM_WORLD);
    endtime=MPI_Wtime();
    if(rank==0) puts("*");
    MPI_Barrier(MPI_COMM_WORLD); /* to prevent unlink from interfering with file open */
    unlink(newfilename);
    MPI_Barrier(MPI_COMM_WORLD);
    if(rank==0){
      puts("======================================================");
      printf("Raw 1-file-per-proc Total Duration %lf seconds, iterations=%u %lf Megabytes written Nprocs= %u \n",
	     (endtime-starttime),NSTEPS,((double)foffset)/(1024.0*1024.0),nprocs);
      printf("Raw 1-file-per-proc Effective Data Rate = %lf Megabytes/sec global and %lf Megabytes/sec per task Nprocs= %u \n",
	     (double)(nprocs*localnp*sizeof(double))*((double)NSTEPS)*6.0/((endtime-starttime)*1024.0*1024.0),
	     (double)(localnp*sizeof(double))*((double)NSTEPS)*6.0/((endtime-starttime)*1024.0*1024.0),nprocs);
      puts("======================================================");
    }

#ifndef DISABLE_H5PART
    //printf("H5Part benchmark...");
    MPI_Barrier(MPI_COMM_WORLD); /* to prevent unlink from interfering with file open */
    /* OK, now we do this using H5Part */
    f = H5PartOpenFileParallel(filename,H5PART_WRITE,MPI_COMM_WORLD);
    MPI_Barrier(MPI_COMM_WORLD); /* to prevent unlink from interfering with file open */
    /* start the timer */
    starttime=endtime=MPI_Wtime();
    H5PartSetNumParticles(f,localnp);
    for(i=0;i<NSTEPS;i++){
      for(j=0;j<6;j++){
	/* touch data */
	for(n=0;n<localnp;n++)
	  (data[j])[n]=(double)rank;
      }
      H5PartSetStep(f,i);
      H5PartWriteDataFloat64(f,"x",x);
      H5PartWriteDataFloat64(f,"y",y);
      H5PartWriteDataFloat64(f,"z",z);
      H5PartWriteDataFloat64(f,"px",px);
      H5PartWriteDataFloat64(f,"py",py);
      H5PartWriteDataFloat64(f,"pz",pz);

      curtime=MPI_Wtime(); /* ensure no race condition by broadcasting time */
      MPI_Bcast(&curtime,1,MPI_DOUBLE,0,MPI_COMM_WORLD);
    }
    H5PartCloseFile(f);
    MPI_Barrier(MPI_COMM_WORLD);
    endtime=MPI_Wtime();
    if(rank==0){
      puts("*");
      unlink(filename);
      puts("======================================================");
      printf("H5Part Total Duration %lf seconds, iterations=%u %lf Megabytes written Nprocs= %u \n",
	     (endtime-starttime),NSTEPS,((double)foffset)/(1024.0*1024.0),nprocs);
      printf("H5Part Effective Data Rate = %lf Megabytes/sec global and %lf Megabytes/sec per task Nprocs= %u \n",
	     (double)(nprocs*localnp*sizeof(double))*((double)NSTEPS)*6.0/((endtime-starttime)*1024.0*1024.0),
	     (double)(localnp*sizeof(double))*((double)NSTEPS)*6.0/((endtime-starttime)*1024.0*1024.0),nprocs);
      puts("======================================================");
    }
    MPI_Barrier(MPI_COMM_WORLD);
#endif
  } /* trials */

  if (rank==0) {
    printf("lastfilename: %s \n", lastfilename);
    unlink(lastfilename);
  }

  MPI_Finalize();


  return 0;
}
