#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <hdf5.h>
#include "H5Part.h"

/*
  A simple regression test that shows how you use this API
  to write and read multi-timestep files of particle data.
*/



int main(int argc,char *argv[]){
	int sz=5;
	double *x,*y,*z;
	h5part_int64_t *id;
	H5PartFile *file;
	int i,t,nt,nds, np;
	int nprocs = 1;
	int myproc = 0;

#ifdef PARALLEL_IO
	MPI_Comm comm=MPI_COMM_WORLD;

	MPI_Init(&argc,&argv);
	MPI_Comm_size(comm,&nprocs);
	MPI_Comm_rank(comm,&myproc);
#endif
	x=(double*)malloc(sz*nprocs*sizeof(double));
	y=(double*)malloc(sz*nprocs*sizeof(double));
	z=(double*)malloc(sz*nprocs*sizeof(double));
	id=(h5part_int64_t*)malloc(sz*nprocs*sizeof(h5part_int64_t));

#ifdef PARALLEL_IO
	file=H5PartOpenFileParallel("parttest.h5",H5PART_WRITE,comm);
#else
	file=H5PartOpenFile("parttest.h5",H5PART_WRITE);
#endif
	if(!file) {
		perror("File open failed:  exiting!");
		exit(0);
	}

	if ( myproc == 0 ) {
		H5PartWriteFileAttribString (
			file,
			"File Description",
			"Simple H5Part file for testing purpose...");
		char* FileAttrib = "Created by H5PartTest.cc";
		H5PartWriteFileAttrib (
			file,
			"Origin",
			H5T_NATIVE_CHAR,
			FileAttrib,
			strlen(FileAttrib));
	}
	for(t=0;t<5;t++){
#ifdef PARALLEL_IO
		MPI_Barrier(comm);
#endif
		fprintf(stdout,"Writing timestep %u\n",t);

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
		if ( myproc == 0 ) {
			H5PartWriteStepAttribString (
				file,
				"Step Description",
				"STEP STEP STEP"
				);
		}

	}

	h5part_int64_t idStart = 0+sz*myproc;
	h5part_int64_t idEnd = (sz-1)+sz*myproc;

	printf("AllDone p[%u]\n",myproc);
	H5PartCloseFile(file);
	fprintf(stderr,"Closed files p[%u]\n",myproc);
#ifdef PARALLEL_IO
	MPI_Barrier(comm);
#endif 
	fprintf ( 
		stderr,
		"p[%u:%u] : OK, close file and reopen for reading idStart %lld  idEnd %lld \n",
		myproc, nprocs, (long long)idStart,
		(long long)idEnd );

#ifdef PARALLEL_IO
	file = H5PartOpenFileParallel ( "parttest.h5", H5PART_READ, comm );
#else
	file = H5PartOpenFile ( "parttest.h5", H5PART_READ );
#endif
	H5PartSetStep(file,0);
	nt = H5PartGetNumSteps(file); /* get number of steps in file */
	nds = H5PartGetNumDatasets(file); /* get number of datasets in timestep 0 */
	np = H5PartGetNumParticles(file);
	
	H5PartSetView(file,idStart,idEnd);
	
	np = (int)H5PartGetNumParticles(file);
	printf("After SetView(%d,%d): steps= %u  datasets= %u particles= %u\n",
	       (int)idStart,(int)idEnd,
	       nt,nds,np);

#ifdef PARALLEL_IO
	MPI_Barrier(comm);
#endif
	// clear the particles
	for(i=0;i<np;i++){
		x[i]=y[i]=z[i]=0.0;
		id[i]=0;
	}

	H5PartReadDataFloat64(file,"x",x);
	H5PartReadDataFloat64(file,"y",y);
	H5PartReadDataFloat64(file,"z",z);
	H5PartReadDataInt64(file,"id",id);

	for(i=0;i<np;i++){
		fprintf(stdout,
			"\tp[%3u] x=%lf y=%lf z=%lf id=%lld\n",
			i,x[i],y[i],z[i],(long long)(id[i]));
	}

	printf("Set to last step and reload data\n");
	H5PartSetStep(file,nt-1);
	H5PartReadDataFloat64(file,"x",x);
	H5PartReadDataFloat64(file,"y",y);
	H5PartReadDataFloat64(file,"z",z);
	H5PartReadDataInt64(file,"id",id);
	for(i=0;i<np;i++){
		fprintf(stdout,"\tp[%3u] x=%lf y=%lf z=%lf id=%lld\n",
			i,x[i],y[i],z[i],(long long) (id[i]));
	}
  
	/********************************************/
	idEnd=np;
	printf("Old View is %d:%d\n",(int)idStart,(int)idEnd);
	H5PartSetView(file,idStart,idEnd>>1);
	printf("Set new view = %d:%d\n",(int)idStart,(int)(idEnd>>1));
	H5PartGetView(file,&idStart,&idEnd);
	np=H5PartGetNumParticles(file);
	printf("steps= %u  datasets= %u particles= %d with view %d:%d\n",
	       nt,nds,(int)np,(int)idStart,(int)idEnd);
	H5PartSetStep(file,nt-1); // set to last step
	printf("Setting to last step = %u\n",nt-1);
	for(i=0;i<10;i++){ x[i]=y[i]=z[i]=0.0; id[i]=0; } /* clear the arrays */
	H5PartReadDataFloat64(file,"x",x);
	H5PartReadDataFloat64(file,"y",y);
	H5PartReadDataFloat64(file,"z",z);
	H5PartReadDataInt64(file,"id",id);
	
	for(i=0;i<np;i++){
		fprintf(stdout,
			"\tp[%3u] x=%lf y=%lf z=%lf id=%lld\n",
			i,x[i],y[i],z[i],(long long)id[i]);
	}

	/********************************************/
	printf("Now set the view to the latter half of the data in step #%u\n",nt-1);
	H5PartResetView(file);
	H5PartGetView(file,&idStart,&idEnd);
	printf("Reset view = %d:%d\nSetting to %u:%u\n",
	       (int)idStart,(int)idEnd,
	       (int)idEnd>>1,(int)idEnd);
	H5PartSetView(file,(idEnd>>1),idEnd);
	np=H5PartGetNumParticles(file);
	printf("Now particles in selection are %d\n",np);
	printf("doubleCheck=%lld\n", (long long)H5PartGetView(file,0,0));
	
	for(i=0;i<10;i++){ x[i]=y[i]=z[i]=0.0; id[i]=0; } /* clear the arrays */
	
	H5PartReadDataFloat64(file,"x",x);
	H5PartReadDataFloat64(file,"y",y);
	H5PartReadDataFloat64(file,"z",z);
	H5PartReadDataInt64(file,"id",id);
	for(i=0;i<np;i++){
		fprintf(stdout,
			"\tp[%3u] x=%lf y=%lf z=%lf id=%lld\n",
			i,x[i],y[i],z[i],(long long)id[i]);
	}
	
	// read dataset names
	h5part_int64_t status = H5PART_SUCCESS;

	H5PartSetStep ( file, 0 );
	for (h5part_int64_t i=0; i < nds; i++) {
		const h5part_int64_t lenName = 64;
		char datasetName[lenName];
		h5part_int64_t datasetType;
		h5part_int64_t datasetNElems;

		status = H5PartGetDatasetInfo(file, i, datasetName, lenName,
					      &datasetType, &datasetNElems);

		if (status != H5PART_SUCCESS) {
			perror("Could not retrieve dataset names!");
		} else {
			printf("datasetName: %s, type: %lld, nElements: %lld   ", 
			       datasetName, datasetType, datasetNElems);
			if (datasetType ==  H5PART_INT64) {
				printf("H5PPART_INT64 \n");
			}
			else {
				printf("H5PPART_FLOAT64 \n");
			}
		}
	}

	if(x) 
		free(x); 
	if(y) 
		free(y);
	if(z) 
		free(z);
	if(id) 
		free(id);
	
	H5PartCloseFile(file);
	fprintf(stderr,"proc[%u]:  done\n",myproc);

#ifdef PARALLEL_IO
	MPI_Barrier(comm);
	return MPI_Finalize();
#else
	return 0;
#endif
}



