#include <stdio.h>
#include <stdlib.h>
#include <hdf5.h>
#include "H5Part.h"

#include <fstream>
#include <iomanip>
#include <iostream>
#include <string>
using namespace std;


/*
  A simple regression test that shows how you use this API
  to write and read multi-timestep files of particle data.
*/

int ReadFile(const string fn){
  char name[64];
  h5_file_t *file;
  int i,nt,nds;
  cout << "Open " << fn << endl;
  
  file= H5PartOpenFile(fn.c_str(),H5_O_RDONLY);
  
  nt=H5GetNumSteps(file);
  H5PartSetStep(file,0);
  nds=H5PartGetNumDatasets(file);
  
  cout << "Timesteps = " << nt << " dataSets per timestep = " << nds << endl;
  
  cout << endl << endl << "===============================" << endl;
  for(i=0;i<nds;i++){ 
    H5PartGetDatasetName(file,i,name,64);
    printf("\tDataset[%u] name=[%s]\n",
	   i,name);
  }
  cout << "===============================" << endl << endl;;
  
  for (int steps=0; steps<nt; steps++) {
    H5PartSetStep(file,steps);
    h5_int64_t n = H5PartGetNumParticles(file);
    cout << "number of particles this step =" << n << endl;
    double *x=new double[n];
    double *y=new double[n];
    double *z=new double[n];
    double *px=new double[n];
    double *py=new double[n];
    double *pz=new double[n];
    h5_int64_t *id=new h5_int64_t[n];
    
    H5PartReadParticleStep(file,steps,x,y,z,px,py,pz,id);
    
    double sumx = 0.0;
    double sumpz = 0.0;
    for (h5_int64_t i=0; i<n; i++) {    
      sumx += x[i];
      sumpz += pz[i];
    }
    
    cout << "\tstep= " << steps << " sum(x)= " << sumx << " sum(pz)= " << sumpz << endl;
    cout << "\tfirst x is " << x[0] << "\tlast x is " << x[n-1] << endl;
    cout << "\tFor fake data, expect sumx to be =" << x[0]*((double)n)<<endl;
    delete x;
    delete y;
    delete z;
    delete px;
    delete py;
    delete pz;
    delete id;
  }
  H5PartCloseFile(file);
  return 1;
}

int WriteFile(const string fn){
  h5_file_t *file;
  int i,t;
  h5_int64_t n;
  const int nt = 5;
  const h5_int64_t np = 1024*1024;
  cout << "Open " << fn << endl;
  
  file= H5PartOpenFile(fn.c_str(),H5_O_WRONLY);
  
  double *x=new double[np];
  double *y=new double[np];
  double *z=new double[np];
  double *px=new double[np];
  double *py=new double[np];
  double *pz=new double[np];
  h5_int64_t *id=new h5_int64_t[np];

  H5PartSetNumParticles(file,np); // sets number of particles in simulation
  
  for(n=0;n<np;n++) {
    id[n]=i;
    x[n]=1.0;
    y[n]=2.0;
    z[n]=3.0;
    px[n]=1.0*((double)i)*((double)(i%10));
    py[n]=2.0*((double)i)*((double)(i%10));
    pz[n]=3.0*((double)i)*((double)(i%10));
  }
  
  for(t=0;t<nt;t++){
    // setup the step number
    H5PartSetStep(file,t);
    printf("Write Step %u\n",t);
    // write fake data
    H5PartWriteDataFloat64(file,"x",x);
    H5PartWriteDataFloat64(file,"y",y);
    H5PartWriteDataFloat64(file,"z",z);
    H5PartWriteDataFloat64(file,"px",px);
    H5PartWriteDataFloat64(file,"py",py);
    H5PartWriteDataFloat64(file,"pz",pz);
    H5PartWriteDataInt64(file,"id",id);
  }
  
  H5PartCloseFile(file);
  return 1;
}

int main(int argc,char **argv){
  char *str;
  char dstr[]="testfile.h5";

  if(argc>1) str=argv[1];
  else str=dstr;
  const string fn = string(str);
  /* f=fopen(fn.c_str(),"r");
  if(f!=NULL) {  a poor-man's stat()
    fclose(f);
  */
      if(!WriteFile(fn)){
	cerr << "Failed to write file " << fn << endl;
	exit(0);
      }
      /*
  }
  else {
    cout << "File " << fn << " already exists, so we will proceed to reading" << endl;
    } */
  if(!ReadFile(fn)){
    cerr << "Failed to read file " << fn << endl;
  }
}
