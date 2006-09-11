#ifndef _H5Part_HH_
#define _H5Part_HH_

extern "C" {
#include "H5Part.h"
#ifdef PARALLEL_IO
#include <mpi.h>
#endif
}

/* Add any C++ specific extensions/implementations/wrappers here */

/* here is a sample class wrapping of H5Part for C++ usage */

class H5ParticleFile {
  H5PartFile *file;
  long long np;
public:
  H5ParticleFile(const char *filename,const int flags):np(0){
    this->file=H5PartOpenFile(filename,flags);
  }

  //  H5ParticleFile(const std::string fn,const int flags):np(0){
  //  this->file=H5PartOpenFile(fn.cstr(),flags);
  //}

#ifdef PARALLEL_IO  
  H5ParticleFile(const char *filename,const int flags,const MPI_Comm comm):np(0){
    this->file=H5PartOpenFileParallel(filename,flags,comm);
  }
  //  H5ParticleFile(const string fn,const int flags,const MPI_Comm comm):np(0){
  //  this->file=H5PartOpenFile(fn.cstr(),flags,comm);
  //}
#endif
  inline int isValid() { return H5PartFileIsValid(this->file);}

  ~H5ParticleFile(){H5PartCloseFile(this->file);}
  
  inline void setNumParticles(long long nparticles){
    np=nparticles;
    H5PartSetNumParticles(file,nparticles);
  }
  // get the current step (-1 means step is invalid)
  inline int step(){ return file->timestep; }
  // set the current step
  inline int step(int s){ H5PartSetStep(this->file,s); return file->timestep; }
  inline int nSteps(){ return H5PartGetNumSteps(this->file);}
  inline int nFields(){ return H5PartGetNumDatasets(file);}
  inline int fieldName(int index,char *name,int maxlen){H5PartGetDatasetName(file,index,name,maxlen);}
  inline long long nParticles(){return H5PartGetNumParticles(file);}
  inline int write(char *name,double *array){
    return H5PartWriteDataFloat64(this->file,name,array);
  }
  inline int write(char *name,long long *array){
    return H5PartWriteDataInt64(this->file,name,array);
  }
  inline int read(char *name,double *array){
    return H5PartReadDataFloat64(this->file,name,array);
  }
  inline int read(char *name,long long *array){
    return H5PartReadDataInt64(this->file,name,array);
  }
  inline int readStep(int step,
	       double *x,double *y,double *z,
	       double *px,double *py,double *pz,
	       long long *id){
    return H5PartReadParticleStep(file,step,
				  x,y,z,
				  px,py,pz,
				  id);
  }

  // Attribute stuff
  // Info on attributes
  inline int nStepAttribs(){ return H5PartGetNumStepAttribs(file);}
  inline void getStepAttribInfo(int idx,char *name,size_t maxsize,
			 hid_t &type,int &nelem){
    H5PartGetStepAttribInfo(file,idx,name,maxsize,&type,&nelem);
  }
  inline int nFileAttribs(){ return H5PartGetNumFileAttribs(file);}
  inline void getFileAttribInfo(int idx,char *name,size_t maxsize,
			 hid_t &type,int &nelem){
    H5PartGetFileAttribInfo(file,idx,name,maxsize,&type,&nelem);
  }
  // step attribs
  // int writeStepAttrib(char *key,string *valuestring){
  //   return H5PartWriteStepAttribString(file,key,valuestring.c_str());
  //}
  inline int writeStepAttrib(char *key,char *valuestring){
    return H5PartWriteStepAttribString(file,key,valuestring);
  }
  inline int writeStepAttrib(char *key,double *value,int nelem=1){
    return H5PartWriteStepAttrib(file,key,H5T_NATIVE_DOUBLE,value,nelem);
  }
  inline int writeStepAttrib(char *key,float *value,int nelem=1){
    return H5PartWriteStepAttrib(file,key,H5T_NATIVE_FLOAT,value,nelem);}
  inline int writeStepAttrib(char *key,int *value,int nelem=1){
    return H5PartWriteStepAttrib(file,key,H5T_NATIVE_INT,value,nelem);}
  inline int writeStepAttrib(char *key,long long *value,int nelem=1){
    return H5PartWriteStepAttrib(file,key,H5T_NATIVE_INT64,value,nelem);}
  inline void readStepAttrib(char *key,void *value){
    H5PartReadStepAttrib(file,key,value);
  }
  // FileAttribs
    // Attribute stuff
  //  int writeFileAttrib(char *key,string *valuestring){
  //  return H5PartWriteFileAttribString(file,key,valuestring.c_str());
  //}
  inline int writeFileAttrib(char *key,char *valuestring){
    return H5PartWriteFileAttribString(file,key,valuestring);
  }
  inline int writeFileAttrib(char *key,double *value,int nelem=1){
    return H5PartWriteFileAttrib(file,key,H5T_NATIVE_DOUBLE,value,nelem);
  }
  inline int writeFileAttrib(char *key,float *value,int nelem=1){
    return H5PartWriteFileAttrib(file,key,H5T_NATIVE_FLOAT,value,nelem);}
  inline int writeFileAttrib(char *key,int *value,int nelem=1){
    return H5PartWriteFileAttrib(file,key,H5T_NATIVE_INT,value,nelem);}
  inline int writeFileAttrib(char *key,long long *value,int nelem=1){
    return H5PartWriteFileAttrib(file,key,H5T_NATIVE_INT64,value,nelem);}  
  // read file attribs
  inline int readFileAttrib(char *key,void *valuestring){
    H5PartReadFileAttrib(file,key,valuestring);
  }
};

/*
  An even wackier idea
  file.datasets[0:ndatasets-1]
  file.attributes[0:nfileattribs-1]
  file.datasets.attributes[0:ndatasetattribs-1]
  file.datasets.size()
  
  operators
  .size()
  .name()
  [idx]
  ["name" or "keyname"]
  
*/
#if 0

class H5Dataset {
  const hid_t dataset;
  char *n; // return as const
  int d[5]; // simple start
  int nd;
public:
  H5Dataset(const hid_t ds_handle):dataset(ds_handle){}
  const char *name() {return n;}
  int nDims(){ return nd; }
  const int *dims(){return d;}
};

class H5Attribute{
  char *n;
  int sz;
public:
  const char *name(){return n;}
  int size(){return sz;}
}


class H5Group {
  hid_t gid;
  int sz;
protected:
  virtual int computesize(){}
  void setGroup(gid_t g){
    if(gid>0) H5close(gid);
    gid=g;
    // must compute size if available
    if(gid>0) computesize();
    else sz=0;
  }
public:
    // const hid_t gid;
  H5Group(hid_t g):gid(g){}
  H5Group():gid(-1){}
  ~H5Group(){if(gid>0) H5Gclose(gid);}
  int size(){return sz;}
};

class H5AttribGroup : public H5Group{
  int size;
  virtual void computesize(){
    // compute the number of items in this group
  }
public:
  const H5Attribute &operator[](char *name);
  const H5Attribute &operator[](int idx);
};

class H5DataGroup : public H5Group {
  int size;
public:
  const H5Dataset &operator[](int idx);
  const H5Dataset &operator[](char *name);
  // no name here??
};

class H5FancyParticles {
  char *n;
  hid_t file;
public:
  hid_t mygroup;
  const H5AttribGroup attributes;
  const H5DataGroup datasets;
  H5FancyParticles(char *filename,int readwriteflag);
  ~H5FancyParticles();
  // int size();// return the number of datasets in file
  // const char *name(); // return the name of the file?
  // const H5Dataset &operator[](int idx); // index dataset by integer
  // const H5Dataset &operator[](char *n); // index dataset by name
};

#endif

#endif
