/*
  Read Homdyn Hxxx.DAT and convert it to the H5Part data
  format.

  g++  -I/Users/adelmann/install/hdf5-1.6.5/hdf5/include -I/Users/adelmann/svnwork/H5Part/src  -g -c homdynToH5p.cc
  g++ -o homdynToH5p homdynToH5p.o -L/Users/adelmann/svnwork/H5Part/src -lH5Part -L/Users/adelmann/install/hdf5-1.6.5/hdf5/lib -lhdf5 -lz   -lm


  Usage: homdynToH5p [-f newFilename]

  Reads HBUNCH.OUT and writes the data to HBUNCH.h5 or newFilename.h5 



*/

#include "H5Part.h"
#include <iostream>
#include <fstream>
#include <string>

using namespace std;

int main(int argc,char *argv[]){

  const int nCol = 25;
  const int nHeader = 1;
  int nLines  = 0;

  H5PartFile *file;

  double data[nCol][10000];
  string headers[nCol];

  string fnStr("HBUNCH.OUT");

  ifstream in;    

  /*
  Open and read HOMDYN File:
  */

  in.open(fnStr.c_str());	

  /* 
    over read possible header
  */

  for (int l=0;l<nHeader;l++) {
    for (int c=0;c<nCol;c++) {	
     in >> headers[c];
     cout << c << " - - " << headers[c] << endl;
    }
   }
  headers[0] = string("SPOS"); // H5Root needs this name 
   /* 
     read in file data
   */

  while (1) {    
   for (int c=0;c<nCol;c++)
     in >> data[c][nLines];
   if (!in.good()) break;      
     nLines++;   
  }   
  
  in.close();

  cout  << "In HBUNCH.OUT found " << nLines << " lines " << endl;

  file=H5PartOpenFile("HBUNCH.h5",H5PART_WRITE);
  if(!file) {
    perror("File open failed:  exiting!");
    exit(0);
  }

  H5PartWriteFileAttribString(file,"File Description", "This file contains HOMDYN HBUNCH.OUT data");
  
  for (int c=0;c<nCol;c++)	
   int rc = H5PartWriteFileAttribString(file,(headers[c]+string("Unit")).c_str(), " ");	;

  for (int t=0;t<nLines;t++)	{
   H5PartSetStep(file,t); /* must set the current timestep in file */
   for (int c=0;c<nCol;c++) {
    int rc = H5PartWriteStepAttrib(file,headers[c].c_str(), H5T_NATIVE_DOUBLE,&data[c][t],1);
   }	
  }
  H5PartCloseFile(file);
} 

