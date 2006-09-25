// rights - 2006-, copyright benedikt oswald and patrick leidenberger, all rights reserved
// project - phidias3d
// file name - phidias3d.h
// file type - c++ header file
// objective - header file for the phidias3d visualization postprocessor
// modified - 2006 jun 26, creation, benedikt oswald
// modified - 2006 jun 28, 
// modified - 2006 jul 30, Add include for command line argument parser
//                         with boost::program_options, pl.
// inheritance - 
// feature - implements the phidias3d visualization postprocessor; the features
// feature - will be (1) read VTK legacy formatted files (2) read HDF5/ELECTROMAGNETIC
// feature - structures files (3) export VTK legacy formatted files (4) export
// feature - data, both meshes and fields, throught the Renderman Interface
// feature - routines.
// required software - 


/* include standard header files */
#include <cmath>
#include <complex>
#include <string>
#include <vector>
#include <iostream>
#include <iterator>
#include <math.h>

/* Include the files for rlog. */
#include <rlog/rlog.h>
#include <rlog/rloglocation.h>
#include <rlog/Error.h>
#include <rlog/RLogChannel.h>
#include <rlog/StdioNode.h>
// Include this if you want to log the time.
#include <rlog/RLogTime.h>

// Include the boost program program_options to parse the comand line
// options.
#include <boost/program_options.hpp>


/* include standard proprietary header files */
#include <nonsciconst.h>
#include <physicomath.h>
//#include <ristream.h>
#include <gmsh.hh>

// Include the Hdf5FiniteElementData API.
#ifdef HAVE_HDF5
  // Include the Hdf5FiniteElementData API.
  #include <h5fed.hh>
  // Include h5fed specific constants.
  #include <h5fedconst.hh>
#else
  #warning No hdf5 lib found!!
#endif



#ifndef PHIDIAS3D_H_
#define PHIDIAS3D_H_

using namespace physicomath;
using namespace nonsciconst;
using namespace gmshtohdf5fed;



#endif /*PHIDIAS3D_H_*/
