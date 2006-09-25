// rights    - 2006-, copyright by 
//                    benedikt oswald and patrick leidenberger,
//                    all rights reserved
// project   - h5feddump
// file name - h5feddump.hh
// file type - c++ header file
// objective - header file for the h5feddump
// modified  - 2006 sep 21, creation, Patrick Leidenberger
// modified  - 2006 sep 22, pl, add dump for coordinates.
// modified  - 2006 sep 25, pl, chage h5fed -> hdf5fed.

#ifndef HDF5FEDDUMP_HH_
#define HDF5FEDDUMP_HH_

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

// Include the Hdf5FiniteElementData API.
#ifdef HAVE_HDF5
  // Include the Hdf5FiniteElementData API.
  #include <hdf5fed.hh>
  // Include h5fed specific constants.
  #include <hdf5fedconst.hh>
#else
  #warning No hdf5 lib found!!
#endif

using namespace physicomath;
using namespace nonsciconst;
//using namespace H5Fed;
//using namespace gmshtohdf5fed;

#endif /* HDF5FEDDUMP_HH_ */
