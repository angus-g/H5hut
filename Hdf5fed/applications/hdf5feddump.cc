// rights - 2006-, copyright patrick leidenberger and benedikt oswald,
//                 all rights reserved
// project   - hdf5feddump
// file name - hdf5feddump.cc
// file type - c++ implementaton file
// objective - main file hdf5fed dump program
// modified  - 2006 sep 21, creation, patrick leidenberger.
// modified  - 2006 sep 22, pl, add dump for coordinates.
//
// feature - Implements the a dump tool for h5fed files.
// feature - It will read hf5ed file and write the data to the standard 
// feature - output.
// feature - There are different options on command line what to dump.
// required software - rlog library, boost library 

#include <hdf5feddump.hh>

using namespace rlog;

int main(int argc, char **argv)
{
  //=================//
  // Initialize RLog //
  //=================//
  // Make a instance of the class for RLog.
  StdioNode stdLog;

  /** Subscchar* inFile_;ribe output channels.
   * Compile with -DRLOG_DEBUG to get the debug output.
   **/
  #ifdef USE_RLOG_DEBUG_CHANNEL
    stdLog.subscribeTo(GetGlobalChannel("debug"));
  #endif //USE_RLOG_DEBUG_CHANNEL
  #ifdef USE_RLOG_ERROR_CHANNEL
    stdLog.subscribeTo(GetGlobalChannel("error"));
  #endif //USE_RLOG_ERROR_CHANNEL
  #ifdef USE_RLOG_INFO_CHANNEL
    stdLog.subscribeTo(GetGlobalChannel("info"));
  #endif //USE_RLOG_INFO_CHANNEL
  #ifdef USE_RLOG_WARNING_CHANNEL
    stdLog.subscribeTo(GetGlobalChannel("warning"));
  #endif //USE_RLOG_WARNING_CHANNEL

  // Define variables that hold the command line parameters.
  string hdf5fedFile;
 
  //==================================================//
  // Parse the comand line options                    //
  // with the program_options from the boost library. //
  //==================================================//
  try
  {
    // Define and instance of the program_options class and name it.
    boost::program_options::options_description 
      desc("Allowed program options");
    // Define the command line options parsing rules.
    desc.add_options()
      ("help", "produce this help")
      ("input-file", boost::program_options::value<string>(),
       "hdf5fed file to dump");
    // Parse the command line.
    boost::program_options::variables_map varMap;
    boost::program_options::store
      (boost::program_options::parse_command_line(argc, argv, desc),
       varMap);
    boost::program_options::notify(varMap);
    // Action in relation to the command line options.
    if (varMap.count("help"))
    {
      cout << desc << "\n";
      return ERRORCODE;
    }
    else if (varMap.count("input-file")) 
    {
      hdf5fedFile = varMap["input-file"].as<string>();
      rInfo("Input filename: %s",
            hdf5fedFile.c_str());
    }    
    else
    {
      rError("You have insert wrong options.");
      rError("For details use: --help.");
      return ERRORCODE;
    }    
  }
  catch(exception& error)
  {
    rError("Error: %d",error.what());
    return ERRORCODE;
  }  

  //==========================//
  // Start with the main work //
  //==========================//

  // Put all Hdf5fed funktions in here.
  #ifdef HAVE_HDF5
  // Create H5Fed class instance. 
  Hdf5fed::Hdf5fed h5fedFile;
  
  // Open Hdf5fed file for reading. Filename comes from 
  // command line parameters. 
  h5fedFile.open(hdf5fedFile,Hdf5fed::FILE_READ);

  // Vector for the tetrahedorn nodes and the material tag.
  std::vector< std::vector<unsigned int> > elem;
  std::vector<unsigned int> materialIndex;

  // Read the tetrahedrons of the hdf5fed file an print them.
/*  h5fedFile.rTetrahedron((unsigned int)0, elem, materialIndex);
  for(int varI = 0; varI<elem.size(); varI++)
  {
    rDebug("Tet number: %d; nodes: %d %d; %d %d; material index: %d",
           varI,
           elem[varI][0],
           elem[varI][1],
           elem[varI][2],
           elem[varI][3],
           materialIndex[varI]);
  }
*/
  // Read the boundary trianglel of the hdf5fed file an print them.
  h5fedFile.rTriangleB((unsigned int)0, (unsigned int) 0, elem, materialIndex);
  for(int varI = 0; varI<elem.size(); varI++)
  {
    rDebug("Triangle number: %d; nodes: %d %d; %d; material index: %d",
           varI,
           elem[varI][0],
           elem[varI][1],
           elem[varI][2],
           materialIndex[varI]);
  }
/*
  // Read the 3d coordinates of the h5fed file an print them.
  std::vector<std::vector< double> > coord;
  h5fedFile.rCoord3d(coord);
  for(int varI = 0; varI<coord.size(); varI++)
  {
    rDebug("Coor Number %d; Coord3d: %f;  %f;  %f",
           varI,
           coord[varI][0],
           coord[varI][1],
           coord[varI][2]);
  }
  
*/  
  // Close Hdf5fed file.
  h5fedFile.close();
    
  #endif // HAVE_HDF5

  return(0);
}

