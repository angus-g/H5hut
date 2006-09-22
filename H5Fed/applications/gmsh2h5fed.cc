// rights - 2006-, copyright patrick leidenberger and benedikt oswald,
//                 all rights reserved
// project   - gmsh2h5fed
// file name - gmsh2h5fed.cc
// file type - c++ implementaton file
// objective - main file for the gmsh to hdf5fed converter
// modified  - 2006 jun 26, creation, patrick leidenberger
// modified  - 2006 aug 25, extend, patrick leidenberger
// modified  - 2006 aug 26, pl, integrate automatic index mapping.
// modified  - 2006 sep 22, pl, addaped to h5fed api changes.
//
// feature - Implements the a mesh preprocessor.
// feature - It will read gmsh's mesh files of version 2.0 and write the mesh
// feature - into an HDF5/FiniteElementData file.
// required software - rlog library, boost library 

#include <gmsh2h5fed.hh>

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
  string gmshInputFile;
  string hdf5fedOutputFile;
  bool writeFileForce = false;
 
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
       "gmsh v2.0 input file")
      ("output-file", boost::program_options::value<string>(),
       "hdf5fed output file")
      ("force", "if output file already exists, overwrite");
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
    else if ((varMap.count("input-file")) 
             && (varMap.count("output-file")))
    {
      gmshInputFile = varMap["input-file"].as<string>();
      hdf5fedOutputFile = varMap["output-file"].as<string>();
      rInfo("Input filename: %s",
            gmshInputFile.c_str());
      rInfo("Output filename: %s",
            hdf5fedOutputFile.c_str());
      // Check it --force is set.
      // --force writes the output file, if the file already exists.
      if (varMap.count("force"))
        writeFileForce = true;
      else 
        writeFileForce = false;
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
  //---------------------------------------//
  // Variables for the data form gmsh file //
  //---------------------------------------//
  // Variable for the gmsh nodes.
  std::vector<vector<double> > gmshNodes;
  gmshNodes.clear();
  // Vector for the gmshs nodes index.
  std::vector<unsigned int> gmshNodesIndex;
  gmshNodesIndex.clear();
  // Vector for the gmshs tetrahedrons.
  std::vector<std::vector<unsigned int> > gmshTetrahedron;
  gmshTetrahedron.clear();
  // Vector for the gmshs tetrahedrons.
  std::vector<std::vector<unsigned int> > gmshTetrahedronTag;
  gmshTetrahedronTag.clear();
  
  // Make an instance of the vtk class.
  gmsh gmshInFile;

  // Set the file name of the input file
  gmshInFile.gmshFileName(gmshInputFile);

  // Open the gmsh mesh file, write the content to a string and close the
  // file.
  gmshInFile.gmshOpen();
  
  // Parse the gmsh mesh file.
  gmshInFile.gmshParseFile(&gmshInFile);
  
  // Get a vector with all node coordinates from the gmsh file.
  gmshNodes = gmshInFile.gmshNode();
  
  // Put all H5Fed funktions in here.
  #ifdef HAVE_HDF5
  // Create H5Fed class instance. 
  H5Fed::H5Fed h5fedFile;
  
  // Open H5Fed file for writing. Filename and file write access comes
  // from command line parameters. 
  if (writeFileForce == false)
    h5fedFile.open(hdf5fedOutputFile,FILE_CREATE);
  else
    h5fedFile.open(hdf5fedOutputFile,FILE_CREATE_FORCE);

  // Create the group hierarchie in the hdf5fed file.
  h5fedFile.createGroupHierarchie();

  // Get the gmsh node index vector
  gmshNodesIndex = gmshInFile.gmshNodeNumber();
  
  // The gmsh node index has gaps and is not consecutive.
  // So we activate the automatic index mapping from the h5fed api.
  h5fedFile.beginIndexMapping(gmshNodesIndex);

    // Get a vector with all node coordinates from the gmsh file.
    gmshNodes = gmshInFile.gmshNode();
    // Write the nodes to the h5fed file.
    h5fedFile.wCoord3d(gmshNodes);
    // Every node in h5fed file, so we can save memory.
    gmshNodes.clear();
  
    //Get a vector with all tetrahedrons from gmsh file.
    gmshTetrahedron = gmshInFile.gmshTetrahedron();
    for(int varI = 0; varI<gmshTetrahedron.size(); varI++)
    {
//      rDebug("Elem: %d Nodes: %d; %d; %d; %d", varI, gmshTetrahedron[varI][0], gmshTetrahedron[varI][1], gmshTetrahedron[varI][2], gmshTetrahedron[varI][3]);
    }
    std::vector<unsigned int> gmshTetrahedronMatIndex;
    gmshTetrahedronMatIndex.resize(0,gmshTetrahedron.size());
    // Write the tetrahedrons to the h5fed file on the respective level.
    h5fedFile.wTetrahedron(0,gmshTetrahedron, gmshTetrahedronMatIndex);
    // Every terahedron in h5fed file, so we can save memory.
    gmshTetrahedron.clear();
    
    //Get a vector with all tetrahedrons from gmsh file.
    gmshTetrahedronTag = gmshInFile.gmshTetrahedronTag();
    for(int varI = 0; varI<gmshTetrahedronTag.size(); varI++)
    {
//      rDebug("Elem: %d Nodes: %d; %d", varI, gmshTetrahedronTag[varI][0], gmshTetrahedronTag[varI][1]);
    }


  
  // End with the automatic index mapping because we have no further actcion
  // with an gmsh file index.
  h5fedFile.endIndexMapping();

  // Close H5Fed file.
  h5fedFile.close();
  
    
  #endif // HAVE_HDF5
/*  
  // Write all vtkPoints_ to the screen.
  for (int i = 0; i<(gmshInFile.getVtkVecSize()); i++)
  {
    rDebug("%d vtkPoint x, y, z: %e %e %e", 
            i, gmshInFile.getVtkVec(i,0),gmshInFile.getVtkVec(i,1),gmshInFile.getVtkVec(i,2));
  }
*/
/*  
  //Write all vktCells_ with cell types and cell points on the screen.
  for (int i = 0; 
       (i<(gmshInFile.getVtkCellTypesN())) && (i<(gmshInFile.getVtkCellsN()));
       i++)
  {
    rDebug("vtkCellType: %d", gmshInFile.getVtkCellType(i));
    for (int j = 0; j<(gmshInFile.getVtkCellPointN(i)); j++)
    {
      rDebug("   vtkPoint: %d", gmshInFile.getVtkCellPoint(i,j));
    }
    
  }

  
  // Write all vtk point datas on the screen.
  //rDebug("size: %d", gmshInFile.vtkVectorSize("vtkPointData_"));
  for (int i = 0; 
       i<(gmshInFile.vtkVectorSize("vtkPointData_"));
       i++)
  {
    for (int j = 0; j<(gmshInFile.vtkVectorSize("vtkPointData_",i)); j++)
    {
      for(int k = 0; k<(gmshInFile.vtkVectorSize("vtkPointData_",i,j)); k++)
      rDebug("vtk point vector element i, j, k, value: %d %d %d %e",
             i,j,k,gmshInFile.vtkVector("vtkPointData_",i,j,k));
    }
  }

  // Write all vtkPointsType_ to the screen.
  rDebug("size of vtkPointDataType_: %d", gmshInFile.vtkVectorSize("vtkPointDataType_"));
  for (int i = 0; i<(gmshInFile.vtkVectorSize("vtkPointDataType_")); i++)
  {
    rDebug("vtkPointDataType_ %s", gmshInFile.vtkVector("vtkPointDataType_",i).c_str());
  }
  // Write all vtkPointsName_ to the screen.
  rDebug("size of vtkPointDataName_: %d", gmshInFile.vtkVectorSize("vtkPointDataName_"));
  for (int i = 0; i<(gmshInFile.vtkVectorSize("vtkPointDataName_")); i++)
  {
    rDebug("vtkPointName_ %s", gmshInFile.vtkVector("vtkPointDataName_",i).c_str());
  }
  // Write all vtkPointsFormat_ to the screen.
  rDebug("size of vtkPointDataFormat_: %d", gmshInFile.vtkVectorSize("vtkPointDataFormat_"));
  for (int i = 0; i<(gmshInFile.vtkVectorSize("vtkPointDataFormat_")); i++)
  {
    rDebug("vtkPointFormat_ %s", (gmshInFile.vtkVector("vtkPointDataFormat_",i)).c_str());
  }


  // Write all vtk point datas on the screen.
  //rDebug("size: %d", gmshInFile.vtkVectorSize("vtkCellData_"));
  for (int i = 0; 
       i<(gmshInFile.vtkVectorSize("vtkCellData_"));
       i++)
  {
    for (int j = 0; j<(gmshInFile.vtkVectorSize("vtkCellData_",i)); j++)
    {
      for(int k = 0; k<(gmshInFile.vtkVectorSize("vtkCellData_",i,j)); k++)
      rDebug("vtk cell vector element i, j, k, value: %d %d %d %e",
             i,j,k,gmshInFile.vtkVector("vtkCellData_",i,j,k));
    }
  }
  
  // Write all vtkCellsType_ to the screen.
  rDebug("size of vtkCellDataType_: %d", gmshInFile.vtkVectorSize("vtkCellDataType_"));
  for (int i = 0; i<(gmshInFile.vtkVectorSize("vtkCellDataType_")); i++)
  {
    rDebug("vtkCellDataType_ %s", (gmshInFile.vtkVector("vtkCellDataType_",i)).c_str());
  }
  // Write all vtkCellsName_ to the screen.
  rDebug("size of vtkCellDataName_: %d", gmshInFile.vtkVectorSize("vtkCellDataName_"));
  for (int i = 0; i<(gmshInFile.vtkVectorSize("vtkCellDataName_")); i++)
  {
    rDebug("vtkCellDataName_ %s", (gmshInFile.vtkVector("vtkCellDataName_",i)).c_str());
  }
  // Write all vtkCellsFormat_ to the screen.
  rDebug("size of vtkCellDataFormat_: %d", gmshInFile.vtkVectorSize("vtkCellDataFormat_"));
  for (int i = 0; i<(gmshInFile.vtkVectorSize("vtkCellDataFormat_")); i++)
  {
    rDebug("vtkCellDataFormat_ %s", (gmshInFile.vtkVector("vtkCellDataFormat_",i)).c_str());
  }
*/
  
//  cout <<"\n\n" <<gmshInFile.getUg PointsN()<< "\n\n";
  
//  int i = gmshInFile.openVtk();
//vtk* vtkPt;
//vtkPt = &gmshInFile;
//vtkPt->vtkVersion();

//gmshInFile.vtkVersion();

  // This vector is used in loops to transport datas from vtk to ristream.
  vector<double> dataVector1;
/*  
  // Scaling factor for the vtk input data.
  double scale = 15.0;
  ristream rib;
  // Set the filname of the .rib file.
  rib.RibsFile(ribOutputFile);
  rib.openRibs();
  rib.RibsFormat(1024,768);
  rib.RibsLightSource("distantlight");
  rib.RibsProjection("perspective");
  //rib.RibsProjection("orthographic");
  
  // Give the center or the rotation.
  double rotCenterX = 0.0;
  double rotCenterY = 0.0;
  double rotRadius = 1.6;
  double cameraPosX = sqrt(3.92); // Only the camera postion if nLoop = 1;
  double cameraPosY = 0;
  cameraPosX = 1; // Only the camera postion if nLoop = 1;
  cameraPosY = -1;

  char filename[20];

  const int nLoop = 1;
  for(int i = 0; i<nLoop; i++)
  {
    if (nLoop > 1)
    {
      cameraPosX = -rotRadius*cos(2*PI*i/(double)nLoop) + rotCenterX;
      cameraPosY = -rotRadius*sin(2*PI*i/(double)nLoop) + rotCenterY;
    }
    
    rib.RibsFrameBegin();  
      // Set the file name for the renderer output image.
      string tempstring = "";
      tempstring = "tests/movie_data/Hallo";
      //tempstring.append("%d.tiff",i);
      //tempstring.append((string)i);
      tempstring.append(".tiff");
      //sprintf ( tempchar ,"tests/movie_data/Hallo%d.tiff",i );
//      rib.RibsFrameFilename("");
      rib.RibsDisplay(frameName);

      rDebug(tempstring.c_str());

      // Camera Position and orientation:
      // The default camera postion is the world coordinate origin and it
      // looks in the -z-direction.

      // The function RibsPlaceCamera set the position of the camera in world  
      // coordinates and the point, where to look at in world coordinates.
      // To get a skew view, you can roll the camera.
      // This function must be called befor RibsWorldBegin!
      rib.RibsPlaceCamera(1.5*cameraPosX, 1.5*cameraPosY, 2.0,  // position
                          0.0, 0.0, 1.2,               // look-at point
                          0.0) ;                      // roll angle (in degree)
      
      rib.RibsWorldBegin();
        rib.RibsSurface("plastic");
        rib.RibsCoordinateSystem();

        //Write all vktCells_ with cell types and cell points on the screen.
        //rDebug("getVtkCellTypesN(): %d", gmshInFile.getVtkCellTypesN());
        for (int i = 0; i<(gmshInFile.getVtkCellTypesN()); i++)
        {
          // Look if there is a thetrahedron.
          if (gmshInFile.getVtkCellType(i) == 10)
          {
            // Clear the data structur to save the point cooridnates temporal.
            dataVector1.clear();
            
            // Read the points of a tetrahedron.
            //rDebug("getVtkCellPointN(%d): %d", i, gmshInFile.getVtkCellPointN(i));
            for (int j = 0; j<(gmshInFile.getVtkCellPointN(i)); j++)
            {
              int tempInt = gmshInFile.getVtkCellPoint(i,j);
              dataVector1.push_back(gmshInFile.getVtkVec(tempInt, 0));
              dataVector1.push_back(gmshInFile.getVtkVec(tempInt, 1));
              dataVector1.push_back(gmshInFile.getVtkVec(tempInt, 2));
              //rDebug("vtkPoint: %d, %f %f %f", tempInt,
              //        gmshInFile.getVtkVec(tempInt, 0),
              //        gmshInFile.getVtkVec(tempInt, 1),
              //        gmshInFile.getVtkVec(tempInt, 2));
              
            }
        rib.RibsColor(0.0,0,1.0,1.0);
            rib.RibsTetrahedron(scale*dataVector1[0], scale*dataVector1[1], scale*dataVector1[2],
                                scale*dataVector1[3], scale*dataVector1[4], scale*dataVector1[5],
                                scale*dataVector1[6], scale*dataVector1[7], scale*dataVector1[8],
                                scale*dataVector1[9], scale*dataVector1[10],scale*dataVector1[11],
                                0.8);
          }
        }
*/
/*                         
        rib.RibsColor(1.0,0,0,0.6);
        rib.RibsTetrahedron(0.3, 0.3, 0.0,
                            2.0, 0.3, 0.0,
                            0.3, 2.0, 0.0,
                            0.3, 0.3, 0.5,
                            1.0);
        rib.RibsColor(0,1.0,0,0.7);
        rib.RibsTetrahedron(0.3, 0.3, 0.0,
                            2.0, 0.3, 0.0,
                            0.3, 2.0, 0.0,
                            0.3, 0.3, 0.5,
                            0.6);

        rib.RibsColor(0.0,0,1.0,1.0);
        rib.RibsTetrahedron(0.3, 0.3, 0.0,
                            2.0, 0.3, 0.0,
                            0.3, 2.0, 0.0,
                            0.3, 0.3, 0.5,
                            0.3);
*/                            
//        rib,RibsAxes();
//        rib.RibsVector(0,0,0,0.5);
/*
      rib.RibsWorldEnd();
    rib.RibsFrameEnd();
  }
  rib.closeRibs();
*/
//  return(0);
}

