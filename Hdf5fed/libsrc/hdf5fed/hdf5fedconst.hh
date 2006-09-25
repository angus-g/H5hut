// rights  2006- , 
//             copyright patrick leidenberger and benedikt oswald,
//             all rights reserved
// project   - hdf5fedconst.hh
// file name - hdf5fedconst.cc
// file type - c++ header file
// objective - header file for the hdf5fed api.
// modified  - 2006 sep 25, pl, add this comment and change h5fed -> hdf5fed.
//
// feature - Defines the group and dataset names in hdf5fed file.
// feature - Defines some topological constants.
// feature - Defines file access constans.
//
// required software - stl, hdf5 library

#ifndef HDF5FEDCONST_HH_
#define HDF5FEDCONST_HH_

#include<string>

// Include HDF5 headers.
#include <hdf5.h>

namespace Hdf5fed
{
  
// Return values.
const int OKCODE    = 0;
const int ERRORCODE = 1;

//! Hdf5 specific file access:
// "r" for read only access.
// "w" for read and write access.
// "c" create a new a new file with if it does not exist already.
// "cf" create a new file, overwrite it if it exists already.
const std::string FILE_READ("r");
const std::string FILE_READ_WRITE("w");
const std::string FILE_CREATE("c");
const std::string FILE_CREATE_FORCE("cf");

// Define the group names of the H5Fed starndard groups.
const std::string HDF5FED_G_ROOT           ("/HDF5_FINITE_ELEMENT_DATA");
const std::string HDF5FED_G_COORD          (HDF5FED_G_ROOT+"/COORD");
const std::string HDF5FED_G_VOLUME_MESH    (HDF5FED_G_ROOT+"/VOLUME_MESH");
const std::string HDF5FED_G_BOUNDARY_MESH  (HDF5FED_G_ROOT+"/BOUNDARY_MESH");
const std::string HDF5FED_G_MATERIAL       (HDF5FED_G_ROOT+"/MATERIAL");
const std::string HDF5FED_G_ELECTROMAGNETIC 
                    (HDF5FED_G_MATERIAL+"/ELECTROMAGNETIC");
const std::string HDF5FED_G_DISCRETE  (HDF5FED_G_ELECTROMAGNETIC+"/DISCRETE");
const std::string HDF5FED_G_PHYSICAL  (HDF5FED_G_ELECTROMAGNETIC+"/PHYSICAL");
const std::string HDF5FED_G_DEBYE     (HDF5FED_G_PHYSICAL+"/DEBYE");
const std::string HDF5FED_G_LORENTZ   (HDF5FED_G_PHYSICAL+"/LORENTZ");
const std::string HDF5FED_G_DRUDE     (HDF5FED_G_PHYSICAL+"/DRUDE");
const std::string HDF5FED_G_DOF       (HDF5FED_G_ROOT+"/DOF");
const std::string HDF5FED_G_FIELD     (HDF5FED_G_ROOT+"/FIELD");


// Define the dataset names of the H5Fed standard datasets.
const std::string HDF5FED_D_COORD3D  (HDF5FED_G_COORD+"/COORD3D");
const std::string HDF5FED_D_TETMESH  (HDF5FED_G_VOLUME_MESH+"/TETMESH_L");
const std::string HDF5FED_D_HEXMESH  (HDF5FED_G_VOLUME_MESH+"/HEXMESH_L");
const std::string HDF5FED_D_PRISMATICMESH 
                    (HDF5FED_G_VOLUME_MESH+"/PRISMATICMESH_L");
const std::string HDF5FED_D_PYRAMIDMESH
                    (HDF5FED_G_VOLUME_MESH+"/PYRAMIDMESH_L");
const std::string HDF5FED_D_TRIANGLEMESH
                    (HDF5FED_G_VOLUME_MESH+"/TRIANGLEMESH_L");
const std::string HDF5FED_D_QUADRANGLEMESH 
                    (HDF5FED_G_VOLUME_MESH+"/QUADRANGLEMESH_L");
const std::string HDF5FED_D_TRIANGLEBOUNDARY 
                    (HDF5FED_G_BOUNDARY_MESH+"/BOUNDARY_TRIANGLE_L");

// How much nodes have a geometric figure. 
const unsigned short int HDF5FED_TET_N_NODE        = 4;
const unsigned short int HDF5FED_HEX_N_NODE        = 8;
const unsigned short int HDF5FED_PRISMATIC_N_NODE  = 6;
const unsigned short int HDF5FED_PYRAMID_N_NODE    = 5;
const unsigned short int HDF5FED_TRIANGLE_N_NODE   = 3;
const unsigned short int HDF5FED_QUADRANGLE_N_NODE = 4;

// In which format should a single element of a mesh entity and the
// index to the material list be stored.
const hid_t HDF5FED_MESH_ELEM_DATATYPE = H5T_STD_U32LE;
const hid_t HDF5FED_COORD_DATATYPE     = H5T_IEEE_F64LE;


} /* namespace H5Fed*/
#endif /*HDF5FEDCONST_HH_*/
