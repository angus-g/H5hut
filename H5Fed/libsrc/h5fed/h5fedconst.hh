#ifndef H5FEDCONST_HH_
#define H5FEDCONST_HH_

#include<string>

// Include HDF5 headers.
#include <hdf5.h>

using namespace std;

//! Hdf5 specific file access:
// "r" for read only access.
// "w" for read and write access.
// "c" create a new a new file with if it does not exist already.
// "cf" create a new file, overwrite it if it exists already.
const string FILE_READ("r");
const string FILE_READ_WRITE("w");
const string FILE_CREATE("c");
const string FILE_CREATE_FORCE("cf");

// Define the group names of the H5Fed starndard groups.
const string H5FED_G_ROOT                ("/HDF5_FINITE_ELEMENT_DATA");
  const string H5FED_G_COORD             (H5FED_G_ROOT+"/COORD");
  const string H5FED_G_VOLUME_MESH       (H5FED_G_ROOT+"/VOLUME_MESH");
  const string H5FED_G_BOUNDARY_MESH     (H5FED_G_ROOT+"/BOUNDARY_MESH");
  const string H5FED_G_MATERIAL          (H5FED_G_ROOT+"/MATERIAL");
    const string H5FED_G_ELECTROMAGNETIC (H5FED_G_MATERIAL+"/ELECTROMAGNETIC");
      const string H5FED_G_DISCRETE      (H5FED_G_ELECTROMAGNETIC+"/DISCRETE");
      const string H5FED_G_PHYSICAL      (H5FED_G_ELECTROMAGNETIC+"/PHYSICAL");
        const string H5FED_G_DEBYE       (H5FED_G_PHYSICAL+"/DEBYE");
        const string H5FED_G_LORENTZ     (H5FED_G_PHYSICAL+"/LORENTZ");
        const string H5FED_G_DRUDE       (H5FED_G_PHYSICAL+"/DRUDE");
  const string H5FED_G_DOF               (H5FED_G_ROOT+"/DOF");
  const string H5FED_G_FIELD             (H5FED_G_ROOT+"/FIELD");

// Define the dataset names of the H5Fed standard datasets.
const string H5FED_D_COORD3D        (H5FED_G_COORD+"/COORD3D");
const string H5FED_D_TETMESH        (H5FED_G_VOLUME_MESH+"/TETMESH_L");
const string H5FED_D_HEXMESH        (H5FED_G_VOLUME_MESH+"/HEXMESH_L");
const string H5FED_D_PRISMATICMESH  (H5FED_G_VOLUME_MESH+"/PRISMATICMESH_L");
const string H5FED_D_PYRAMIDMESH    (H5FED_G_VOLUME_MESH+"/PYRAMIDMESH_L");
const string H5FED_D_TRIANGLEMESH   (H5FED_G_VOLUME_MESH+"/TRIANGLEMESH_L");
const string H5FED_D_QUADRANGLEMESH (H5FED_G_VOLUME_MESH+"/QUADRANGLEMESH_L");

// How much nodes have a geometric figure. 
const unsigned short int H5FED_TET_N_NODE        = 4;
const unsigned short int H5FED_HEX_N_NODE        = 8;
const unsigned short int H5FED_PRISMATIC_N_NODE  = 6;
const unsigned short int H5FED_PYRAMID_N_NODE    = 5;
const unsigned short int H5FED_TRIANGLE_N_NODE   = 3;
const unsigned short int H5FED_QUADRANGLE_N_NODE = 4;

// In which format should a single element of a mesh entity and the
// index to the material list be stored.
const hid_t H5FED_MESH_ELEM_DATATYPE = H5T_STD_U32LE;
const hid_t H5FED_COORD_DATATYPE = H5T_IEEE_F64LE;


#endif /*H5FEDCONST_HH_*/
