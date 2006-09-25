// rights - 2006-, copyright benedikt, all rights reserved
// project - phidias3d
// file name - hdf5em.h
// file type - c++ header file
// objective - declare class for HDF5/ELECTROMAGNETIC file format access
// modified  - 2006 jun 26, creation, benedikt oswald
// modified  - 2006 aug 26, pl, integrate automatic index mapping.
// modified  - 2006 sep 22, pl, add output functions for tets and coordinates.
// inheritance - 
// feature - declares the base class for HDF5/ELECTROMAGNETIC file format access;
// feature - this class is completely self contained, i.e. it does not need anything
// feature - external, except the STL and the HDF5 header files and the corresponding
// feature - library.
// required software - 

#ifndef HDF5FED_HH_
#define HDF5FED_HH_

#ifdef HAVE_HDF5

/* include standard header files */
#include <cmath>
#include <iostream>
#include <complex>
#include <string>
#include <vector>

// Include the files for rlog. 
#ifdef HAVE_RLOG
  #include <rlog/rlog.h>
  #include <rlog/rloglocation.h>
  #include <rlog/Error.h>
  #include <rlog/RLogChannel.h>
  #include <rlog/StdioNode.h>
  #include <rlog/RLogTime.h>
#endif // HAVE_RLOG    

// Include HDF5 headers.
#include <hdf5.h>

// Include h5fed specific constants.
#include "hdf5fedconst.hh"

/* include standard proprietary header files */
/*
#include "nonsciconst.h"
#include "physicomath.h"
*/

// Don't use this: doesn't working with dune.
//using namespace std;
//using namespace physicomath;
//using namespace nonsciconst;

namespace Hdf5fed
{

class Hdf5fed {
public:

  /** \brief constructor and destructor */
  Hdf5fed()
  {
    doIndexMapping_ = false;
    indexMap_.clear();
    positionMap_.clear();
    
    // Deactivate the HDF5 error output.
    H5Eset_auto (0, NULL );
  };

  // The Destructor.
  ~Hdf5fed(){};

  //! Open an hdf5 finite element data file with appropriate access.
  int open(std::string fileName, std::string fileAccess)
  {
    // Store filename and file access in private variable.
    fileName_ = fileName;
    fileAccess_ = fileAccess;
    // Open file with respective rights via HDF5 API.
    if (!fileAccess_.compare(FILE_READ))
    {
      hdf5FileIdent_ = H5Fopen(fileName_.c_str(), H5F_ACC_RDONLY, H5P_DEFAULT);
      return OKCODE;
    }
    else if (!fileAccess_.compare(FILE_READ_WRITE))
    {
      hdf5FileIdent_ = H5Fopen(fileName_.c_str(), H5F_ACC_RDWR, H5P_DEFAULT);
      return OKCODE;
    }
    else if (!fileAccess_.compare(FILE_CREATE))
    {
      hdf5FileIdent_ = H5Fcreate(fileName_.c_str(), H5F_ACC_EXCL,
                                 H5P_DEFAULT, H5P_DEFAULT);
      if (hdf5FileIdent_ < 0)
      {
        rError("The file %s already exist.",fileName_.c_str());
        rError("To overwrite use the --force option.");
        exit(ERRORCODE);
      }
      else
        return OKCODE;
    }
    else if (!fileAccess_.compare(FILE_CREATE_FORCE))
    {
      hdf5FileIdent_ = H5Fcreate(fileName_.c_str(), H5F_ACC_TRUNC,
                                 H5P_DEFAULT, H5P_DEFAULT);
      return OKCODE;
    }
    else
    {
      rError("Unknown file-open-attribute for hdf5 file.");
      exit(ERRORCODE);
    }
  };

   // Close an open hdf5 file.
  int close(void)
  {
    if (hdf5FileIdent_ >= 0)
    {
      hdf5Status_ = H5Fclose(hdf5FileIdent_);
      return OKCODE;
    }
    else 
    {
      rError("You cannot close a file that is not opened.");
      return ERRORCODE;
    }
  };

  // Create the empty HDF5 Finite Element Stardart group hierarchy.
  int createGroupHierarchie()
  {
    // Hdf5 group identifier for group access, localy valid (this method).
    hid_t hdf5GroupIdent_;
    // Create the groups. All group-name strings are defined in 
    // hdf5fedconst.hh.
    hdf5GroupIdent_ = H5Gcreate(hdf5FileIdent_,
                              HDF5FED_G_ROOT.c_str(),H5P_DEFAULT);
    hdf5GroupIdent_ = H5Gcreate(hdf5FileIdent_,
                              HDF5FED_G_COORD.c_str(),H5P_DEFAULT);
    hdf5GroupIdent_ = H5Gcreate(hdf5FileIdent_,
                              HDF5FED_G_VOLUME_MESH.c_str(),H5P_DEFAULT);
    hdf5GroupIdent_ = H5Gcreate(hdf5FileIdent_,
                              HDF5FED_G_BOUNDARY_MESH.c_str(),H5P_DEFAULT);
    hdf5GroupIdent_ = H5Gcreate(hdf5FileIdent_,
                              HDF5FED_G_MATERIAL.c_str(),H5P_DEFAULT);
    hdf5GroupIdent_ = H5Gcreate(hdf5FileIdent_,
                              HDF5FED_G_ELECTROMAGNETIC.c_str(),H5P_DEFAULT);
    hdf5GroupIdent_ = H5Gcreate(hdf5FileIdent_,
                              HDF5FED_G_DISCRETE.c_str(),H5P_DEFAULT);
    hdf5GroupIdent_ = H5Gcreate(hdf5FileIdent_,
                              HDF5FED_G_PHYSICAL.c_str(),H5P_DEFAULT);
    hdf5GroupIdent_ = H5Gcreate(hdf5FileIdent_,
                              HDF5FED_G_DEBYE.c_str(),H5P_DEFAULT);
    hdf5GroupIdent_ = H5Gcreate(hdf5FileIdent_,
                              HDF5FED_G_LORENTZ.c_str(),H5P_DEFAULT);
    hdf5GroupIdent_ = H5Gcreate(hdf5FileIdent_,
                              HDF5FED_G_DRUDE.c_str(),H5P_DEFAULT);
    hdf5GroupIdent_ = H5Gcreate(hdf5FileIdent_,
                              HDF5FED_G_DOF.c_str(),H5P_DEFAULT);
    hdf5GroupIdent_ = H5Gcreate(hdf5FileIdent_,
                              HDF5FED_G_FIELD.c_str(),H5P_DEFAULT);
    return OKCODE;
  };

  // This function activate the automatic index mapping to create a gapfree
  // and consecutive index set for the hdf5fed file.
  // The indexVec holdes the numbers of the nodes in the same way, as the
  // nodes are in the node vector.
  int beginIndexMapping(std::vector<unsigned int>& indexVec)
  {
    rDebug("Begin automatic index mapping.");
    doIndexMapping_ = true;
    indexMap_.clear();
    positionMap_.clear();

    // Copy the old index to the map as map-key.
    for(unsigned int varI = 0; varI < indexVec.size(); varI++)
    {
      indexMap_.insert(std::make_pair(indexVec[varI],0));
    }
    // Number all elements in the map consecutive, starting with zero, that
    // is the new index set.
    unsigned int tempUnInt = 0;
    for(std::map<unsigned int, unsigned int>::iterator 
        iter = indexMap_.begin();
        iter != indexMap_.end(); iter ++)
    {
      iter->second = tempUnInt;
      tempUnInt++;
    }
    // The first column contains the new index, the second the old position
    // of the coordinate in the vector.
    for(unsigned int varI = 0; varI < indexVec.size(); varI++)
    {
      positionMap_.insert(std::make_pair(
                           (indexMap_.find(indexVec[varI]))->second,varI));
    }
    return OKCODE;
  };
  
  // This function deactivate the automatic index mapping.
  // The uses index sets must be consecutive and gapfree.
  int endIndexMapping()
  {
    rDebug("End automatic index mapping.");
    doIndexMapping_ = false;
    indexMap_.clear();
    return OKCODE;
  };

  // Write 3dim coordinates to hdf5fed file.
  int wCoord3d (std::vector<std::vector<double> >& coord)
  {
    // All these operations are only allowed, if there is a valid file
    // identifier.
    if (hdf5FileIdent_ >= 0)
    {
      // Define the rank of the different dataspaces.
      const int rank = 2;
      // Hdf5 error handling variable for Hdf5 actions.
      herr_t hdf5Status;
      // Hdf5 dataspace identifier.
      hid_t hdf5DataspaceId;
      // Hdf5 dataset identifier.
      hid_t hdf5DatasetId;
      // Define the dimension of the data array.
      // Number of rows: as much as coordinates.
      // Number of colums: 3, one for each dimension.
      hsize_t dim[2];
      dim[0] = coord.size();
      dim[1] = 3;
      
      // We copy the coordinates from coord in an standart array, so that 
      // hdf5 can handle it.
      // We do this in portions, so we save memory.
      
      // Subdimension of hyperslab in memory: 1 line, 3 columns.
      hsize_t dim_sub_mem[2];
      dim_sub_mem[0] = 1;
      dim_sub_mem[1] = 3;
      // Subdimension of hyperslab in file: 1 line, 3 columns.
      hsize_t dim_sub_file[2];
      dim_sub_file[0] = 1;
      dim_sub_file[1] = 3;
      // Define the offset for reading from the memory.
      // Always fixed.
      hsize_t offset_mem[2];
      offset_mem[0] = 0;
      offset_mem[1] = 0;
      // Define the offset for writing into the file.
      hsize_t offset_file[2];
      offset_file[0] = 0;     // Row only at the beginning. We iterate over it.
      offset_file[1] = 0;     // We have no offset respective to the column.
      
      // Array for x, y and z component of one coordinate.
      double coordinate[3];
      
      // Dimesion and datatype of the file dataset, read out form file.
      // This is not the fastest, but the most robust way.
      hsize_t dim_out[2];
      hid_t dataType;


      // Create dataspace of full size.
      hdf5DataspaceId = H5Screate_simple(rank, dim, H5P_DEFAULT);

      // Create dataset of full size.
      hdf5DatasetId = H5Dcreate(hdf5FileIdent_, HDF5FED_D_COORD3D.c_str(), 
                                HDF5FED_COORD_DATATYPE,
                                hdf5DataspaceId, H5P_DEFAULT);
      

      
      // Loop over all rows of the dataset (file) and copy the coord vector
      // element wise.
      hdf5Status = H5Sget_simple_extent_dims(hdf5DataspaceId, dim_out, H5P_DEFAULT);
      dataType = H5Dget_type(hdf5DatasetId);
      // If we use automatic mapping we need an interator to the map.     
      std::map<unsigned int, unsigned int>::iterator 
          iter = positionMap_.begin();
      for (unsigned int varI = 0; varI < dim_out[0]; varI++)
      {
        // Iterate over the rows in file dataspace.
        offset_file[0] = varI;
        
        // Copy every element of a coordinate.
        for (unsigned int varJ = 0; varJ < dim_out[1]; varJ++)
        {
          // Here is the only special case for automated mapping:
          // Do not copy the next in the input vector, copy the coord
          // with the next following number.
          if (doIndexMapping_ == true)
           coordinate[varJ] = coord[iter->second][varJ];
           else
             coordinate[varJ] = coord[varI][varJ];
        }

        // Select hyperslab ('region') in file dataspace.
        hdf5Status = H5Sselect_hyperslab(hdf5DataspaceId, H5S_SELECT_SET,
                                         offset_file, H5P_DEFAULT,
                                         dim_sub_file, H5P_DEFAULT);

        // Define memory Dataspace.
        hid_t hdf5MemspaceId;
        hdf5MemspaceId = H5Screate_simple(rank, dim_sub_mem, H5P_DEFAULT);
        // Select hyperslab ('region') in memory.
        hdf5Status = H5Sselect_hyperslab(hdf5MemspaceId, H5S_SELECT_SET,
                                         offset_mem, H5P_DEFAULT,
                                         dim_sub_mem, H5P_DEFAULT);
        
        // Copy dataset to dataset from memory to file.
        hdf5Status = H5Dwrite(hdf5DatasetId, H5T_NATIVE_DOUBLE,
                              hdf5MemspaceId, hdf5DataspaceId,
                              H5P_DEFAULT, coordinate);
        
        H5Sclose(hdf5MemspaceId);
        // Increment the map iterator if we make automatic mapping.
        if (doIndexMapping_) 
          iter++;
     }

      // Close hdf5 identifier.
      hdf5Status = H5Dclose(hdf5DatasetId);
      hdf5Status = H5Sclose(hdf5DataspaceId);
      
      return OKCODE;
    }
    else 
    {
      rError("You cannot operate to dataset COORD3D.");
      rError("There is no valid file identifier.");
      return ERRORCODE;
    }
  };

  // Read 3dim coordinates from hdf5fed file.
  int rCoord3d (std::vector<std::vector<double> >& coord)
  {
    // All these operations are only allowed, if there is a valid file
    // identifier.
    if (hdf5FileIdent_ >= 0)
    {
      // The name of the dataset.
      std::string datasetName = HDF5FED_D_COORD3D;
      // Define the rank of the different dataspaces.
      const int rank = 2;
      // Hdf5 error handling variable for Hdf5 actions.
      herr_t hdf5Status;
      // Hdf5 dataspace identifier.
      hid_t hdf5DataspaceId;
      // Hdf5 dataset identifier.
      hid_t hdf5DatasetId;
      // Open the hdf5 dataset.
      hdf5DatasetId = H5Dopen(hdf5FileIdent_, datasetName.c_str());
      // Get the dataspace from the dataset.
      hdf5DataspaceId = H5Dget_space(hdf5DatasetId);
      // Read the dimension of the file dataspace.
      hsize_t dim_out[rank];
      hdf5Status = H5Sget_simple_extent_dims(hdf5DataspaceId, dim_out,
                                             H5P_DEFAULT);

      // Get the datatype of the datas we want to read.
      hid_t dataType;
      dataType = H5Dget_type(hdf5DatasetId);

      // Resize the element and material index vector to dataspace size.
      coord.clear();
      coord.resize(dim_out[0]);
      for (unsigned int varI = 0; varI < coord.size(); varI++)
      {
        coord[varI].resize(dim_out[1]);
      }

      rDebug("dim_out[0]: %d",dim_out[0]);
      rDebug("dim_out[1]: %d",dim_out[1]);

      // We copy the coordinates from coord in an standart array, so that 
      // hdf5 can handle it.
      // We do this in portions, so we save memory.
      
      // Subdimension of hyperslab in memory: 1 line, 3 columns.
      hsize_t dim_sub_mem[2];
      dim_sub_mem[0] = 1;
      dim_sub_mem[1] = dim_out[1];
      // Subdimension of hyperslab in file: 1 line, 3 columns.
      hsize_t dim_sub_file[2];
      dim_sub_file[0] = 1;
      dim_sub_file[1] = dim_out[1];
      // Define the offset for reading the file.
      hsize_t offset_file[2];
      offset_file[0] = 0;     // Row only at the beginning. We iterate over it.
      offset_file[1] = 0;     // We have no offset respective to the column.
      // Define the offset for writing into the memory.
      // Always fixed.
      hsize_t offset_mem[2];
      offset_mem[0] = 0;
      offset_mem[1] = 0;
      
      // Array for x, y and z component of one coordinate.
      double coordinate[dim_out[1]];

      // Loop over all rows of the dataset (file) and copy the coord vector
      // element wise.
      for (unsigned int varI = 0; varI < dim_out[0]; varI++)
      {
        // Iterate over the rows in file dataspace.
        offset_file[0] = varI;

        // Select hyperslab ('region') in file dataspace.
        hdf5Status = H5Sselect_hyperslab(hdf5DataspaceId, H5S_SELECT_SET,
                                         offset_file, H5P_DEFAULT,
                                         dim_sub_file, H5P_DEFAULT);

        // Define memory Dataspace.
        hid_t hdf5MemspaceId;
        hdf5MemspaceId = H5Screate_simple(rank, dim_sub_mem, H5P_DEFAULT);
        // Select hyperslab ('region') in memory.
        hdf5Status = H5Sselect_hyperslab(hdf5MemspaceId, H5S_SELECT_SET,
                                         offset_mem, H5P_DEFAULT,
                                         dim_sub_mem, H5P_DEFAULT);
        
        // Copy dataset to dataset from memory to file.
        hdf5Status = H5Dread(hdf5DatasetId, dataType,
                              hdf5MemspaceId, hdf5DataspaceId,
                              H5P_DEFAULT, coordinate);
        H5Sclose(hdf5MemspaceId);

        // Copy every element of a coordinate.
        for (unsigned int varJ = 0; varJ < dim_out[1]; varJ++)
        {
          coord[varI][varJ]=coordinate[varJ];
        }
     }

      // Close hdf5 identifier.
      hdf5Status = H5Dclose(hdf5DatasetId);
      hdf5Status = H5Sclose(hdf5DataspaceId);
      
      return OKCODE;
    }
    else 
    {
      rError("You cannot operate to dataset %s.",HDF5FED_D_COORD3D.c_str());
      rError("There is no valid file identifier.");
      return ERRORCODE;
    }
  };

  // Read and return tetrahedrons and respective material index of the
  // given level.
  int rTetrahedron(unsigned int level,
                   std::vector<std::vector<unsigned int> >& elem,
                   std::vector<unsigned int>& materialIndex)
  {
    // Set the name of the dataset, we want to read.
    std::string datasetName = HDF5FED_D_TETMESH;
    rElement_(datasetName, level, -1, elem, materialIndex);
    return OKCODE;
  };

  // Copy the tetrahedon elements to the hdf5fed filse.
  int wTetrahedron(unsigned int level,
                   std::vector< std::vector<unsigned int> >& elem,
                   std::vector<unsigned int>& materialIndex)
  {
    // Set dimension of an elements vector.
    unsigned int elemDim = HDF5FED_TET_N_NODE;
    // Set the name of the dataset, we want to operate.
    std::string datasetName = HDF5FED_D_TETMESH;
    // Select the data type in which the elements should be stored.
    hid_t dataType = HDF5FED_MESH_ELEM_DATATYPE;
    // This function does the real work for all elements.
    wElement_(datasetName, level, -1, elemDim, elem, materialIndex, dataType);
    return OKCODE;
  }
  
  int wTriangleB(unsigned int level,
                 unsigned int number,
                 std::vector< std::vector<unsigned int> >& elem,
                 std::vector<unsigned int>& boundaryIndex)
  {
    // Set dimension of an elements vector.
    unsigned int elemDim = HDF5FED_TRIANGLE_N_NODE;
    // Set the name of the dataset, we want to operate.
    std::string datasetName = HDF5FED_D_TRIANGLEBOUNDARY;
    // Select the data type in which the elements should be stored.
    hid_t dataType = HDF5FED_MESH_ELEM_DATATYPE;
    // This function does the real work for all elements.
    wElement_(datasetName, level, (int)number,  elemDim, elem, boundaryIndex, dataType);
    return OKCODE;
  }

  // Read and return boundary triangles and respective boundary index of the
  // given level and number.
  int rTriangleB(unsigned int level,
                 unsigned int number,
                 std::vector<std::vector<unsigned int> >& elem,
                 std::vector<unsigned int>& boundaryIndex)
  {
    // Set the name of the dataset, we want to read.
    std::string datasetName = HDF5FED_D_TRIANGLEBOUNDARY;
    rElement_(datasetName, level, (int)number, elem, boundaryIndex);
    return OKCODE;
  };

  
  // This function gets some attributes of the element, it should insert and
  // insert it to the given dataset.
  // Here we do the index mapping.
  // datasetNameBlank: the name of the elements dataset without the level!
  //                   (example: ../TETMESH_L)
  // level: the hierarchy level of the elements.
  // number: if there are different meshes on the same level (i.e. for
  //         boudaries, they are numbered. For no numbering choose
  //         number < 0.
  // elemDim: the number of nodes a single element has (tet = 4, line = 2, ..)
  // elem: the outer vector contains all elements,
  //       the inner vector contains the elements node numbers
  // materialIndex: the material index to an element.
  // elementType: this is the element type we use to store the elements and
  //              the material index in the file.
  int wElement_(std::string datasetNameBlank,
                unsigned int level,
                int number,
                unsigned int elemDim,
                std::vector< std::vector<unsigned int> >& elem,
                std::vector<unsigned int>& materialIndex,
                hid_t dataType)
  {
    // All these operations are only allowed, if there is a valid file
    // identifier.
    if (hdf5FileIdent_ >= 0)
    {
      // Make datasetName with the datasetNameBlank and the level.
      std::string datasetName = datasetNameBlank + stringify(level);
      if (number >= 0)
      {
        datasetName = datasetName + "_K" + stringify(number);
      }

      // Check if the dataset with the given datasetName already exists:
      // if it exists, abort; 
      // if not, check if the previouse level exists:
      //   if not, prompt warning
      //   if it exits, every thing is correct.
      if (existsDataspace(datasetName))
      {
        rError("Dataspace %s already exists!", datasetName.c_str());
        rError("You cannot insert it twice.");
        rError("Exit.");
        exit(ERRORCODE);
      }
      else
      {
        // We need no check for level 0.
        // If the level is higher than zero, check if levle-1 exists.
        if (level > 0)
        {
          if (!(existsDataspace(datasetNameBlank + stringify(level-1))))
          {
            rError("You insert level %d while level %d does not exists.",
                   level, level-1);
            rError("So the %s levels will be not consective.",
                   datasetNameBlank.c_str());
            rError("Exit.");
            exit(ERRORCODE);
          }
        }

        // Define the rank of the different dataspaces.
        const int rank = 2;
        // Hdf5 error handling variable for Hdf5 actions.
        herr_t hdf5Status;
        // Hdf5 dataspace identifier.
        hid_t hdf5DataspaceId;
        // Hdf5 dataset identifier.
        hid_t hdf5DatasetId;
        // Define the dimension of the data array.
        hsize_t dim[2];
        // Number of rows: as much as elements.
        dim[0] = elem.size();
        // Number of colums: dimenstion of an elements vector + 1.
        // We need the extra column as index in the material list!
        dim[1] = elemDim + 1;
        
        // We copy the element nodes from a element in an standart array,
        // so that hdf5 can handle it.
        // We do this for every element separate, so we save memory.
        
        // Subdimension of hyperslab in memory: 1 line, elemDim columns.
        hsize_t dim_sub_mem[2];
        dim_sub_mem[0] = 1;
        dim_sub_mem[1] = dim[1];
        // Subdimension of hyperslab in file: 1 line, elemDim columns.
        hsize_t dim_sub_file[2];
        dim_sub_file[0] = 1;
        dim_sub_file[1] = dim[1];
        // Define the offset for reading from the memory.
        // Always fixed, because memory has exactly the same size.
        hsize_t offset_mem[2];
        offset_mem[0] = 0;
        offset_mem[1] = 0;
        // Define the offset for writing into the file.
        hsize_t offset_file[2];
        offset_file[0] = 0;     // Row ofsett zero only at the beginning.
        offset_file[1] = 0;     // We have no offset respective to the column.
        
        // Array for single elements of an element vector.
        unsigned int element[dim[1]];
        
        // Dimesion and datatype of the file dataset, read out form file.
        // This is not the fastest, but the most robust way.
        hsize_t dim_out[2];
  
        // Create dataspace of full size.
        hdf5DataspaceId = H5Screate_simple(rank, dim, H5P_DEFAULT);
  
        // Create dataset of full size.
        hdf5DatasetId = H5Dcreate(hdf5FileIdent_, datasetName.c_str(), 
                                  dataType, hdf5DataspaceId, H5P_DEFAULT);
        
        // Loop over all rows of the dataset (file) and copy the elem vector
        // row wise.
        hdf5Status = H5Sget_simple_extent_dims(hdf5DataspaceId, dim_out,
                                               H5P_DEFAULT);
        // Get the datatype of the datas we want to write.
//        dataType = H5Dget_type(hdf5DatasetId);
        for (unsigned int varI = 0; varI < dim_out[0]; varI++)
        {
          // Iterate over the rows in file dataspace.
          offset_file[0] = varI;
          
          // Copy every element of a coordinate.
          // Remember: the last column is an index in the material list!
          for (unsigned int varJ = 0; varJ < dim_out[1] - 1; varJ++)
          {
            // Here is the only special case for automated mapping:
            // Do not copy the next in the input vector, copy the coord
            // with the next following number.
            if (doIndexMapping_ == true)
            {
              std::map<unsigned int, unsigned int>::iterator iter;
              iter = indexMap_.find((elem)[varI][varJ]);
              element[varJ] = iter->second;
            }
            else
              element[varJ] = (elem)[varI][varJ];
          }
          
          // Set the appropriate material list!
//          element[dim_out[1]-1] = materialIndex[varI];
          element[dim_out[1]-1] = 0;
  
          // Select hyperslab ('region') in file dataspace.
          hdf5Status = H5Sselect_hyperslab(hdf5DataspaceId, H5S_SELECT_SET,
                                           offset_file, H5P_DEFAULT,
                                           dim_sub_file, H5P_DEFAULT);
          // Define memory Dataspace.
          hid_t hdf5MemspaceId;
          hdf5MemspaceId = H5Screate_simple(rank, dim_sub_mem, H5P_DEFAULT);
          // Select hyperslab ('region') in memory.
          hdf5Status = H5Sselect_hyperslab(hdf5MemspaceId, H5S_SELECT_SET,
                                           offset_mem, H5P_DEFAULT,
                                           dim_sub_mem, H5P_DEFAULT);
          // Copy dataset to dataset from memory to file.
          hdf5Status = H5Dwrite(hdf5DatasetId, dataType,
                                hdf5MemspaceId, hdf5DataspaceId,
                                H5P_DEFAULT, element);
          // Close the memory space identifier.
          H5Sclose(hdf5MemspaceId);
       }
  
        // Close hdf5 identifier.
        hdf5Status = H5Dclose(hdf5DatasetId);
        hdf5Status = H5Sclose(hdf5DataspaceId);
        
        return OKCODE;
      }
    }
    else 
    {
      rError("You cannot operate to a dataset.");
      rError("There is no valid file identifier.");
      rError("Exit.");
      exit(ERRORCODE);
    }
  };

  // This function gets the level and name of an element in
  // a Hdf5fed file and returns the element nodes and material tag as a
  // vetor of vectors and a vector.
  // datasetNameBlank: the name of the elements dataset without the level!
  //                   (example: ../TETMESH_L)
  // level:            the hierarchy level of the elements
  // number: if there are different meshes on the same level (i.e. for
  //         boudaries, they are numbered. For no numbering choose
  //         number < 0.
  // elem: the outer vector gets all elements,
  //       the inner vectors get the elements node numbers.
  // materialIndex: the material index to an elemen.
  int rElement_(std::string datasetNameBlank,
                unsigned int level,
                int number,
                std::vector< std::vector<unsigned int> >& elem,
                std::vector<unsigned int>& materialIndex)
  {
    // All these operations are only allowed, if there is a valid file
    // identifier.
    if (hdf5FileIdent_ >= 0)
    {
      // Make datasetName with the datasetNameBlank and the level.
      std::string datasetName = datasetNameBlank + stringify(level);
      if (number >= 0)
      {
        datasetName = datasetName + "_K" + stringify(number);
      }

      // Check if the dataset with the given datasetName exists:
      // if it does not return errorcode, else continue. 
      if (!(existsDataspace(datasetName)))
      {
        rDebug("Dataspace %s does not exists!", datasetName.c_str());
        rDebug("So you cannot read.");
        return ERRORCODE;
      }
      else
      {
        // Define the rank of the different dataspaces.
        const int rank = 2;
        // Hdf5 error handling variable for Hdf5 actions.
        herr_t hdf5Status;

        // Hdf5 dataspace identifier.
        hid_t hdf5DataspaceId;

        // Hdf5 dataset identifier.
        hid_t hdf5DatasetId;
        // Open the hdf5 dataset.
        hdf5DatasetId = H5Dopen(hdf5FileIdent_, datasetName.c_str());
        
        // Get the dataspace from the dataset.
        hdf5DataspaceId = H5Dget_space(hdf5DatasetId);

        // Read the dimension of the file dataspace.
        hsize_t dim_out[2];
        hdf5Status = H5Sget_simple_extent_dims(hdf5DataspaceId, dim_out,
                                               H5P_DEFAULT);

        // Get the datatype of the datas we want to read.
        hid_t dataType;
        dataType = H5Dget_type(hdf5DatasetId);

        // Resize the element and material index vector to dataspace size.
        elem.clear();
        elem.resize(dim_out[0]);
        for (unsigned int varI = 0; varI < elem.size(); varI++)
        {
          elem[varI].resize(dim_out[1]);
        }
        materialIndex.clear();
        materialIndex.resize(dim_out[0]);
        
        // We copy a line from file to a standart array, and the array to a
        // stl::vector of vector so that hdf5 can handle it.
        // We do this for every element separate, so we save memory.
        
        // Subdimension of hyperslab in memory: 1 line, elemDim columns.
        hsize_t dim_sub_mem[2];
        dim_sub_mem[0] = 1;
        dim_sub_mem[1] = dim_out[1];
        // Subdimension of hyperslab in file: 1 line, elemDim columns.
        hsize_t dim_sub_file[2];
        dim_sub_file[0] = 1;
        dim_sub_file[1] = dim_out[1];
        // Define the offset for reading into the file.
        hsize_t offset_file[2];
        offset_file[0] = 0;     // Row ofsett zero only at the beginning.
        offset_file[1] = 0;     // We have no offset respective to the column.
        // Define the offset for writing to the memory.
        // Always fixed, because memory has exactly the same size.
        hsize_t offset_mem[2];
        offset_mem[0] = 0;
        offset_mem[1] = 0;
        
        // Array for single elements in memory.
        unsigned int element[dim_out[1]];
        
        // Loop over all rows of the dataset (file) and copy the elem vector
        // row wise.
        for (unsigned int varI = 0; varI < dim_out[0]; varI++)
        {
          // Iterate over the rows in file dataspace.
          offset_file[0] = varI;
          
          // Select hyperslab ('region') in file dataspace.
          hdf5Status = H5Sselect_hyperslab(hdf5DataspaceId, H5S_SELECT_SET,
                                           offset_file, H5P_DEFAULT,
                                           dim_sub_file, H5P_DEFAULT);
          // Define memory Dataspace.
          hid_t hdf5MemspaceId;
          hdf5MemspaceId = H5Screate_simple(rank, dim_sub_mem, H5P_DEFAULT);
          // Select hyperslab ('region') in memory.
          hdf5Status = H5Sselect_hyperslab(hdf5MemspaceId, H5S_SELECT_SET,
                                           offset_mem, H5P_DEFAULT,
                                           dim_sub_mem, H5P_DEFAULT);
          // Copy dataset to dataset from file to memory.
          hdf5Status = H5Dread(hdf5DatasetId, dataType,
                                hdf5MemspaceId, hdf5DataspaceId,
                                H5P_DEFAULT, element);
          // Close the memory space identifier.
          H5Sclose(hdf5MemspaceId);

          // Copy every element of a coordinate.
          // Remember: the last column is an index in the material list!
          for (unsigned int varJ = 0; varJ < dim_out[1]-1; varJ++)
          {
              elem[varI][varJ] =  element[varJ];
          }
          // Copy the material index in the appropriate vector.          
          materialIndex[varI] = element[dim_out[1]-1];
       }

        // Close hdf5 identifier.
        hdf5Status = H5Sclose(hdf5DataspaceId);
        hdf5Status = H5Dclose(hdf5DatasetId);
        
        return OKCODE;
      }
    }
    else 
    {
      rError("You cannot operate to a dataset.");
      rError("There is no valid file identifier.");
      rError("Exit.");
      exit(ERRORCODE);
    }
  };

  // Write 3dim coordinates to hdf5fed file.
  bool existsDataspace (std::string dataspaceName)
  {
    // All these operations are only allowed, if there is a valid file
    // identifier.
    if (hdf5FileIdent_ >= 0)
    {
      // Hdf5 error handling variable for Hdf5 actions.
      herr_t hdf5Status;
      // Hdf5 dataspace identifier.
      hid_t hdf5DataspaceId;

      hdf5DataspaceId = H5Dopen(hdf5FileIdent_, dataspaceName.c_str());
      if (hdf5DataspaceId < 0)
      {
        //rDebug("This dataspace exists NOT in this file: %s",
        //       dataspaceName.c_str());
        return false;
      }
      else
      {
        //rDebug("This dataspace exists in this file: %s",
        //       dataspaceName.c_str());
        return true;
      }
    }
    else 
    {
      rError("You cannot check if the dataspace exists.");
      rError("There is no valid file identifier.");
      return false;
    }
  }

		
  /** \brief Inquire existence of HDF5 groups */
//  existsVolumeMesh()
//  existsCoord()
//	virtual bool existsMesh(){ return(false); }
//	virtual bool existsVertices(){ return(false); }
//	virtual bool existsEdges(){ return(false); }
//	virtual bool existsTriangles(){ return(false); }
//	virtual bool existsBoundaryTriangles(){ return(false); }
			
//  /** \brief Data access both reading and writing */
//  virtual unsigned int nDim(){ return(ndim_); } /* retrieve number of mesh dimension */
//  virtual unsigned int nLevel(){ return(nlevel_); } /* retrieve number of mesh levels */
//  virtual unsigned int nCoord(){ return(nvertex_); } /* retrieve total number of all vertices */
//  virtual unsigned int nCoord(unsigned int level){ return(nvertex_); } /* retrieve total number of vertices used for defining the mesh */
//        Überladen greifen auf nelem zu
//  virtual std::vector<unsigned int> nTet(){ return(ntet_); } /* retrieve vector containing number of tetrahedra present in levels of the mesh */
//  virtual std::vector<unsigned int> nBoundary(){ return(nboundary_); } /* retrieve vector containing number of boundary meshes present on a level of the mesh */
//          Pism, ...
//        nBoundaryTriangle
//                 Quad..
//            Edge
 
	
	/** \brief HDF5EM data access routines */
		
	/** \brief Electromagnetic boundary conditions */
//_______________________________
//	virtual int triangleBC(unsigned int level, unsigned int triangle, unsigned int bc){} /* set electromagnetic boundary condition for triangle on level */
//	virtual unsigned int triangleBC(unsigned int level, unsigned int triangle){} /* retrieve electromagnetic boundary condition type */
//
//	virtual int rhoSurfaceImpedance(unsigned int level, unsigned int triangle, double rho){} /* store resistance of surface impedance for triangle on level */
//	virtual double rhoSurfaceImpedance(unsigned int level, unsigned int triangle){} /* retrieve resistance of surface impedance boundary condition */
//
//	/** \brief Mesh */
//        stl vectoren
//	virtual unsigned int** tetrahedron(unsigned int level){} /* read tetrahedra on level l stored in hdf5 file and return them in simple form */
//	virtual int tetrahedron(unsigned int level, unsigned int ntetrahedron, unsigned int** tetrahedron){} 
//	virtual unsigned int* vertexId(){} /* read identification tag of vertices */
//
//          virtual double** vertex(unsigned int level){} /* read vertices */
//	virtual int vertex(unsigned int nvertex, double** vertex){} /* write vertices */
//
//	virtual unsigned int** boundary(unsigned int level, unsigned int wboundary){} /* read boundary meshes stored in the hdf5 file on level and return number of them */ 
//	virtual int boundary(unsigned int level, unsigned int** boundary, unsigned int wboundary){} /* write whichboundary boundary mesh into file */ 
//
//	virtual unsigned int** edge(unsigned int level, aqmedge* &edge){} /* read edges on level and return number of them */
//	virtual int edge(unsigned int level, unsigned int* edge){} /* write edges on level and return number of them */
//
//	virtual unsigned int** triangle(unsigned int level){} /* read Triangular faces on level */
//	virtual int triangle(unsigned int level, unsigned int** triangle){} /* write Triangular faces on level */
//	
//	/** \brief Materials */
//	bool existsMaterials() /* find out if there are materials parameters at discrete frequencies, energies or wavelength */
//			
//	vector<double> frequency(){} /* read a vector of discrete frequencies stored in the file */
//	int frequency(vector<double> f){} /* store a vector of discrete frequencies into the file */
//				
//			vector<double> energy(){} /* read a vector of discrete energies stored in the file */
//			int energy(vector<double> e){} /* store a vector of discrete wavelengths into the file */
//
//			vector<double> wavelength(){} /* read a vector of discrete wavelengths stored in the file */
//			int wavelength(vector<double> lambda){} /* store a vector of discrete wavelengths into the file */
//
//			vector< complex<double> > permittivity(){} /* read a vector of complex valued relative permittivities from the file */
//			int permittivity( vector< complex<double> > epsilonr){} /* store a vector of complex valued relative permittivities into the file */ 
//
//			vector< complex<double> > permeability(){} /* read a vector of complex valued relative permittivities from the file */
//			int permeability( vector< complex<double> > mur){} /* store a vector of complex valued relative permittivities into the file */ 
//
//			vector< complex<double> > conductivity(){} /* read a vector of complex valued relative permittivities from the file */
//			int conductivity( vector< complex<double> sigma>){} /* store a vector of complex valued relative permittivities into the file */ 
//
//		/** \brief Materials */
//			bool existsDebyeMaterial(){} /* are the Debye material model paramters */
//			bool existsLorentz(){} /* are there Lorentz material model parameters */
//			bool existsDrudeMaterial(){} /* are there Drude material model parameters */
//			
//			/* Debye dielectric material model parameters, cf. Taflove et al. pp. 354 */
//			std<double> weightsDebye(){} /* retrieve vector of Debye material model weights */
//			int weightsDebye(vector<double> weightsdebye){} /* store vector of Debye material model weights */
//
//			std<double> relaxationFrequencyDebye(){} /* retrieve vector of Debye material model relaxation frequencies */
//			int relaxationFrequencyDebye(vector<double> relaxfreq){} /* retrieve vector of Debye material model relaxation frequencies */
//								
//			double epsilonStaticDebye(){} /* retrieve static limit of relative permittivitiye */
//			int epsilonStaticDebye(double epsilonstaticr){} /* store static limit of relative permittivitiye */
//			
//			double epsilonInfinityDebye(){} /* retrieve infinite limit of relative permittivity */
//			int epsilonInfinityDebye(double epsiloninftyr){} /* store infinite limit of relative permittivity */
//			
//			/* Lorentz dielectric material model parameters, cf. Taflove et al. pp. 354 */
//			std<double> weightsLorentz(){} /* retrieve vector of Debye material model weights */
//			int weightsLorentz(vector<double> weightsdebye){} /* store vector of Debye material model weights */
//
//			std<double> poleFrequencyLorentz(){} /* retrieve vector of Lorentz material model relaxation frequencies */
//			int poleFrequencyDebye(vector<double> polefreq){} /* retrieve vector of Lorentz material model relaxation frequencies */
//						
//			std<double> dampingCoefficientLorentz(){} /* retrieve vector of Lorentz material model damping coefficients */
//			int dampingCoefficientLorentz(vector<double> dampingcoeff){} /* retrieve vector of Lorentz material model damping coefficients */
//						
//			double epsilonStaticLorentz(){} /* retrieve static limit of relative permittivitiye */
//			int epsilonStaticLorentz(double epsilonstaticr){} /* store static limit of relative permittivitiye */
//			
//			double epsilonInfinityLorentz(){} /* retrieve infinite limit of relative permittivity */
//			int epsilonInfinityLorentz(double epsiloninftyr){} /* store infinite limit of relative permittivity */
//	
//			/* Drude dielectric material model parameters, cf. Taflove et al. pp. 354 */
//			std<double> weightsDrude(){} /* retrieve vector of Drude material model weights */
//			int weightsDrude(vector<double> weightsdebye){} /* store vector of Drude material model weights */
//
//			std<double> poleFrequencyDrude(){} /* retrieve vector of Drude material model pole frequencies */
//			int poleFrequencyDrude(vector<double> relaxfreq){} /* retrieve vector of Drude material model pole frequencies */
//						
//			std<double> inversePoleRelaxationTimeDrude(){} /* retrieve vector of Lorentz material model inverse pole relaxation time */
//			int inversePoleRelaxationTimeDrude(vector<double> inversepolerelaxtime){} /* retrieve vector of Lorentz material model inverse pole relaxation time */
//						
//			double epsilonStaticDrude(){} /* retrieve static limit of relative permittivitiye */
//			int epsilonStaticDrude(double epsilonstaticr){} /* store static limit of relative permittivitiye */
//			
//			double epsilonInfinityDrude(){} /* retrieve infinite limit of relative permittivity */
//			int epsilonInfinityDRude(double epsiloninftyr){} /* store infinite limit of relative permittivity */
//________________________________________________________________
//		protected:
//		
private:
  //-----------------------------------------------------------------------//
  // Private data structure.                                               //
  //-----------------------------------------------------------------------//
  // Store the filename of the Hdf5fed here.
  std::string fileName_;
  // Store the file access rights of the Hdf5fed here.
  std::string fileAccess_;

  // Hdf5 error variable stores the success of an Hdf5 action.
  herr_t hdf5Status_;
  // Hdf5 file identifier for file access. If a file access fails, the 
  // identifyer is negetive.
  hid_t hdf5FileIdent_;
  
  // If the elements we want to write to the hdf5fed file are not consecutive
  // and with gaps numbered, this API shoud be able to map this to an gapfree
  // and consecutive index set.
  // To activate this function we have the following variable.
  bool doIndexMapping_;
  // The first entry in indexMap_ is the old index of a coordinate, the
  // second is the new index usend in the hdf5fed file.
  std::map<unsigned int, unsigned int> indexMap_;
  // The first entry in positionMap_ is the new index usend in the hdf5fed
  // file, the second is the old postion in the coordinate vector.
  std::map<unsigned int, unsigned int> positionMap_;

  //-----------------------------------------------------------------------//
  // Private helper functions.                                             //
  //-----------------------------------------------------------------------//
  // This function converts a number to a string.
  template<typename type>
  inline std::string stringify(type value)
  {
    std::ostringstream oStream;
    try
    {
      oStream << value;
    }
    catch(std::exception& error)
    {
      rError("Cannot convert this variable to a string.");
      rError("Error: %d",error.what());
      exit(ERRORCODE);
    }  
    return oStream.str();
  }

			
};

} // End of namespace Hdf5fed

#elif
#warning "No HDF5 found. You cannot use hdf5fed."
#endif // HAVE_HDF5
#endif //HDF5FED_HH_
