// rights - 2006-, copyright patrick leidenberger and benedikt oswald, 
//                 all rights reserved
// project   - gmsh2h5fed
// file name - gmsh.hh
// file type - c++ header file
// objective - declare class for readind gmsh mesh files v2.0
//
// modified  - 2006 aug 21, creation, Patrick Leidenberger
// modified  - 2006 aug 22, pl.
// modified  - 2006 aug 23, pl.
// modified  - 2006 aug 24, pl, delete old things.
// modified  - 2006 aug 26, pl, Access to private members.

#ifndef GMSH_HH_
#define GMSH_HH_

#include <iostream>
#include <string>
#include <vector>
#include <boost/spirit/core.hpp>
#include <boost/spirit/iterator/file_iterator.hpp>
#include <boost/spirit/core.hpp>

/* Include the files for rlog. */
#include <rlog/rlog.h>
#include <rlog/rloglocation.h>
#include <rlog/Error.h>
#include <rlog/RLogChannel.h>
#include <rlog/StdioNode.h>
#include <rlog/RLogTime.h>

#include <nonsciconst.h>

// Include gmsh specific constants.
#include "gmshconst.hh"

using namespace std;
using namespace nonsciconst;

namespace gmshtohdf5fed
{
  class gmsh
  {
    public:
      // Constructor: Here we clear all private data vectors.
      gmsh();
      // Destructor.
      ~gmsh();
        
      int gmshFileName(char* fileName);
      int gmshFileName(string fileName);

      int gmshOpen(void);
      
      int gmshParseFile(gmsh* vtkSelf);
      
      void cpString(char const* first,
                    char const* last,
                    string* str);
      string cpString(char const* first,
                      char const* last);

      int gmshCpString(char const* first,
                       string*str);
      // Check if gmsh file is of version 2.
      int gmshCheckVersion(int* version);
      // Check if the gmsh file is in ascii format.
      int gmshCheckFileType(int* fileType);

      // Pushes the three coordinates of a node in the gmshNodes_ vector.
      int gmshPushNode(vector<double>* nodeCoords);

      // Pushes the number of a node in the gmshNodesNumber_ vector.
      int gmshPushNodeNumber(unsigned int * nodeNumber);

      // Push the node vector nodeVector to the appropriate data structure 
      // denoted by the string.
      int gmshPushElem(unsigned short int elemType, 
                       vector<unsigned int>* nodeVector);
      // Push the tag vector tagVector to the appropriate data structure 
      // denoted by the string.
      int gmshPushTag(unsigned short int elemType, 
                       vector<unsigned int>* tagVector);
      // Show the size of the element vectors.
      int gmshShowElemResult();

      //-------------------------------------//
      // Access to private stored gmsh data. //
      //-------------------------------------//

      std::vector< std::vector <double> > gmshNode();
      std::vector<unsigned int> gmshNodeNumber();
      std::vector< std::vector<unsigned int> > gmshTetrahedron();
      std::vector< std::vector<unsigned int> > gmshTetrahedronTag();

    private:
      string fileName_;
      string fileContent;

      // Data structure for node coordinates from gmsh file.
      vector<vector<double> > gmshNodes_;
      // Data structure for gmsh node number. 
      // A number belongs to the respective node coordinate in the gmshNodes_
      // vector.
      vector<unsigned int> gmshNodesNumber_;

      // Data structure for the topological entities and their tags stored in
      // the gmsh  file. The 'gmsh2nd..' denote second order Elements.
      vector<vector<unsigned int> > gmshLine_;
      vector<vector<unsigned int> > gmshLineTag_;
      vector<vector<unsigned int> > gmshTriangle_;
      vector<vector<unsigned int> > gmshTriangleTag_;
      vector<vector<unsigned int> > gmshQuadrangle_;
      vector<vector<unsigned int> > gmshQuadrangleTag_;
      vector<vector<unsigned int> > gmshTetrahedron_;
      vector<vector<unsigned int> > gmshTetrahedronTag_;
      vector<vector<unsigned int> > gmshHexahedron_;
      vector<vector<unsigned int> > gmshHexahedronTag_;
      vector<vector<unsigned int> > gmshPrism_;
      vector<vector<unsigned int> > gmshPrismTag_;
      vector<vector<unsigned int> > gmshPyramid_;
      vector<vector<unsigned int> > gmshPyramidTag_;
      vector<vector<unsigned int> > gmsh2ndLine_;
      vector<vector<unsigned int> > gmsh2ndLineTag_;
      vector<vector<unsigned int> > gmsh2ndTriangle_;
      vector<vector<unsigned int> > gmsh2ndTriangleTag_;
      vector<vector<unsigned int> > gmsh2ndQuadrangle_;
      vector<vector<unsigned int> > gmsh2ndQuadrangleTag_;
      vector<vector<unsigned int> > gmsh2ndTetrahedron_;
      vector<vector<unsigned int> > gmsh2ndTetrahedronTag_;
      vector<vector<unsigned int> > gmsh2ndHexahedron_;
      vector<vector<unsigned int> > gmsh2ndHexahedronTag_;
      vector<vector<unsigned int> > gmsh2ndPrism_;
      vector<vector<unsigned int> > gmsh2ndPrismTag_;
      vector<vector<unsigned int> > gmsh2ndPyramid_;
      vector<vector<unsigned int> > gmsh2ndPyramidTag_;
      vector<vector<unsigned int> > gmshPoint_;
      vector<vector<unsigned int> > gmshPointTag_;
  };
}

#endif //GMSH_HH_
