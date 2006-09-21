// rights - 2006-, copyright patrick leidenberger and benedikt oswald,
//                 all rights reserved
// project   - gmsh2h5fed
// file name - gmsh.cc
// file type - c++ implementation file
// objective - implement class for readind gmsh data files
// modified  - 2006 aug 24, creation, patrick leidenberger
// modified  - 2006 aug 26, pl, Access to private members.

#include "gmsh.hh"

using namespace boost::spirit;
using namespace gmshtohdf5fed;
using namespace rlog;
using namespace std;

#include <boost/spirit/core.hpp>
#include <boost/spirit/iterator/file_iterator.hpp>
#include <iostream>
#include <sstream>
#include <fstream>
#include <string>
#include <vector>

#include "gmshgrammar.hh"
#include "gmshsemanticaction.hh"


gmsh::gmsh()
{
  //rDebug("Enter vtk constructor.");
  // Clean the gmsh data structure.
  gmshNodes_.clear();
  gmshNodesNumber_.clear();
  gmshLine_.clear();
  gmshLineTag_.clear();
  gmshTriangle_.clear();
  gmshTriangleTag_.clear();
  gmshQuadrangle_.clear();
  gmshQuadrangleTag_.clear();
  gmshTetrahedron_.clear();
  gmshTetrahedronTag_.clear();
  gmshHexahedron_.clear();
  gmshHexahedronTag_.clear();
  gmshPrism_.clear();
  gmshPrismTag_.clear();
  gmshPyramid_.clear();
  gmshPyramidTag_.clear();
  gmsh2ndLineTag_.clear();
  gmsh2ndTriangle_.clear();
  gmsh2ndTriangleTag_.clear();
  gmsh2ndQuadrangle_.clear();
  gmsh2ndQuadrangleTag_.clear();
  gmsh2ndTetrahedron_.clear();
  gmsh2ndTetrahedronTag_.clear();
  gmsh2ndHexahedron_.clear();
  gmsh2ndHexahedronTag_.clear();
  gmsh2ndPrism_.clear();
  gmsh2ndPrismTag_.clear();
  gmsh2ndPyramid_.clear();
  gmsh2ndPyramidTag_.clear();
  gmshPoint_.clear();
  gmshPointTag_.clear();
  //rDebug("Leafe vtk constructor.");
}

gmsh::~gmsh()
{
  //rDebug("Enter vtk destructor.");
  //rDebug("Leafe vtk destructor.");
}


int gmsh::gmshFileName(char* fileName)
{
  //rDebug("Enter fileName.");
  // Erase the fileName_. 
  fileName_.erase();
  // Copy fileName in private string fileName_.
  gmshCpString(fileName, &fileName_);
  // rDebug("fileName_= %s",fileName_.c_str());
  //rDebug("Leafe fileName");
  return OKCODE;
}
int gmsh::gmshFileName(string fileName)
{
  //rDebug("Enter fileName.");
  // Erase the fileName_. 
  fileName_.erase();
  // Copy fileName in private string fileName_.
  fileName_ = fileName;
  // rDebug("fileName_= %s",fileName_.c_str());
  //rDebug("Leafe fileName");
}


int gmsh::gmshOpen(void)
{
  //rDebug("Enter openVtk.");

  // Open a stream to read the gmsh file in a string. 
  ifstream inFile (fileName_.c_str());

  // Check if file was opened corretly.
  if (! inFile)
  {
    rError("Cannot open file %s.", fileName_.c_str());
    return ERRORCODE;
  }
  else
  {
    rInfo("Open file %s", fileName_.c_str());
    
    // Read file content into a string
    fileContent.erase();
    char chr;
    while (inFile.get(chr))
    {
      fileContent.append(1,chr);
    }
    rInfo("  File has been read.");
    //rDebug("Input file:\n%s",fileContent.c_str());
    // Close input file.
    inFile.close();
    rInfo("  File Closed.");
    return OKCODE;
  }
  //rDebug("Leave openVtk.");
}


int gmsh::gmshParseFile(gmsh* gmshSelf)
{
  //rDebug("Enter parseFile.");

  // Provide an instance of the grammar describing the gmsh file format.
  stack<long> gmshStack;
  // Our vtk parser.
  // Give the parser a pointer to the instance of the gmsh class. So we can
  // call member functions as semantic action.
  gmsh_g gmsh_p(gmshSelf);

  // Call the parser.
  parse_info <iterator_t> info = parse(fileContent.c_str(),gmsh_p,space_p);

  // Feedback to user on parsing outcome.
  if (info.full)
  {
    rInfo("-------------------------");
    rInfo("parsing succeeded :-)");
    rInfo("-------------------------");
    return OKCODE;
  }
  else
  {
    ostringstream temp;
    temp << info.stop ;
    rError("-------------------------");
    rError("parsing failed :-(");
    rError("stopped at line: %s", temp.str().c_str());
    rError("-------------------------");
    return ERRORCODE;
  }
    
  //rDebug("Leave parseFile.");
}

int gmsh::gmshPushNode(vector<double>* nodeCoords)
{
  //rDebug("gmshPushNode(vector<double>* nodeCoords).");
  gmshNodes_.push_back(*nodeCoords);
  return OKCODE;
}

int gmsh::gmshPushNodeNumber(unsigned int* nodeNumber)
{
  gmshNodesNumber_.push_back(*nodeNumber);
  return OKCODE;
}

int gmsh::gmshPushElem(unsigned short int elemType, 
                       vector<unsigned int>* nodeVector)
{
  //rDebug("Inside gmshPushElem.");
  if (elemType == GMSH_LINE)
    gmshLine_.push_back(*nodeVector);
  else if (elemType == GMSH_TRIANGLE)
    gmshTriangle_.push_back(*nodeVector);
  else if (elemType == GMSH_QUADRANGLE)
    gmshQuadrangle_.push_back(*nodeVector);
  else if (elemType == GMSH_TETRAHEDRON)
    gmshTetrahedron_.push_back(*nodeVector);
  else if (elemType == GMSH_HEXAHEDRON)
    gmshHexahedron_.push_back(*nodeVector);
  else if (elemType == GMSH_PRISM)
    gmshPrism_.push_back(*nodeVector);
  else if (elemType == GMSH_PYRAMID)
    gmshPyramid_.push_back(*nodeVector);
  else if (elemType == GMSH_2ND_LINE)
    gmsh2ndLine_.push_back(*nodeVector);
  else if (elemType == GMSH_2ND_TRIANGLE)
    gmsh2ndTriangle_.push_back(*nodeVector);
  else if (elemType == GMSH_2ND_QUADRANGLE)
    gmsh2ndQuadrangle_.push_back(*nodeVector);
  else if (elemType == GMSH_2ND_TETRAHEDRON)
    gmsh2ndTetrahedron_.push_back(*nodeVector);
  else if (elemType == GMSH_2ND_HEXAHEDRON)
    gmsh2ndHexahedron_.push_back(*nodeVector);
  else if (elemType == GMSH_2ND_PRISM)
    gmsh2ndPrism_.push_back(*nodeVector);
  else if (elemType == GMSH_2ND_PYRAMID)
    gmsh2ndPyramid_.push_back(*nodeVector);
  else if (elemType == GMSH_POINT)
    gmshPoint_.push_back(*nodeVector);
  else 
  {
    rError("A unknown element type found!");
    rError("Element type: %d", elemType);
  }
  return OKCODE;
}

int gmsh::gmshPushTag(unsigned short int elemType, 
                       vector<unsigned int>* tagVector)
{
  //rDebug("Inside gmshPushElem.");
  if (elemType == GMSH_LINE)
  {
    //rDebug("  Line pushedback");
  }
  else if (elemType == GMSH_TRIANGLE)
  {
    //rDebug("  Triangle pushedback");
  }
  else if (elemType == GMSH_QUADRANGLE)
  {
    //rDebug("  Quadrangle pushedback");
  }
      else if (elemType == GMSH_TETRAHEDRON)
  {
    //rDebug("  Tetrahedron pushedback");
    gmshTetrahedronTag_.push_back(*tagVector);
  }
  else 
  { 
  }
  return OKCODE;
}


int gmsh::gmshShowElemResult()
{
  rInfo("Elements found in gmsh file:");
  rInfo("  lines:                  %d",gmshLine_.size());
  rInfo("  triangles:              %d",gmshTriangle_.size());
  rInfo("  quadrangles:            %d",gmshQuadrangle_.size());
  rInfo("  tetrahedrons:           %d",gmshTetrahedron_.size());
  rInfo("  hexahedrons:            %d",gmshHexahedron_.size());
  rInfo("  prisms:                 %d",gmshPrism_.size());
  rInfo("  pyramids:               %d",gmshPyramid_.size());
  rInfo("  2nd order lines:        %d",gmsh2ndLine_.size());
  rInfo("  2nd order Triangles:    %d",gmsh2ndTriangle_.size());
  rInfo("  2nd order Quadrangles:  %d",gmsh2ndQuadrangle_.size());
  rInfo("  2nd order Tetrahedrons: %d",gmsh2ndTetrahedron_.size());
  rInfo("  2nd order Hexahedrons:  %d",gmsh2ndHexahedron_.size());
  rInfo("  2nd order Prisms:       %d",gmsh2ndPrism_.size());
  rInfo("  2nd order Pyramids:     %d",gmsh2ndPyramid_.size());
  rInfo("  Points            :     %d",gmshPoint_.size());
  return OKCODE;
}

void gmsh::cpString(char const* first,
                   char const* last,
                   string* str)
{
  //rDebug("Enter cpString.");
  
  // Erase the str content. 
  str->erase();
  
  // Copy version in str.
  for(unsigned int i = 0; i<strlen(first)-strlen(last); i++)
  {
    str->append(1,first[i]);
  }
  //rDebug("*str= %s",str->c_str());

  //rDebug("Leafe cpString.");
}
string gmsh::cpString(char const* first,
                   char const* last)
{
  //rDebug("Enter cpString.");
  
  // Create and erase a tempral string;
  string tempString;
  tempString.erase();
  
  // Copy version in str.
  for(unsigned int i = 0; i<strlen(first)-strlen(last); i++)
  {
    tempString.append(1,first[i]);
  }
  //rDebug("tempString= %s",tempString.c_str());
  //rDebug("Leafe cpString.");
  return tempString;
}

int gmsh::gmshCpString(char const* first,
                       string*str)
{
  // Create and erase a tempral string;
  std::string tempString;
  tempString.erase();
  // Copy the character sign for sign into the string.
  for(unsigned int i = 0; i<strlen(first); i++)
  {
    tempString.append(1,first[i]);
  }
  //rDebug("tempString= %s",tempString.c_str());
  //rDebug("Leafe cpString.");
  return OKCODE;
}

int gmsh::gmshCheckVersion(int* version)
{
  if ((int)(*version) == 2)
  {
    rInfo("Gmsh file has format 2.");
    return OKCODE;
  }
  else
  {
    rError("Gmsh file has format: %d.", (int)(*version));
    rError("But this program can only read format 2.");
    rError("Exit");
    exit(ERRORCODE);
  }
}

int gmsh::gmshCheckFileType(int* fileType)
{
  if ((int)(*fileType) == 0)
  {
    rInfo("Gmsh file has ascii format.");
    return OKCODE;
  }
  else
  {
    rError("Gmsh file has format: %d.", (int)(*fileType));
    rError("But this program can only read format 0 (ascii).");
    rError("Exit");
    exit(ERRORCODE);
  }
}

std::vector< std::vector <double> > gmsh::gmshNode()
{
  return gmshNodes_;
}

std::vector<unsigned int> gmsh::gmshNodeNumber()
{
  return gmshNodesNumber_;
}

std::vector< std::vector<unsigned int> > gmsh::gmshTetrahedron()
{
  return gmshTetrahedron_;
}

std::vector< std::vector<unsigned int> > gmsh::gmshTetrahedronTag()
{
  return gmshTetrahedronTag_;
}
