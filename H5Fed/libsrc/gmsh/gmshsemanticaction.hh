//  rights - 2006-, copyright benedikt oswald and patrick leidenberger, 
//                  all rights reserved
/** \brief Definition of VTK ASCII file format parser
  * project   - phidias3d
  * file name - vtksemanticaction.h
  * file type - c++ include file
  * objective - declare semantic action for the vtk file parser
  * modified  - 2006 jul 03, creation, pl
  * modified  - 2006 jul 05, insert fuctionality of calling member functions
  *                          of the vtk class, pl
  * modified  - 2006 jul 06, Add actions for vtk file header and file format.
  * modified  - 2006 jul 11, Add a lot of action to store the unstructured
  *                          grid dataset. This section is complete.
  * modified  - 2006 jul 13, Make the action work well: it is not a good idea
  *                          to transport pointers or strings! Use pointers of
  *                          characters.
  * modified  - 2006 aug 27, Correct error in pushNode function, pl.
  **/


/* include standard header files */
#include <boost/spirit/core.hpp>
#include <boost/spirit/iterator/file_iterator.hpp>
#include <iostream>
//#include <iterator>
#include <string>
#include <vector>
#include <map>
#include <functional>
#include <stack>
#include "gmsh.hh"

/* include proprietary header files */
//#include "ansyscnx.h"

/* activate namespaces */
using namespace std;
using namespace boost;
using namespace boost::spirit;
//using namespace phidias3d;

using namespace gmshtohdf5fed;


/* type definitions */
typedef char                    char_t;
typedef file_iterator<char_t>   iterator_t;
typedef scanner<iterator_t>     scanner_t;
typedef rule<scanner_t>         rule_t;

#ifndef GMSHSEMANTICACTION_HH_
#define GMSHSEMANTICACTION_HH_

/* semantic actions */

// Call version-check of gmsh file.
struct gmsh_version_a
{
  gmsh_version_a(gmsh* gmshSelf, int* tempInt)
  : gmshSelf_(gmshSelf), tempInt_(tempInt) {}
  void operator()(int value) const
  {
    //rDebug("Inside gmsh_version_a.");
    gmshSelf_->gmshCheckVersion(tempInt_);
  };
  gmsh* gmshSelf_;
  int* tempInt_;
};

struct gmsh_file_type_a
{
  gmsh_file_type_a(gmsh* gmshSelf, int* tempInt)
  : gmshSelf_(gmshSelf), tempInt_(tempInt) {}
  void operator()(int value) const
  {
    //rDebug("Inside gmsh_version_a.");
    gmshSelf_->gmshCheckFileType(tempInt_);
  };
  gmsh* gmshSelf_;
  int* tempInt_;
};

template<typename Type>
struct gmsh_clear_vec_a
{
  gmsh_clear_vec_a(vector<Type>* vect)
  : vect_(vect) {}
  void operator()(char const* first, char const* last)
    const
  {
    //rDebug("Clear temporal vector.");
    (*vect_).clear();
  };
  vector<Type>* vect_;
};

template<typename type>
struct gmsh_resize_vec_a
{
  gmsh_resize_vec_a(vector<type>* vec, unsigned int size)
  : vec_(vec), size_(size) {}
  void operator() (char const* first,char const* last)
    const
  {
    //rDebug("Inside gmsh_resize_vec_a.");
    (*vec_).resize(size_);
  };
  void operator() (const int tempInt)
    const
  {
    //rDebug("Inside gmsh_resize_vec_a.");
    (*vec_).resize(size_);
  };
  unsigned int size_;
  vector<type> * vec_;
};

template<typename type>
struct gmsh_insert_vec_a
{
  gmsh_insert_vec_a(vector<type>* vec, unsigned int const position, 
                    type* value)
  : vec_(vec), position_(position), value_(value) {}
  void operator() (char const* first,char const* last)
    const
  {
    (*vec_)[position_]=*value_;
  }
  vector<type> * vec_;
  unsigned int position_;
  type* value_;
};


template<typename type>
struct gmsh_push_vec_a
{
  gmsh_push_vec_a(vector<type>* vec, type* value)
  : vec_(vec), value_(value){}
  void operator() (char const* first,char const* last)
    const
  {
    //rDebug("  Push value to  vector.");
    (*vec_).push_back(*value_);
  };
  void operator() (const double tempDouble)
    const
  {
    //rDebug("  Push value to  vector.");
    (*vec_).push_back(*value_);
  };
  type* value_;
  vector<type> * vec_;
};


template<typename type1, typename type2>
struct gmsh_insert_map_a
{
  gmsh_insert_map_a(map<type1, type2>* tempMap, type1* key, 
                    type2* value)
  : map_(tempMap), key_(key), value_(value) {}
  void operator() (char const* first,char const* last)
    const
  {
   (*map_)[(*key_)]=*value_;
  };
  map<type1, type2> * map_;
  type1* key_;
  type2* value_;
};

template<typename type1, typename type2>
struct gmsh_clear_map_a
{
  gmsh_clear_map_a(map<type1, type2>* tempMap)
  : map_(tempMap) {}
  void operator() (char const* first,char const* last)
    const
  {
    (*map_).clear();
  };
  map<type1, type2> * map_;
};


struct gmsh_transport_nodes_a
{
  gmsh_transport_nodes_a(gmsh* gmshSelf, 
                         map<unsigned int, vector<double> >* tempMap)
  : gmshSelf_(gmshSelf), map_(tempMap) {}
  void operator() (char const* first,char const* last)
    const
  {
    //rDebug("Copy nodes in data structure.");
    for (map<unsigned int, vector<double> >::iterator 
           iter = (*map_).begin(); iter != (*map_).end(); iter++)
    {
      vector<double>* temp;
      unsigned int tempNumber;
      temp = &((*iter).second);
      tempNumber = ((*iter).first);
      gmshSelf_->gmshPushNode(temp);
      gmshSelf_->gmshPushNodeNumber(&tempNumber);
      //rDebug("%f %f %f", (*temp)[0], (*temp)[1], (*temp)[2]);
    }
    
  };
  map<unsigned int, vector<double> >* map_;
  gmsh* gmshSelf_;
};



struct gmsh_transport_elem_a
{
  gmsh_transport_elem_a(gmsh* gmshSelf, unsigned short int* elemType,
                        vector <unsigned int>* tempElemNode)
  : gmshSelf_(gmshSelf), elemType_(elemType), tempElemNode_(tempElemNode) {}
  void operator() (char const* first,char const* last)
    const
  {
    //rDebug("Copy element type %d in data structure.", *elemType_);
    gmshSelf_->gmshPushElem(*elemType_, tempElemNode_);
  };
  gmsh* gmshSelf_;
  unsigned short int* elemType_;
  vector <unsigned int>* tempElemNode_;
};


struct gmsh_transport_tag_a
{
  gmsh_transport_tag_a(gmsh* gmshSelf, unsigned short int* elemType,
                        vector <unsigned int>* tempElemTag)
  : gmshSelf_(gmshSelf), elemType_(elemType), tempElemTag_(tempElemTag) {}
  void operator() (char const* first,char const* last)
    const
  {
    //rDebug("Copy element tag list in data structure.");
    gmshSelf_->gmshPushTag(*elemType_, tempElemTag_);
  };
  gmsh* gmshSelf_;
  unsigned short int* elemType_;
  vector <unsigned int>* tempElemTag_;
};


// Print actions.
struct gmshEMsg_a
{
  gmshEMsg_a(string gmshEMsg, unsigned int* tempUnLongInt)
  : gmshEMsg_(gmshEMsg), tempUnLongInt_(tempUnLongInt) {}
  void operator()(char const* first, char const* last) 
    const
  {
    rError("%s %d",gmshEMsg_.c_str(),(*tempUnLongInt_));
  };
  string gmshEMsg_;
  unsigned int* tempUnLongInt_;
};

struct gmsh_info_msg_a
{
  gmsh_info_msg_a(string gmsh_info_msg)
  : gmsh_info_msg_(gmsh_info_msg), tempUnLongInt_(NULL), tempUnShortInt_(NULL) {}
  gmsh_info_msg_a(string gmsh_info_msg, unsigned int* tempUnLongInt)
  : gmsh_info_msg_(gmsh_info_msg), tempUnLongInt_(tempUnLongInt), tempUnShortInt_(NULL) {}
  gmsh_info_msg_a(string gmsh_info_msg, unsigned short int* tempUnShortInt)
  : gmsh_info_msg_(gmsh_info_msg), tempUnLongInt_(NULL), tempUnShortInt_(tempUnShortInt) {}
  void operator()(char const* first, char const* last) 
    const
  {
    if ((tempUnLongInt_ == NULL) && (tempUnShortInt_ == NULL))
      rInfo("%s",gmsh_info_msg_.c_str());
    else if ((tempUnLongInt_ != NULL) && (tempUnShortInt_ == NULL))
      rInfo("%s%d",gmsh_info_msg_.c_str(),(*tempUnLongInt_));
    else if ((tempUnLongInt_ == NULL) && (tempUnShortInt_ != NULL))
      rInfo("%s%d",gmsh_info_msg_.c_str(),(*tempUnShortInt_));
  };
  string gmsh_info_msg_;
  unsigned int* tempUnLongInt_;
  unsigned short int* tempUnShortInt_;
};

struct gmsh_nothing_a
{
  gmsh_nothing_a()
  {}
  void operator()(char const* first, char const* last) 
    const
  {
  };
};

template<typename type>
struct gmsh_compare_a
{
  gmsh_compare_a(type* a, type* b)
  : a_(a), b_(b) {}
  bool operator () () const
  {
    if (*a_ == *b_)
    {
      //rDebug("Return TRUE! a = %d  b = %d", *a_, *b_);      
      return true;
    }
    else
    {
      //rDebug("Return FALSE! a = %d  b = %d", *a_, *b_);      
      return false;
    }
  };
  type* a_;
  type* b_;
};

template<typename type>
struct gmsh_array_access_a
{
  gmsh_array_access_a(type* a, type* b)
  : a_(a), b_(b) {}
  void operator()(char const* first, char const* last) const
  {
    *a_ = GMSH_ELEM_N_NODES[*b_];
  };
  type* a_;
  type* b_;
};


struct gmsh_show_elem_result_a
{
  gmsh_show_elem_result_a(gmsh* gmshSelf)
  : gmshSelf_(gmshSelf) {}
  void operator()(char const* first, char const* last) 
    const
  {
    gmshSelf_->gmshShowElemResult();
  };
  gmsh* gmshSelf_;
};

#endif // GMSHSEMANTICACTION_HH_
