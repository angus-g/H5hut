/** \brief Definition of gmsh file format v2.0 parser data types and variables
 * 
 * rights    - patrick leidenberger and benedikt oswald
 * file name - gmshgrammar.hh
 * file type - include file
 * objective - define grammar of gmsh file format v2.0
 * author    - patrick leidenberger and benedikt oswald
 * modified  - 2006 jun 29, pl, creation
 * features  - defines grammar of gmsh files v2.0
 */

/* include standard header files */
#include <boost/spirit/core.hpp>
#include <boost/spirit/iterator/file_iterator.hpp>
#include <boost/spirit/actor/assign_actor.hpp>
#include <boost/spirit/utility/functor_parser.hpp>
#include <boost/spirit/utility/escape_char.hpp>
#include <boost/spirit/utility/loops.hpp>
#include <boost/spirit/dynamic/if.hpp>
#include <iostream>
#include <functional>
#include <map>
#include <stack>
#include <string>
#include <boost/bind.hpp>
#include <boost/ref.hpp>

/* include proprietary header files */
#include "gmshsemanticaction.hh"
#include "gmsh.hh"
// Include gmsh specific constants.
#include "gmshconst.hh"

#ifndef GMSHGRAMMAR_HH_
#define GMSHGRAMMAR_HH_

/* activate namespaces */
using namespace std;
using namespace boost::spirit;
using namespace gmshtohdf5fed;

/* type definitions */
//typedef char                    char_t;
//typedef file_iterator<char_t>   iterator_t;
//typedef scanner<iterator_t>     scanner_t;
//typedef rule<scanner_t>         rule_t;


////////////////////////////////////////////////////////////////////////////
//
//  Our calculator grammar
//
////////////////////////////////////////////////////////////////////////////
struct gmsh_g : public grammar<gmsh_g>
{
//  vtk_g(/*stack<long>& eval_*/)
//    /*: eval(eval_)*/ {}
  gmsh_g(gmsh* gmshSelf)
    : gmshSelf_(gmshSelf) 
    {
    }

  template <typename ScannerT>
  struct definition
  {
    definition(gmsh_g const& self)
    {
      gmsh_import_r = gmsh_mesh_format_r
                      >> gmsh_nodes_r
                      >> gmsh_elements_r;

     //////////////////////////////////////////////////////////////////////////
     // GENERAL NOTE: All tranport of numbers to gmshsemanticaction.hh must //
     //               be done by reference, not by value. Transport by      //
     //               value fails! You have to debug long for it!           //
     /////////////////////////////////////////////////////////////////////////

     // Rules for the MeshFormat section.
      gmsh_mesh_format_r = str_p("$MeshFormat")
                           >> int_p[assign_a(tempInt1_)]
                                [gmsh_version_a(self.gmshSelf_, &tempInt1_)]
                           >> int_p[assign_a(tempInt1_)]
                                [gmsh_file_type_a(self.gmshSelf_, &tempInt1_)]
                           >> int_p[assign_a(tempInt1_)]
                           >> str_p("$EndMeshFormat");

      // Rules for the Nodes section.      
      gmsh_nodes_r = str_p("$Nodes") 
                     >> eps_p[gmsh_clear_map_a<unsigned int, 
                                vector<double> >(&tempCoordMap_)]
                     >> int_p[assign_a(tempUnInt1_)]
                     >> eps_p[gmsh_info_msg_a("Nodes in gmsh file: ",
                                &tempUnInt1_)]
                     >> repeat_p(boost::ref(tempUnInt1_))
                        [
                          eps_p[gmsh_clear_vec_a<double>(&tempVecDouble1_)]
                          >> int_p[assign_a(tempUnInt2_)]
                          >> repeat_p(3) 
                             [real_p[assign_a(tempDouble1_)]
                                [gmsh_push_vec_a<double>(&tempVecDouble1_, 
                                   &tempDouble1_)]
                             ]
                             [gmsh_insert_map_a<unsigned int, 
                                                vector<double> >
                                (&tempCoordMap_, &tempUnInt2_,
                                 &tempVecDouble1_)
                             ]
                        ]
                     >> str_p("$EndNodes")
                     >> eps_p[gmsh_transport_nodes_a(self.gmshSelf_,
                                                     &tempCoordMap_)];

      // Rules for the Elements section.      
      gmsh_elements_r = str_p("$Elements")
                        >> int_p[assign_a(tempUnInt1_)]
                        >> eps_p[gmsh_info_msg_a("Elements in gmsh file: ",
                                   &tempUnInt1_)]
                        >> gmsh_element_r
                        >> str_p("$EndElements");

      gmsh_element_r = repeat_p(boost::ref(tempUnInt1_))
                       [ // Parse one element
                         // Clear the vectors for nodes and tags of an elem.
                         eps_p[gmsh_clear_vec_a<unsigned int>
                                 (&tempElemNode_)]
                         >> eps_p[gmsh_clear_vec_a<unsigned int>
                                   (&tempElemTag_)]
                         // Parse the element number. We don't need it.
                         >> int_p[assign_a(tempUnInt2_)]
                         // Parse the element type.
                         >> int_p[assign_a(tempUnShortInt1_)]
                         // Parse the number of tags.
                         >> int_p[assign_a(tempUnInt3_)]
                         // Parse the tags.
                         >> repeat_p(boost::ref(tempUnInt3_))
                            [
                              // Parse a single tag.
                              int_p[assign_a(tempUnInt4_)]
                              // Copy to vector.
                              >> eps_p[gmsh_push_vec_a<unsigned int>
                                        (&tempElemTag_, &tempUnInt4_)]
                            ][gmsh_nothing_a()]
                         // Copy the number of nodes of the actual element
                         // type (in tempUnShortInt1_) from the 
                         // GMSH_ELEM_N_NODES array in gmshconst.hh in 
                         // tempUnShortInt2_. 
                         // Do this, because I don't get the direct array 
                         // access not work.
                         >> eps_p[gmsh_array_access_a<unsigned short int>
                                    (&tempUnShortInt2_, &tempUnShortInt1_)]
                         // Parse the respective number of nodes.
                         >> repeat_p(boost::ref(tempUnShortInt2_))
                            [
                               // Parse a sigle node.
                               int_p[assign_a(tempUnInt4_)]
                              // Copy to vector.
                              >> eps_p[gmsh_push_vec_a<unsigned int>
                                        (&tempElemNode_, &tempUnInt4_)]
                              
                             ]
                            [
                              // Transport the parsed element nodes in the 
                              // respective gmsh.hh data structure.
                              gmsh_transport_elem_a(self.gmshSelf_, 
                                &tempUnShortInt1_, &tempElemNode_)
                            ]
                            [
                              // Transport the parsed element tags in the 
                              // respective gmsh.hh data structure.
                              gmsh_transport_tag_a(self.gmshSelf_, 
                                &tempUnShortInt1_, &tempElemTag_)
                            ]
                         // This short section demonstrates, like con-
                         // ditional parsing can be used instead:
                         //
                         //>> if_p(gmsh_compare_a<unsigned short int>
                         //            (&tempUnShortInt1_,&(elemType_[0])))
                         //     [
                         //       // Element type 1 == line.
                         //       repeat_p(2)
                         //       [
                         //         int_p[assign_a(tempUnInt4_)]
                         //       ][gmsh_nothing_a()]
                         //     ]
                         //
                       ]
                       [
                         // All elements are parsed.
                         // Show what we have paresd.
                         gmsh_show_elem_result_a(self.gmshSelf_)
                       ];

    }

    rule<ScannerT> expression, term, factor, integer;


    rule<ScannerT> vtk_import_r;

    rule<ScannerT> gmsh_import_r;
    rule<ScannerT> gmsh_mesh_format_r;
    rule<ScannerT> gmsh_nodes_r;
    rule<ScannerT> gmsh_elements_r;
    rule<ScannerT> gmsh_element_r;

    rule<ScannerT> const&
    start() const { return gmsh_import_r; }

  // These are temporal variables used during the parsing process.
  // These variables are used at different places for different things,
  // they are only valid in small ranges.
  int tempInt1_;
  unsigned int tempUnInt1_;
  unsigned int tempUnInt2_;
  unsigned int tempUnInt3_;
  unsigned int tempUnInt4_;
  unsigned short int tempUnShortInt1_;
  unsigned short int tempUnShortInt2_;

  double tempDouble1_;

  vector<int> tempIntVec1_;
  vector<double> tempDoubleVec1_;
  vector<vector<double> > tempDoubleVec2_;

  vector<double> tempVecDouble1_;

  // Data structure for node coordinates. 
  // The gmsh node coordinates are not stored and numbered consecutive. So we
  // have to sort and map them to a consecutive index set using this map.
  map<unsigned int, vector<double> > tempCoordMap_;
  // Data structure to store the nodes of an element.
  vector<unsigned int> tempElemNode_;
  // Data structure to store an elements tags.
  vector<unsigned int> tempElemTag_;
  
  };

  gmsh* gmshSelf_;
};

#endif // GMSHGRAMMAR_HH_
