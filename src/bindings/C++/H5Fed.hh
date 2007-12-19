/*
  Header file for declaring the H5Fed application programming
  interface (API) in the C++ language. The header files follows
  the declaration of the C application programming interface.
  
  Copyright 2006-2007
 	Paul Scherrer Institut, Villigen, Switzerland;
 	Benedikt Oswald;
 	Achim Gsell
 	All rights reserved.
 
  Authors
 	Benedikt Oswald, Achim Gsell
  
  Warning
	This code is under development.
 
 */

/*!
  Some conventions:
	Functions:
		Name:
			ThisIsAFunction()
		Return values:
			-1 or NULL signals an error

  \note
  In function names we use the words \b get and \b store insteed of
  \b read and \b write, because no I/O is actually done in these
  functions.
*/


/** include standard header files */
#include <iostream>
#include <ostream>
#include <iterator>
#include <fstream>
#include <vector>   
#include <iomanip>
#include <string>
#include <complex>

/** include fundamental HDF5 header files */
#include <hdf5.h>


#ifndef H5FED_HH
#define H5FED_HH

namespace H5Fed
{
	/** \brief Define return codes */
	enum H5FED_RETURN_CODE{OKCODE=0,ERROR=-1};



	class H5Fed
	{
	public:
		
		/** \brief infrastructure routines */
		H5Fed();																						/** \brief Constructor without argument */
		H5Fed(std::string filename);												/** \brief Constructor without argument */
		~H5Fed();																						/** \brief Class destructor */
		
		H5FED_RETURN_CODE filename(std::string filename);		/** \brief Set name of H5Fed file to be accessed */
		std::string filename();															/** \brief retrieve name of H5Fed file to be accessed */
		
		/******	General routines *****************************************************/
		H5FED_RETURN_CODE  open_file();											/** \brief open the file */
		H5FED_RETURN_CODE  closeFile();											/** \brief close the file */
		
		
		/******	INQUIRY routines *****************************************************/	
		
		
		
		
		
		
		/******	STORE routines*****************************************************/
		
		/*!
		  \ingroup h5fed_c_api

		  Stores the the coordinates of a specific vertex at level \c level
		  with id \c vertex_id of the tetrahedral mesh.

		  \return value \c >=0 on success
		  \return \c -1 on error
		*/		
		H5FED_RETURN_CODE storeVertexCoordinate(
				unsigned int level,				/*!< mesh level			*/
				unsigned int vertex_id,		/*!< global vertex id		*/
				std::vector<double>	/*!< 3-tuple of coordinates	*/
		);

		
		
		
		/*!
		  \ingroup h5fed_c_api

		  Stores the 4-tuple, that contains the specific indices describing
		  a tetrahedron with id \c tet_id at level \c level of the tetrahedral
		  mesh.

		  \return value \c >=0 on success
		  \return \c -1 on error
		*/
		H5FED_RETURN_CODE H5FedStoreTetrahedron (
			const unsigned int level,							/*!< mesh level			*/
			const unsigned int tet_id,						/*!< global tetrahedron id	*/
			const unsigned int parent_id,					/*!< parent id if level \c >0 else \x -1 */
			const std::vector<unsigned int> tet		/*!< 4-tuple with vertex id's	*/
			);
		
		
		
		
		
		
		
		
		/******	UPWARD ADJACENCY routines *********************************************/
		

		
		
		/******	DOWNWARD ADJACENCY routines *********************************************/
		

		
		
		
		
		/******	routines for accessing degrees of freedom DoF *************************/
		
		
		
		
		
		
		
		
		
		
	protected:
		
		
	private:
		/** book keeping */
		std::string filename_;														/** \brief Name of H5Fed file to be accessed */
		
		
		
		
		
		
	};
}

#endif /** H5FED_HH */
