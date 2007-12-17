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


#ifndef H5FED_HH
#define H5FED_HH

namespace H5Fed
{
	const int ERRORCODE		=	-1;
	const int OKCODE			=	0;


	class H5Fed
	{
	public:
		
		/** \brief infrastructure routines */
		H5Fed();													/** \brief Constructor without argument */
		H5Fed(std::string filename);			/** \brief Constructor without argument */
		~H5Fed();													/** \brief Class destructor */
		
		int filename(std::string filename);		/** \brief Set name of H5Fed file to be accessed */
		std::string filename();								/** \brief retrieve name of H5Fed file to be accessed */
		
		/** \brief book keeping */
		
		
	protected:
		
		
		
		
		
		
	private:
		/** book keeping */
		std::string filename_;
		
		
		
		
		
		
	};



















}

#endif /** H5FED_HH */