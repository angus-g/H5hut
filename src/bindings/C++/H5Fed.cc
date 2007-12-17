/*
  Implementation file for implementing the H5Fed application programming
  interface (API) in the C++ language.
  
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


/** include proprietary header files */
#include <H5Fed.hh>


/** activate namespaces */
using namespace H5Fed;
using namespace std;


/** \brief implement constructor without arguments */
H5Fed::H5Fed()
{
	/** initialize internal variables */
	filename_.erase();
}


/** \brief implement constructor without arguments */
H5Fed::H5Fed(std::string filename)
{
	/** initialize internal variables */
	filename_.erase();
	filename_.append(filename);
}


/** \brief Set name of H5Fed file to be accessed */
int H5Fed::filename(std::string filename)
{
	/** initialize internal variables */
	filename_.erase();
	filename_.append(filename);
	
	return(OKCODE);
}


/** \brief retrieve name of H5Fed file to be accessed */
std::string H5Fed::filename()
{
	return(filename_);
}















