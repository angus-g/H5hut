/*
  Header file for declaring the H5Fed application programming
  interface (API) in the C language.
  
  Copyright 2006-2007
 	Paul Scherrer Institut, Villigen, Switzerland;
 	Benedikt Oswald;
 	Achim Gsell
 	All rights reserved.
 
  Authors
 	Achim Gsell
  
  Warning
	This code is under development.
 
 */

#ifndef __H5FED_MAP_H
#define __H5FED_MAP_H

h5_id_t
H5FedMapTet2GlobalID (
	h5_file * const f,
	h5_id_t * const global_vids
	);

#endif

