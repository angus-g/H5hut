#ifndef GMSHCONST_HH_
#define GMSHCONST_HH_

// Contants of gmsh element types.
// See Gmsh Reference Manual (14 May 2006) page 147-148.
const unsigned short int GMSH_LINE            = 1;
const unsigned short int GMSH_TRIANGLE        = 2;
const unsigned short int GMSH_QUADRANGLE      = 3;
const unsigned short int GMSH_TETRAHEDRON     = 4;
const unsigned short int GMSH_HEXAHEDRON      = 5;
const unsigned short int GMSH_PRISM           = 6;
const unsigned short int GMSH_PYRAMID         = 7;
const unsigned short int GMSH_2ND_LINE        = 8;
const unsigned short int GMSH_2ND_TRIANGLE    = 9;
const unsigned short int GMSH_2ND_QUADRANGLE  = 10;
const unsigned short int GMSH_2ND_TETRAHEDRON = 11;
const unsigned short int GMSH_2ND_HEXAHEDRON  = 12;
const unsigned short int GMSH_2ND_PRISM       = 13;
const unsigned short int GMSH_2ND_PYRAMID     = 14;
const unsigned short int GMSH_POINT           = 15;

// Every element has an spesific number of tags.
// This is the list of the number. There is a leading zero, because there is 
// no element with the index 0.
const unsigned short int GMSH_ELEM_N_NODES[] = { 0, 2, 3, 4, 4, 8, 6, 5, 3,
                                                 6, 9, 10, 27, 18, 14, 1 };

#endif //GMSHCONST_HH_
