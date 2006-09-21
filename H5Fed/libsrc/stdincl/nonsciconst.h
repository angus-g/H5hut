// rights    - copyright by benedikt oswald, 2002-2006, all rights reserved
// project   - phidias3d
// file name - nonsciconst.h
// file type - c++ include file
// author    - benedikt oswald
// modified  - 2004 may 21, creation, benedikt oswald
// modified  - 2004 jun 11, added font definitions, benedikt oswald
// modified  - 2006 jun 26, transferred to project phidias3d, benedikt oswald
// modified  - 2006 jun 28, correction of RIGHTANGLE_DEG from int to double, 
//             patrick leidenberger
// modified  - 2006 jul 11, add vtk cell type constants, 
//             patrick leidenberger
// objective - define technical, non-scientific constants, e.g. plot paper 
//             dimensions etc.


/* include standard libraries */
#include <cmath>
#include <complex>
#include <string>
#include <vector>


#ifndef _NONSCICONST_
#define _NONSCICONST_

using namespace std;

namespace nonsciconst
{

const double	A4WIDTH								=	210.0;
const double	A4HEIGHT							=	297.0;

const double	A5WIDHT								=	105.0;
const double	A5HEIGHT							=	148.5;

const unsigned int		FRAMED						=	1;
const unsigned int		NOTFRAMED					=	0;
const unsigned int		BELOW 						=	1;
const unsigned int		ABOVE						=	0;
const unsigned int		ATLEFT    					=	1;
const unsigned int		ATRIGHT   					=	0;
const unsigned int		FILLED    					=	1;
const unsigned int		NOTFILLED 					=	0;
const unsigned int		OUTERFACES 					=	1;
const unsigned int		INNERFACES 					=	0;

const double    		RIGHTANGLE_DEG				=	90.0;

const unsigned int		DRAW_GRID_NODES_FLAG		=	1;
const unsigned int		DRAW_GRID_LINES_FLAG		=	1;

const double 			MAXHUE	  					=	0.7;
const double			MAXSAT						=	1.0;
const double			MAXBRG						=	1.0;

const unsigned int		GRAY_COLORSPACE				=	0;
const unsigned int		RGB_COLORSPACE				=	2;
const unsigned int		HSB_COLORSPACE				=	4;
const unsigned int		CMYK_COLORSPACE				=	8;

const double			HSB_H_BLUE					=	0.7;
const double			HSB_H_RED					=	1.0;
const double 			HSB_H_BLACK					=	0.1;

const unsigned int		LEGEND_HORIZONTAL			=	1;
const unsigned int		LEGEND_VERTICAL				=	0;

const unsigned int		NUMCHARPLTFLNM				=	256;
const unsigned int		NUMCHARSTRINGPLOTPROD		=	256;
const unsigned int		DSPLYSTRLE					=	512;
const unsigned int		MAXFILENAMSTRLEN			=	512;
const unsigned int		DSPSTRLEN					=	256;

const double			MINIMUM_DB_VAL				=	-60.0;

const string			HELVETICA_OBLIQUE_FNT("Helvetica-Oblique");
const string			HELVETICA_FNT("Helvetica");
const string			TIMES_FNT("Times-Roman");
const string			ARIAL_FNT("Arial");
const string			SYMBOL_FNT("Symbol");

const double 			INCHINPT					=	72.0;         /* define an inch in points, a typographic unit */
const double			INCHINMM					=	25.4;         /* define an inch in millimeter */

const string			ELECTRIC_FIELD_UNIT_STRING("[V/m]");
const string			ELECTRIC_FIELD_UNIT_STRING_DB("[norm. dB]");

const double			TEXT_DOWN_DIR_DEG			=	270.0;
const double			TEXT_RIGHT_DIR_DEG			=	0.0;

const unsigned int		COLRSPCDIM					=	3;

const double PSGRAF3_SYMBOL_RADIUS_PCOORD=0.3;

const int PSGRAF3_SYMBOL_CIRCLE						=	0;
const int PSGRAF3_SYMBOL_SQUARE						=	1;
const int PSGRAF3_SYMBOL_RHOMBUS					=	2;
const int PSGRAF3_SYMBOL_TRIANGLE_TIPUP				=	3;
const int PSGRAF3_SYMBOL_TRIANGLE_TIPDOWN			=	4;

const int OKCODE									=	0;
const int ERRORCODE									=	1;
const int ERROR_SINGULAR_MATRIX						=	2;
const int ERROR_PSGRAF3_NOT_AVAILABLE				=	3;
const int ERROR_NNZ_IS_ZERO							=	100;


const string	VTK_LFF_EXTENSION(".vtk");
const string	HDF5_EXTENSION(".h5");

const string	TAB_STRING("\t");
const string	SINGLE_SPACE(" ");


const int DEFAULT_FLOAT_PRECISION=6;

// Define constants for vtk cell types.
const unsigned int VTK_VERTEX               = 1;
const unsigned int VTK_POLY_VERTEX          = 2;
const unsigned int VTK_LINE                 = 3;
const unsigned int VTK_POLY_LINE            = 4;
const unsigned int VTK_TRIANGLE             = 5;
const unsigned int VTK_TRIANGEL_STRIP       = 6;
const unsigned int VTK_POLYGON              = 7;
const unsigned int VTK_PIXEL                = 8;
const unsigned int VTK_QUAD                 = 9;
const unsigned int VTK_TETRA                = 10;
const unsigned int VTK_VOXEL                = 11;
const unsigned int VTK_HEXAHEDRON           = 12;
const unsigned int VTK_WEDGE                = 13;
const unsigned int VTK_PYRAMID              = 14;
const unsigned int VTK_QUADRATIC_EDGE       = 21;
const unsigned int VTK_QUADRATIC_TRIANGLE   = 22;
const unsigned int VTK_QUADRATIC_QUAD       = 23;
const unsigned int VTK_QUADRATIC_TETRA      = 24;
const unsigned int VTK_QUATRATIC_HEXAHEDRON = 25;

}

#endif

