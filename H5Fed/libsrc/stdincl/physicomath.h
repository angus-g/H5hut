// rights - copyright by benedikt oswald, 2002-2006, all rights reserved
// project - phidias3d
// file name - physicomath.h
// file type - c++ include file
// author - benedikt oswald
// modified - 2003 jan 30, creation - 
// modified - 2006 jun 26, transferred to project phidias3d, benedikt oswald
// objective - physical and mathematical quantities, constansts etc.

#include <cmath>
#include <complex>

#ifndef _PHYSICO_MATH_
#define _PHYSICO_MATH_

using namespace std;

namespace physicomath
{
	const double			ZERO				=	0;
	const int				INT_ZERO			=	0;
	const unsigned int		UNSIGNED_INT_ZERO	=	0;
	const double			DOUBLE_ZERO			=	0.0;
	const int				INT_ONE				=	1;
	const unsigned int		UNSIGNED_INT_ONE	=	1;
	const int				INT_TWO				=	2;
	const double			DOUBLE_ONE			=	1.0;
	const double			DOUBLE_TWO			=	2.0;

#ifndef IMAGINARY_UNIT_
#define IMAGINARY_UNIT_
	const complex<double> IU(0.0,1.0);
#endif

	const double			PI					=	3.1415926535;
	const double			EPSILON_ZERO		=	8.85418782e-12;
	
	const double			MU_ZERO				=	4.0 * PI * 1.0e-7;
	
	const double			SPEED_OF_LIGHT		=	(1.0/(sqrt(EPSILON_ZERO*MU_ZERO)));
	
	const double			Z0					=	sqrt(MU_ZERO/EPSILON_ZERO);

	const double			TERAHERTZ			=	1.0e12;
	const double			GIGAHERTZ			=	1.0e9;
	const double			MEGAHERTZ			=	1.0e6;
	const double			KILOHERTZ			=	1.0e3;

	const double			SECOND 				=	1.0e0;
	const double			MILLISECOND			=	1.0e-3;
	const double			MICROSECOND			=	1.0e-6;
	const double			NANOSECOND			=	1.0e-9;
	const double			PICOSECOND			=	1.0e-12;
	const double			FEMTOSECOND			=	1.0e-15;


	const double L_BAND_CENTER_FREQUENCY=1.414e9;

	const double DOUBLE_REL_ZERO_LIMIT=1.0e-100;


	const double METER							=	1.0;			/* define a meter, the standard */
	const double DECIMETER						=	0.1;			/* define a tenth of a meter */
	const double CENTIMETER						=	1.0e-2;			/* define a centimeter */
	const double MICROMETER						=	1.0e-6;			/* define a micrometer */
	const double NANOMETER						=	1.0e-9;			/* define a nanomenter */
	const double ANGSTROM						=	1.0e-10;		/* define an angstrom */
}

#endif


