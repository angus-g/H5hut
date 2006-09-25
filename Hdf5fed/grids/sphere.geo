// rights - 2002-2005, benedikt oswald, 
// project - aq
// file name - sphere.geo
// file type - GMSH script file
// objective - model cube in a cube, 2 spherical objects mesh to investigate sub lambda resolution capability 
// modified - 2005 jan 25, benedikt oswald, creation
// modified - 2005 jan 25, benedikt oswald, added background hexahedron, subsurface hexahedron
// modified - 2005 feb 01, benedikt oswald, adapted to the simple.geo problem
// modified - 2005 mar 08, benedikt oswald, adapted to the cube in cube geometry
// modified - 2005 mar 09, benedikt oswald, adapted to cube in cube and spherical particle configuration
// modified - 2005 mar 11, benedikt oswald, adapted to investigation of sub-lambda resolution capability
// modified - 2005 mar 22, benedikt oswald, adapted to be based on reference background medium subLambda310.geo
// modified - 2005 mar 23, benedikt oswald, adapted to model a microresonator geometry
// modified - 2005 mar 29, benedikt oswald, adapted to model a high quality sphere for a Hertzian dipole benchmark
// modified - 2005
// inheritance - 
// feature - models a microresonator geometry; a dielectric sphere within an air sphere; the dielectric
// feature - sphere will be excited by a Hertzian dipole in the vicinity of the sphere;
// feature - 


// 1 modeling constants

lcair=1.20;             // characteristic length in fractions of lambda@1 GHz for air domain
lcsubsurface=1.20;      // characteristic length in fractions of lambda@1 GHz for subsurface domain
lcil1=0.80;              // characteristic length in fractions of lambda@1 GHz for icelens region
lcil2=0.40;              // characteristic length in fractions of lambda@1 GHz for icelens 2 region
lambda=3.0e-1;          // wavelength of electromagntic wave at a frequency of 1 GHz

lx=3.0;                 // x dim of computational domain
ly=3.0;                 // y dim of computational domain
lz=2.0;                 // z dim of computational domain

cx=0.5*lx;              // calculate center of computational domain
cy=0.5*ly;              // calculate center of computational domain
cz=0.5*lz;              // calculate center of computational domain

dxl=0.3;                // thickness of PML region
dxh=0.3;                // thickness of PML region

dyl=0.3;                // thickness of PML region
dyh=0.3;                // thickness of PML region

dzl=0.30;               // thickness of PML region
dzh=0.60;               // thickness of PML region

xminic=dxl;             // x min of inner cube
xmaxic=lx-dxh;          // x max of inner cube
yminic=dyl;             // y min of inner cube
ymaxic=ly-dyh;          // y max of inner cube
zminic=dzl;             // z min of inner cube
zmaxic=lz-dzh;          // z max of inner cube

dsx=0.10;               // x distance separating the two spherical scatterers
dsy=0.00;               // y distance separating the two spherical scatterers
dsz=0.00;               // z distance separating the two spherical scatterers

r1=1.0;                    // radius of air sphere
ilcx=0.0;                     // x center of air sphere
ilcy=0.0;                     // y center of 
ilcz=0.0;                     // z center of 

r2=2.0e-6;                    // radius of dielectric sphere
il2cx=0.0;                    // x center of dielectric sphere
il2cy=0.0;                    // y center of dielectric sphere
il2cz=0.0;                    // z center of dielectric sphere


// 1.2 Define physical material domains

vacuum=609;                   // physical entity vacuum
air=709;                      // physical entity air
subsurface=809;               // physical entity subsurface
pml=1301;                     // physical entity pml material
icelens1=4001;                // physical entity icelens
icelens2=4003;                // physical entity icelens 2
dielsphere=4005;              // physical entity dielectric sphere
vacuum_background_bnd=12701;  // boundary to background, i.e. vacuum
air_soil_bnd=12702;           // interior boundary, separating air and subsurface material
soil_icelens1_bnd=12703;      // interior boundary, separating background subsurface from icelens particle
soil_icelens2_bnd=12705;      // interior boundary, separating background subsurface from icelens 2 particle
dielectric_sphere_bnd=12707;  // interior boundary, separting air background from dielectric sphere


// 2.2.1 Define air sphere 1

ilp1 = newp; Point(ilp1) = {ilcx,    ilcy,    ilcz,    lcil1 * lambda} ;
ilp2 = newp; Point(ilp2) = {ilcx+r1, ilcy,    ilcz,    lcil1 * lambda} ;
ilp3 = newp; Point(ilp3) = {ilcx,    ilcy+r1, ilcz,    lcil1 * lambda} ;
ilp4 = newp; Point(ilp4) = {ilcx,    ilcy,    ilcz+r1, lcil1 * lambda} ;
ilp5 = newp; Point(ilp5) = {ilcx-r1, ilcy,    ilcz,    lcil1 * lambda} ;
ilp6 = newp; Point(ilp6) = {ilcx,    ilcy-r1, ilcz,    lcil1 * lambda} ;
ilp7 = newp; Point(ilp7) = {ilcx,    ilcy,    ilcz-r1, lcil1 * lambda} ;

ilc1 = newreg; Circle(ilc1) = {ilp2,ilp1,ilp7};
ilc2 = newreg; Circle(ilc2) = {ilp7,ilp1,ilp5};
ilc3 = newreg; Circle(ilc3) = {ilp5,ilp1,ilp4};
ilc4 = newreg; Circle(ilc4) = {ilp4,ilp1,ilp2};
ilc5 = newreg; Circle(ilc5) = {ilp2,ilp1,ilp3};
ilc6 = newreg; Circle(ilc6) = {ilp3,ilp1,ilp5};
ilc7 = newreg; Circle(ilc7) = {ilp5,ilp1,ilp6};
ilc8 = newreg; Circle(ilc8) = {ilp6,ilp1,ilp2};
ilc9 = newreg; Circle(ilc9) = {ilp7,ilp1,ilp3};
ilc10 = newreg; Circle(ilc10) = {ilp3,ilp1,ilp4};
ilc11 = newreg; Circle(ilc11) = {ilp4,ilp1,ilp6};
ilc12 = newreg; Circle(ilc12) = {ilp6,ilp1,ilp7};

// We need non-plane surfaces to define the spherical icelens:
// here we use ruled surfaces, which can have 3 or 4
// sides:

ill1 = newreg; Line Loop(ill1) = {ilc5,ilc10,ilc4};   ilruls1=newreg; Ruled Surface(ilruls1) = {ill1};
ill2 = newreg; Line Loop(ill2) = {ilc9,-ilc5,ilc1};   ilruls2=newreg; Ruled Surface(ilruls2) = {ill2};
ill3 = newreg; Line Loop(ill3) = {ilc12,-ilc8,-ilc1}; ilruls3=newreg; Ruled Surface(ilruls3) = {ill3};
ill4 = newreg; Line Loop(ill4) = {ilc8,-ilc4,ilc11};  ilruls4=newreg; Ruled Surface(ilruls4) = {ill4};
ill5 = newreg; Line Loop(ill5) = {-ilc10,ilc6,ilc3};  ilruls5=newreg; Ruled Surface(ilruls5) = {ill5};
ill6 = newreg; Line Loop(ill6) = {-ilc11,-ilc3,ilc7}; ilruls6=newreg; Ruled Surface(ilruls6) = {ill6};
ill7 = newreg; Line Loop(ill7) = {-ilc2,-ilc7,-ilc12};ilruls7=newreg; Ruled Surface(ilruls7) = {ill7};
ill8 = newreg; Line Loop(ill8) = {-ilc6,-ilc9,ilc2};  ilruls8=newreg; Ruled Surface(ilruls8) = {ill8};

ilsloop0 = newreg; 
Surface Loop(ilsloop0) = {ill8+1,ill5+1,ill1+1,ill2+1,ill3+1,ill7+1,ill6+1,ill4+1};


// 2.3.3 Define volume of air sphere

vil1=newv; Volume(vil1) = {ilsloop0};


// 3.0 define physical entitites

Physical Point(vacuum_background_bnd) = {ilp1,ilp2,ilp3,ilp4,ilp5,ilp6,ilp7};
Physical Surface(vacuum_background_bnd) = {ilruls1,ilruls2,ilruls3,ilruls4,ilruls5,ilruls6,ilruls7,ilruls8};
Physical Volume(air) = {vil1};

