// rights - 2002-2005, benedikt oswald, 
// project - aq
// file name - cube0.geo
// file type - GMSH script file
// objective - model cube fundamental cube
// modified - 2005 jan 25, benedikt oswald, creation
// modified - 2005 jan 25, benedikt oswald, added background hexahedron, subsurface hexahedron
// modified - 2005 feb 01, benedikt oswald, adapted to the simple.geo problem
// modified - 2005 mar 08, benedikt oswald, adapted to the cube in cube geometry
// modified - 2005 mar 22, benedikt oswald, adapted to model fundamental cube
// inheritance - 
// feature - models fundamental cube, used for benchmark calculations of a Hertzian dipole
// feature - radiating into free space; the model uses standard physical domain tags;


// 1 modeling constants

lcl1=0.50;             // characteristic length in fractions of lambda@1 GHz for air domain
lcl2=0.50;             // characteristic length in fractions of lambda@1 GHz for air domain

lambda=0.30;           // wavelength of electromagntic wave at a frequency of 1 GHz

lx=2.0;                // x dim of computational domain
ly=2.0;                // y dim of computational domain
lz=2.0;                // z dim of computational domain

cx=0.5*lx;             // calculate center of computational domain
cy=0.5*ly;             // calculate center of computational domain
cz=0.5*lz;             // calculate center of computational domain


// 1.2 Define physical material domains

vacuum=609;                   // physical entity vacuum
air=709;                      // physical entity air
subsurface=809;               // physical entity subsurface
pml=1301;                     // physical entity pml material
icelens=4001;                 // physical entity icelens
vacuum_background_bnd=12701;  // boundary to background, i.e. vacuum
interelement_bnd=12702;       // interior boundary, separating different material domains



// 2 Define cubes
// 2.1.1 Define points of cube

p0=newp; Point(p0) = {0.0, 0.0, 0.0, lambda * lcl1};
p1=newp; Point(p1) = { lx, 0.0, 0.0, lambda * lcl1};
p2=newp; Point(p2) = { lx,  ly, 0.0, lambda * lcl1};
p3=newp; Point(p3) = {0.0,  ly, 0.0, lambda * lcl1};
p4=newp; Point(p4) = {0.0, 0.0,  lz, lambda * lcl1};
p5=newp; Point(p5) = { lx, 0.0,  lz, lambda * lcl1};
p6=newp; Point(p6) = { lx,  ly,  lz, lambda * lcl1};
p7=newp; Point(p7) = {0.0,  ly,  lz, lambda * lcl1};

// 2.1.3 Define lines of cube

// lower xy plane
li0=newl; Line(li0) = {p0,p1};
li1=newl; Line(li1) = {p1,p2};
li2=newl; Line(li2) = {p2,p3};
li3=newl; Line(li3) = {p3,p0};

// upper xy plane
li4=newl; Line(li4) = {p4,p5};
li5=newl; Line(li5) = {p5,p6};
li6=newl; Line(li6) = {p6,p7};
li7=newl; Line(li7) = {p7,p4};

// vertical lines
li8=newl; Line(li8) = {p0,p4};
li9=newl; Line(li9) = {p1,p5};
li10=newl; Line(li10) = {p2,p6};
li11=newl; Line(li11) = {p3,p7};


// 2.1.5 Define line 6 line loops for the 6 faces of the cube

lloop0= newreg; Line Loop(lloop0) = { li0, li1, li2, li3};
lloop1= newreg; Line Loop(lloop1) = { li4, li5, li6, li7};

lloop2= newreg; Line Loop(lloop2) = { li0, li9, -li4, -li8 };
lloop3= newreg; Line Loop(lloop3) = { li1, li10, -li5, -li9};

lloop4= newreg; Line Loop(lloop4) = { li2, li11, -li6, -li10};
lloop5= newreg; Line Loop(lloop5) = { li3, li8,  -li7, -li11};


// 2.1.7 define plane surfaces for the cube

s0=news; Plane Surface(s0) = {lloop0};
s1=news; Plane Surface(s1) = {lloop1};
s2=news; Plane Surface(s2) = {lloop2};
s3=news; Plane Surface(s3) = {lloop3};
s4=news; Plane Surface(s4) = {lloop4};
s5=news; Plane Surface(s5) = {lloop5};


// 2.1.9 Define surface loops required for volume definition of the cube

sloop0=newreg; Surface Loop(sloop0) ={s0,s1,s2,s3,s4,s5};

// 2.1.10 Define volume of the cube

v0=newv; Volume(v0) = {sloop0};

// 3.0 define physical entitites

Physical Point(vacuum_background_bnd) = {p0:p7};
Physical Line(vacuum_background_bnd) = {li0:li11};
Physical Surface(vacuum_background_bnd) = {s0:s5};
Physical Volume(air) = {v0};


