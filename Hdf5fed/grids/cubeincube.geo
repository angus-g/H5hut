// rights - 2002-2005, benedikt oswald, 
// project - aq
// file name - cubeincube.geo
// file type - GMSH script file
// objective - model cube in a cube mesh, useful for PML booundary conditions
// modified - 2005 jan 25, benedikt oswald, creation
// modified - 2005 jan 25, benedikt oswald, added background hexahedron, subsurface hexahedron
// modified - 2005 feb 01, benedikt oswald, adapted to the simple.geo problem
// modified - 2005 mar 08, benedikt oswald, adapted to the cube in cube geometry
// inheritance - 
// feature - models a cube in a cube mesh, useful for PML booundary conditions;
// feature - the PML boundary model is based on a 3D region with special electromagnetic
// feature - material properties which guarantee, in theory, perfect absorption of incident
// feature - electromagnetic waves independent of the angle between the wave vector
// feature - and the boundary; in order to model the PML material domain a separate mesh
// feature - region is created so that there is a clean boundary between the PML and
// feature - and the air region.


// 1 modeling constants

lcair=0.90;            // characteristic length in fractions of lambda@1 GHz for air domain
lcpml=0.70;            // characteristic length in fractions of lambda@1 GHz for PML region
lambda=0.30;           // wavelength of electromagntic wave at a frequency of 1 GHz

lx=1.0;                // x dim of computational domain
ly=1.0;                // y dim of computational domain
lz=1.0;                // z dim of computational domain

cx=0.5*lx;             // calculate center of computational domain
cy=0.5*ly;             // calculate center of computational domain
cz=0.5*lz;             // calculate center of computational domain

lpmlxl=0.1;            // thickness of PML region
lpmlxh=0.1;            // thickness of PML region

lpmlyl=0.1;            // thickness of PML region
lpmlyh=0.1;            // thickness of PML region

lpmlzl=0.1;            // thickness of PML region
lpmlzh=0.1;            // thickness of PML region

xminic=lpmlxl;          // x min of inner cube
xmaxic=lx-lpmlxh;       // x max of inner cube
yminic=lpmlyl;          // y min of inner cube
ymaxic=ly-lpmlyh;       // y max of inner cube
zminic=lpmlzl;          // z min of inner cube
zmaxic=lz-lpmlzh;       // z max of inner cube

// 1.2 Define physical material domains

vacuum=609;                   // physical entity vacuum
air=709;                      // physical entity air
pml=1301;                     // physical entity pml material
icelens=4001;                 // physical entity icelens
vacuum_background_bnd=12701;  // boundary to background, i.e. vacuum
interelement_bnd=12702;       // interior boundary, separating different material domains



// 2 Define cubes
// 2.1.1 Define points of outer cube

p0=newp; Point(p0) = {0.0, 0.0, 0.0, lambda * lcpml};
p1=newp; Point(p1) = { lx, 0.0, 0.0, lambda * lcpml};
p2=newp; Point(p2) = { lx,  ly, 0.0, lambda * lcpml};
p3=newp; Point(p3) = {0.0,  ly, 0.0, lambda * lcpml};
p4=newp; Point(p4) = {0.0, 0.0,  lz, lambda * lcpml};
p5=newp; Point(p5) = { lx, 0.0,  lz, lambda * lcpml};
p6=newp; Point(p6) = { lx,  ly,  lz, lambda * lcpml};
p7=newp; Point(p7) = {0.0,  ly,  lz, lambda * lcpml};

// 2.1.2 Define points of inner cube

p100=newp; Point(p100) = {xminic, yminic, zminic, lambda * lcair};
p101=newp; Point(p101) = {xmaxic, yminic, zminic, lambda * lcair};
p102=newp; Point(p102) = {xmaxic, ymaxic, zminic, lambda * lcair};
p103=newp; Point(p103) = {xminic, ymaxic, zminic, lambda * lcair};

p104=newp; Point(p104) = {xminic, yminic, zmaxic, lambda * lcair};
p105=newp; Point(p105) = {xmaxic, yminic, zmaxic, lambda * lcair};
p106=newp; Point(p106) = {xmaxic, ymaxic, zmaxic, lambda * lcair};
p107=newp; Point(p107) = {xminic, ymaxic, zmaxic, lambda * lcair};


// 2.1.3 Define lines of outer cube

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

// 2.1.4 Define lines of outer cube

// lower xy plane
li100=newl; Line(li100) = {p100,p101};
li101=newl; Line(li101) = {p101,p102};
li102=newl; Line(li102) = {p102,p103};
li103=newl; Line(li103) = {p103,p100};

// upper xy plane
li104=newl; Line(li104) = {p104,p105};
li105=newl; Line(li105) = {p105,p106};
li106=newl; Line(li106) = {p106,p107};
li107=newl; Line(li107) = {p107,p104};

// vertical lines
li108=newl; Line(li108) = {p100,p104};
li109=newl; Line(li109) = {p101,p105};
li110=newl; Line(li110) = {p102,p106};
li111=newl; Line(li111) = {p103,p107};


// 2.1.5 Define line 6 line loops for the 6 faces of outer cube

lloop0= newreg; Line Loop(lloop0) = { li0, li1, li2, li3};
lloop1= newreg; Line Loop(lloop1) = { li4, li5, li6, li7};

lloop2= newreg; Line Loop(lloop2) = { li0, li9, -li4, -li8 };
lloop3= newreg; Line Loop(lloop3) = { li1, li10, -li5, -li9};

lloop4= newreg; Line Loop(lloop4) = { li2, li11, -li6, -li10};
lloop5= newreg; Line Loop(lloop5) = { li3, li8,  -li7, -li11};


// 2.1.6 Define line 6 line loops for the 6 faces of inner cube

lloop100= newreg; Line Loop(lloop100) = { li100, li101, li102, li103};
lloop101= newreg; Line Loop(lloop101) = { li104, li105, li106, li107};

lloop102= newreg; Line Loop(lloop102) = { li100, li109, -li104, -li108 };
lloop103= newreg; Line Loop(lloop103) = { li101, li110, -li105, -li109};

lloop104= newreg; Line Loop(lloop104) = { li102, li111, -li106, -li110};
lloop105= newreg; Line Loop(lloop105) = { li103, li108, -li107, -li111};


// 2.1.7 define plane surfaces for outer cube

s0=news; Plane Surface(s0) = {lloop0};
s1=news; Plane Surface(s1) = {lloop1};
s2=news; Plane Surface(s2) = {lloop2};
s3=news; Plane Surface(s3) = {lloop3};
s4=news; Plane Surface(s4) = {lloop4};
s5=news; Plane Surface(s5) = {lloop5};

// 2.1.8 define plane surfaces for inner cube

s100=news; Plane Surface(s100) = {lloop100};
s101=news; Plane Surface(s101) = {lloop101};
s102=news; Plane Surface(s102) = {lloop102};
s103=news; Plane Surface(s103) = {lloop103};
s104=news; Plane Surface(s104) = {lloop104};
s105=news; Plane Surface(s105) = {lloop105};


// 2.1.9 Define surface loops required for volume definition of outer cube

sloop0=newreg; Surface Loop(sloop0) ={s0,s1,s2,s3,s4,s5};


// 2.1.9 Define surface loops required for volume definition of inner cube

sloop100=newreg; Surface Loop(sloop100) ={s100,s101,s102,s103,s104,s105};


// 2.1.10 Define volume of outer cube

v0=newv; Volume(v0) = {sloop0,sloop100};


// 2.1.11 Define volume of inner cube

v100=newv; Volume(v100) = {sloop100};


// 3.0 define physical entitites

Physical Point(vacuum_background_bnd) = {p0,p1,p2,p3,p4,p5,p6,p7};
Physical Line(vacuum_background_bnd) = {li0,li1,li2,li3,li4,li5,li6,li7,li8,li9,li10,li11};
//Physical Line Loop(vacuum_background_bnd) = {lloop0,lloop1,lloop2,lloop3,lloop4,lloop5};
Physical Surface(vacuum_background_bnd) = {s0,s1,s2,s3,s4,s5};
//Physical Surface Loop(vacuum_background_bnd) = {sloop0};
Physical Volume(pml) = {v0};

Physical Point(interelement_bnd) = {p100,p101,p102,p103,p104,p105,p106,p107};
Physical Line(interelement_bnd) = {li100,li101,li102,li103,li104,li105,li106,li107,li108,li109,li110,li111};
//Physical Line Loop(interelement_bnd) = {lloop100,lloop101,lloop102,lloop103,lloop104,lloop105};
Physical Surface(interelement_bnd) = {s100,s101,s102,s103,s104,s105};
//Physical Surface Loop(interelement_bnd) = {sloop100};
Physical Volume(air) = {v100};


