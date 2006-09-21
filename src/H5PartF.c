#include "H5Part.h"
#include "Underscore.h"
#include <hdf5.h>
/*

********* Perhaps call this pfile **********

Determine underscores using perl and then send through one stage of preprocessing
to get the F77NAME fixed up.  Otherwise, must include correct flag each time.

Or just don't declare F77NAME in the header (only in source as symbols).


All pointers are cast to haddr_t (a unint64_t) within the C/C++ code
In fortran, these pointers (handles) are carried as INTEGER*8

*/

#ifdef F77_SINGLE_UNDERSCORE
#define F77NAME(a,b,c) a
#elif defined(F77_NO_UNDERSCORE)
#define F77NAME(a,b,c) b
#elif defined(F77_CRAY_UNDERSCORE)
#define F77NAME(a,b,c) c
#else
#error Error, no way to determine how to construct fortran bindings
#endif

/* open/close interface */
#define f_h5pt_openr F77NAME(h5pt_openr_,h5pt_openr,H5PT_OPENR) /* func returns INT8 */
#define f_h5pt_openw F77NAME(h5pt_openw_,h5pt_openw,H5PT_OPENW) /* func returns INT8 */

#define f_h5pt_openr_par F77NAME(h5pt_openr_par_,h5pt_openr_par,H5PT_OPENR_PAR) /* func returns INT8 */
#define f_h5pt_openw_par F77NAME(h5pt_openw_par_,h5pt_openw_par,H5PT_OPENW_PAR) /* func returns INT8 */
#define f_h5pt_close F77NAME(h5pt_close_,h5pt_close,H5PT_CLOSE) 

/* writing interface */
#define f_h5pt_setnpoints F77NAME(h5pt_setnpoints_,h5pt_setnpoints,H5PT_SETNPOINTS)
#define f_h5pt_setstep F77NAME(h5pt_setstep_,h5pt_setstep,H5PT_SETSTEP)
#define f_h5pt_writedata_r8 F77NAME(h5pt_writedata_r8_,h5pt_writedata_r8,H5PT_WRITEDATA_R8)
#define f_h5pt_writedata_i8 F77NAME(h5pt_writedata_i8_,h5pt_writedata_i8,H5PT_WRITEDATA_I8)

/* Reading interface  (define dataset, step, particles, attributes) */
#define f_h5pt_getnsteps F77NAME(h5pt_getnsteps_,h5pt_getnsteps,H5PT_GETNSTEPS)
#define f_h5pt_getndatasets F77NAME(h5pt_getndatasets_,h5pt_getndatasets,H5PT_GETNDATASETS)
#define f_h5pt_getnpoints F77NAME(h5pt_getnpoints_,h5pt_getnpoints,H5PT_GETNPOINTS)
#define f_h5pt_getdatasetname F77NAME(h5pt_getdatasetname_,h5pt_getdatasetname,H5PT_GETDATASETNAME)
#define f_h5pt_getnumpoints F77NAME(h5pt_getnumpoints_,h5pt_getnumpoints,H5PT_GETNUMPOINTS)

/* Views and parallelism */
#define f_h5pt_setview F77NAME(h5pt_setview_,h5pt_setview,H5PT_SETVIEW)
#define f_h5pt_resetview F77NAME(h5pt_resetview_,h5pt_resetview,H5PT_RESETVIEW)
#define f_h5pt_hasview F77NAME(h5pt_hasview_,h5pt_hasview,H5PT_HASVIEW)
#define f_h5pt_getview F77NAME(h5pt_getview_,h5pt_getview,H5PT_GETVIEW)

/* Reading data */
#define f_h5pt_readdata_r8 F77NAME(h5pt_readdata_r8_,h5pt_readdata_r8,H5PT_READDATA_R8)
#define f_h5pt_readdata_i8 F77NAME(h5pt_readdata_i8_,h5pt_readdata_i8,H5PT_READDATA_I8)
#define f_h5pt_readdata F77NAME(h5pt_readdata_,h5pt_readdata,H5PT_READDATA)

/* Attributes */
/* writing */
#define f_h5pt_writefileattrib_r8 F77NAME(h5pt_writefileattrib_r8_,h5pt_writefileattrib_r8,H5PT_WRITEFILEATTRIB_R8)
#define f_h5pt_writefileattrib_i8 F77NAME(h5pt_writefileattrib_i8_,h5pt_writefileattrib_i8,H5PT_WRITEFILEATTRIB_I8)
#define f_h5pt_writefileattrib_string F77NAME(h5pt_writefileattrib_string_,h5pt_writefileattrib_string,H5PT_writefileattrib_string)
#define f_h5pt_writestepattrib_r8 F77NAME(h5pt_writestepattrib_r8_,h5pt_writestepattrib_r8,H5PT_WRITESTEPATTRIB_R8)
#define f_h5pt_writestepattrib_i8 F77NAME(h5pt_writestepattrib_i8_,h5pt_writestepattrib_i8,H5PT_WRITESTEPATTRIB_I8)
#define f_h5pt_writestepattrib_string F77NAME(h5pt_writestepattrib_string_,h5pt_writestepattrib_string,H5PT_WRITESTEPATTRIB_STRING)
/* reading */
#define f_h5pt_getnstepattribs F77NAME(h5pt_getnstepattribs_,h5pt_getnstepattribs,H5PT_GETNSTEPATTRIBS)
#define f_h5pt_getnfileattribs F77NAME(h5pt_getnfileattribs_,h5pt_getnfileattribs,H5PT_GETNFILEATTRIBS)
#define f_h5pt_getstepattribinfo F77NAME(h5pt_getstepattribinfo_,h5pt_getstepattribinfo,H5PT_GETSTEPATTRIBINFO)
#define f_h5pt_getfileattribinfo F77NAME(h5pt_getfileattribinfo_,h5pt_getfileattribinfo,H5PT_GETFILEATTRIBINFO)
#define f_h5pt_readstepattrib F77NAME(h5pt_readstepattrib_,h5pt_readstepattrib,H5PT_READSTEPATTRIB)
#define f_h5pt_readfileattrib F77NAME(h5pt_readfileattrib_,h5pt_readfileattrib,H5PT_READFILEATTRIB)

/* error handling */
#define f_h5pt_set_verbosity_level F77NAME(h5pt_set_verbosity_level_,h5pt_set_verbosity_level,H5PT_SET_VERBOSITY_LEVEL)

/* open/close interface */
h5part_int64_t
f_h5pt_openr (
	char *file,
	int flen
	) { /* func returns INT8 */

	H5PartFile* f;
	char *newname = (char*)malloc(flen+1); /* be safe? */
	strncpy(newname,file,flen);
	newname[flen]='\0';
	f = H5PartOpenFile ( newname, H5PART_READ );

	return (h5part_int64_t)(size_t)f; 

}

haddr_t
f_h5pt_openw (
	char *file,
	int flen) { /* func returns INT8 */

	haddr_t fh;
	H5PartFile* f;
	char *newname = (char*)malloc(flen+1); /* be safe? */
	strncpy(newname,file,flen);
	newname[flen]='\0';
	f = (H5PartOpenFile(newname,H5PART_WRITE));
	/*  printf("openr file=[%s] flen=%u haddr=%u\n",file,flen,f); */
	fh = (haddr_t)f;
	/* printf("FileHandle=%llu\n",fh); */

	return fh;
}

#ifdef PARALLEL_IO
haddr_t
f_h5pt_openr_par (
	char *file,
	MPI_Comm *c,
	int flen ) { /* func returns INT8 */

	haddr_t fh;
	H5PartFile* f;
	char *newname = (char*)malloc(flen+1); /* be safe? */
	strncpy(newname,file,flen);
	newname[flen]='\0';
	f = H5PartOpenFileParallel(newname,H5PART_READ,*((MPI_Comm*)c));
	/*  printf("openr file=[%s] flen=%u haddr=%u\n",file,flen,f); */
	fh = (haddr_t)f;
	/* printf("FileHandle=%llu\n",fh); */

	return fh;
}

haddr_t
f_h5pt_openw_par (
	char *file,
	MPI_Comm *c,
	int flen ) { /* func returns INT8 */

	haddr_t fh;
	H5PartFile* f;
	char *newname = (char*)malloc(flen+1); /* be safe? */
	strncpy(newname,file,flen);
	newname[flen]='\0';
	f = H5PartOpenFileParallel(newname,H5PART_WRITE,*((MPI_Comm*)c));
	/*  printf("openr file=[%s] flen=%u haddr=%u\n",file,flen,f); */
	fh = (haddr_t)f;
	/* printf("FileHandle=%llu\n",fh); */
	
	return fh;
}
#endif

h5part_int64_t
f_h5pt_close (
	haddr_t *file
	) {
	return H5PartCloseFile((H5PartFile*)*file);
}

/*==============Writing and Setting Dataset info========*/
#if 0
h5part_int64_t
f_h5pt_readstep (
	haddr_t *file,
	h5part_int64_t *step,
	h5part_float64_t *x,
	h5part_float64_t *y,
	h5part_float64_t *z,
	h5part_float64_t *px,
	h5part_float64_t *py,
	h5part_float64_t *pz,
	h5part_int64_t *id
	) {

	return H5PartReadParticleStep((H5PartFile*)*file,(*step)-1,x,y,z,px,py,pz,id);
}
#endif

h5part_int64_t
f_h5pt_setnpoints (
	haddr_t *file,
	h5part_int64_t *np
	) {

	return H5PartSetNumParticles((H5PartFile*)*file,*np);
}

h5part_int64_t
f_h5pt_setstep (
	haddr_t *file,
	h5part_int64_t *step ) {

	return H5PartSetStep((H5PartFile*)*file,(*step)-1);
}

h5part_int64_t
f_h5pt_writedata_r8 (
	haddr_t *file,
	char *name,
	h5part_float64_t *data,
	int flen ) {

	h5part_int64_t rc;

	char *newname = (char*)malloc(flen+1); /* be safe? */
	strncpy(newname,name,flen);
	newname[flen]='\0';
	rc = H5PartWriteDataFloat64((H5PartFile*)*file,newname, data);
	free(newname);

	return rc;
}

h5part_int64_t
f_h5pt_writedata_i8 (
	haddr_t *file,
	char *name,
	h5part_int64_t *data,
	int flen ) {

	h5part_int64_t rc;

	char *newname = (char*)malloc(flen+1); /* be safe? */
	strncpy(newname,name,flen);
	newname[flen]='\0';
	rc = H5PartWriteDataInt64((H5PartFile*)*file,newname,data);
	free(newname);

	return rc;
}

/*==============Reading Data Characteristics============*/

h5part_int64_t
f_h5pt_getnsteps (
	haddr_t *file 
	) {

	/* printf("nstep Haddr=%llu\n",file); */
	return H5PartGetNumSteps((H5PartFile*)*file);
}

h5part_int64_t
f_h5pt_getndatasets (
	haddr_t *file
	) {
	/* printf("ndata Haddr=%llu\n",file); */
	return H5PartGetNumDatasets((H5PartFile*)*file);
}

h5part_int64_t
f_h5pt_getnpoints (
	haddr_t *file
	) {
	/*  printf("nprt Haddr=%llu\n",file); */
	return H5PartGetNumParticles((H5PartFile*)*file);
}

/* probably should get index from name */
h5part_int64_t
f_h5pt_getdatasetname ( 
	haddr_t *file,
	h5part_int64_t *index,
	char *name,
	int namelen){

	return H5PartGetDatasetName((H5PartFile *)*file,*index,name,namelen);
}

h5part_int64_t
f_h5pt_getnumpoints ( 
	haddr_t *file) {

	return H5PartGetNumParticles((H5PartFile*)*file);
}

/*=============Setting and getting views================*/

h5part_int64_t
f_h5pt_setview (
	haddr_t *file,
	h5part_int64_t *start,
	h5part_int64_t *end ) {

	return H5PartSetView((H5PartFile*)*file,*start,*end);
}

h5part_int64_t
f_h5pt_resetview (
	haddr_t *file
	) {

	return H5PartResetView((H5PartFile*)*file);
}

h5part_int64_t
f_h5pt_hasview (
	haddr_t *file
	) {

	return H5PartHasView(((H5PartFile*)*file));
}

h5part_int64_t
f_h5pt_getview (
	haddr_t *file,
	h5part_int64_t *start,
	h5part_int64_t *end
	) {

	return H5PartGetView((H5PartFile*)*file, start, end);
}
/*==================Reading data ============*/
h5part_int64_t
f_h5pt_readdata_r8 (
	haddr_t *file,
	char *name,
	h5part_float64_t *array,
	int namelen
	) {

	h5part_int64_t rc;
	char *newname = (char*)malloc(namelen+1); /* be safe? */
	strncpy(newname,name,namelen);
	newname[namelen]='\0';
	rc = H5PartReadDataFloat64((H5PartFile*)*file,newname, array);
	free(newname);
	return rc;
}
h5part_int64_t
f_h5pt_readdata_i8 (
	haddr_t *file,
	char *name,
	h5part_int64_t *array,
	int namelen ) {

	h5part_int64_t rc;
	char *newname = (char*)malloc(namelen+1); /* be safe? */
	strncpy(newname,name,namelen);
	newname[namelen]='\0';
	rc = H5PartReadDataInt64((H5PartFile*)*file,newname, array);
	free(newname);
	return rc;
}

/*=================== Attributes ================*/

/* Attributes */
/* writing */
h5part_int64_t
f_h5pt_writefileattrib_r8 (
	haddr_t *f,
	char *name,
	void *attrib,
	h5part_int64_t *nelem,
	int namelen ) {
	
	h5part_int64_t rc;
	char *newname = (char*)malloc(namelen+1); /* be safe? */
	strncpy(newname,name,namelen);
	newname[namelen]='\0';
	rc = H5PartWriteFileAttrib((H5PartFile *)*f,newname,
				H5T_NATIVE_DOUBLE,attrib,*nelem);
	free(newname);
	return rc;
}

h5part_int64_t
f_h5pt_writefileattrib_i8 (
	haddr_t *f,
	char *name,
	void *attrib,
	h5part_int64_t *nelem,
	int namelen 
	) {
	
	h5part_int64_t rc;
	char *newname = (char*)malloc(namelen+1); /* be safe? */
	strncpy(newname,name,namelen);
	newname[namelen]='\0';
	rc = H5PartWriteFileAttrib((H5PartFile *)*f,newname,
				   H5T_NATIVE_INT64,attrib,*nelem);
	free(newname);
	return rc;
}

h5part_int64_t
f_h5pt_writefileattrib_string (
	haddr_t *f,
	char *name,
	char *attrib,
	int namelen,
	int attriblen
	) {
	
	h5part_int64_t rc;
	char *newattrib;
	char *newname;
	newname = (char*)malloc(namelen+1); /* be safe? */
	strncpy(newname,name,namelen);
	newname[namelen]='\0';
	newattrib = (char*)malloc(attriblen+1); /* be safe? */
	strncpy(newattrib,attrib,attriblen);
	newattrib[attriblen]='\0';
	rc = H5PartWriteFileAttribString((H5PartFile *)*f,newname,newattrib);
	free(newname);
	free(newattrib);
	return rc;
}

h5part_int64_t
f_h5pt_writestepattrib_r8 ( 
	haddr_t *f,
	char *name,
	void *attrib,
	h5part_int64_t *nelem,
	int namelen){

	h5part_int64_t rc;
	char *newname = (char*)malloc(namelen+1); /* be safe? */
	strncpy(newname,name,namelen);
	newname[namelen]='\0';
	rc = H5PartWriteStepAttrib((H5PartFile *)*f,newname,
				H5T_NATIVE_DOUBLE,attrib,*nelem);
	free(newname);
	return rc;
}

h5part_int64_t
f_h5pt_writestepattrib_i8 (
	haddr_t *f,
	char *name,
	void *attrib,
	h5part_int64_t *nelem,
	int namelen 
	) {

	h5part_int64_t rc;
	char *newname = (char*)malloc(namelen+1); /* be safe? */
	strncpy(newname,name,namelen);
	newname[namelen]='\0';
	rc = H5PartWriteStepAttrib((H5PartFile *)*f,newname,
				   H5T_NATIVE_INT64,attrib,*nelem);
	free(newname);
	return rc;
}

h5part_int64_t
f_h5pt_writestepattrib_string (
	haddr_t *f,
	char *name,
	char *attrib,
	int namelen,
	int attriblen ) {

	h5part_int64_t rc;
	char *newattrib;
	char *newname;
	newname = (char*)malloc(namelen+1); /* be safe? */
	strncpy(newname,name,namelen);
	newname[namelen]='\0';
	newattrib = (char*)malloc(attriblen+1); /* be safe? */
	strncpy(newattrib,attrib,attriblen);
	newattrib[attriblen]='\0';
	rc = H5PartWriteStepAttribString((H5PartFile *)*f,newname,newattrib);
	free(newname);
	free(newattrib);
	return rc;
}

/* reading attributes ************************* */
h5part_int64_t
f_h5pt_getnstepattribs (
	haddr_t *f
	) {

	return H5PartGetNumStepAttribs((H5PartFile*)*f);
}

h5part_int64_t
f_h5pt_getnfileattribs (
	haddr_t *f
	) {

	return H5PartGetNumFileAttribs((H5PartFile*)*f);
}

h5part_int64_t
f_h5pt_getstepattribinfo (
	haddr_t *f,
	h5part_int64_t *idx,
	char *name,
	h5part_int64_t *nelem,
	int maxnamelen
	) {

	h5part_int64_t type;
	return H5PartGetStepAttribInfo((H5PartFile*)*f,*idx,name,maxnamelen,&type,nelem);
}

h5part_int64_t
f_h5pt_getfileattribinfo (
	haddr_t *f,
	h5part_int64_t *idx,
	char *name,
	h5part_int64_t *nelem,
	int maxnamelen ) {

	h5part_int64_t type;
	return H5PartGetFileAttribInfo((H5PartFile*)*f,*idx,name,maxnamelen,&type,nelem);
}

h5part_int64_t
f_h5pt_readstepattrib (
	haddr_t *f,
	char *name,
	void *data,
	int namelen
	) {
	
	h5part_int64_t rc;
	char *newname = (char*)malloc(namelen+1); /* be safe? */
	strncpy(newname,name,namelen);
	newname[namelen]='\0';
	rc = H5PartReadStepAttrib((H5PartFile*)*f,newname,data);
	free( newname );
	return rc;
}

h5part_int64_t
f_h5pt_readfileattrib (
	haddr_t *f,
	char *name,
	void *data,
	int namelen
	) {
	
	int rc;
	char *newname = (char*)malloc(namelen+1); /* be safe? */
	strncpy(newname,name,namelen);
	newname[namelen]='\0';
	rc = H5PartReadFileAttrib((H5PartFile*)*f,newname,data);
	free( newname );
	return rc;
}

h5part_int64_t
f_h5pt_set_verbosity_level (
	h5part_int64_t level
	) {
	return H5PartSetVerbosityLevel ( level );
}
