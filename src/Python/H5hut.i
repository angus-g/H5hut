%module H5hut
%{
#define SWIG_FILE_WITH_INIT
#include <stdint.h>
#include "h5core/h5_types.h"
//#include "H5.h"
#include "H5hut.h"
%}

%import <stdint.i>

%include numpy.i

%apply unsigned long int { h5_prop_t };
%apply unsigned long int { h5_file_t };


%apply (unsigned long long* IN_ARRAY1) { h5_size_t* };
%apply (unsigned int* IN_ARRAY1) { h5_uint32_t* }; //uint32_t
%apply (unsigned long long* IN_ARRAY1) { h5_uint64_t* }; //uint64_t
%apply (int* IN_ARRAY1) { h5_int32_t* }; //int32_t
%apply (long long* IN_ARRAY1) { h5_int64_t* }; //int64_t
%apply (float* IN_ARRAY1) { h5_float32_t* };
%apply (double* IN_ARRAY1) { h5_float64_t* };


%init %{
import_array();
%}


%ignore h5_report_errorhandler;
%ignore h5_abort_errorhandler;
%ignore h5priv_vprintf;
%ignore h5_verror;
%ignore H5ReportErrorhandler;
%ignore H5AbortErrorhandler;

%include "h5core/h5_types.h"
//%include "H5.h"
//%include "H5_attribs.h"
%include "H5_model.h"
%include "H5hut.h"
%include "H5Block_attribs.h"
//%include "H5Block.h"
%include "H5Block_io.h"
%include "H5Block_model.h"
//%include "H5Part.h"
%include "H5Part_io.h"
%include "H5Part_model.h"
//%include "H5Fed_adjacency.h"
//%include "H5Fed.h"
//%include "H5Fed_model.h"
//%include "H5Fed_retrieve.h"
//%include "H5Fed_store.h"
//%include "H5Fed_tags.h"


