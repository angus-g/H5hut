#include "h5_types.h"
#include "h5_types_private.h"
#include "h5t_types_private.h"

const char * _h5t_oid_names[] = {
	"N.N.",
	"vertex",
	"edge",
	"triangle",
	"tetrahedron"
};

const char * _h5t_meshes_grpnames[] = {
	"N.N.",
	"N.N.",
	"N.N.",
	"TriangleMeshes",
	"TetMeshes"
};

const size_t _h5t_sizeof_elem[] = {
	0,
	0,
	0,
	sizeof(h5_triangle_t),
	sizeof(h5_tetrahedron_t)
};

const char *
_h5t_map_oid2str (
	h5_oid_t oid
	) {
	if ( oid < 0 || oid >= sizeof(_h5t_oid_names)/sizeof(char*) ) {
		return "[invalid oid]";
	}
	return _h5t_oid_names[oid];
}
