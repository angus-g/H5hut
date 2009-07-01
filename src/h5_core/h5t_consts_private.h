#ifndef __H5T_CONSTS_PRIVATE_H
#define __H5T_CONSTS_PRIVATE_H

extern char * _h5t_oid_names[];
extern char * _h5t_meshes_grpnames[];
extern size_t _h5t_sizeof_elem[];

const char *
_h5t_map_oid2str (
	h5_id_t oid
	);

#endif
