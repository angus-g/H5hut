#ifndef __H5BLOCKPRIVATE_H
#define __H5BLOCKPRIVATE_H

#define H5BLOCK_GROUPNAME_BLOCK	"Block"

#define INIT( f ) { \
	h5part_int64_t herr = _H5Block_init ( f ); \
	if ( herr < 0 ) return herr; \
}

h5part_int64_t
_H5Block_init (
	H5PartFile *f
	);

h5part_int64_t
_H5Block_close (
	H5PartFile *f
	);

h5part_int64_t
_H5Block_open_field_group (
	H5PartFile *f,
	const char *name
	);

h5part_int64_t
_H5Block_close_field_group (
	H5PartFile *f
	);

h5part_int64_t
_H5Block_create_field_group (
	H5PartFile *f,
	const char *name
	);

h5part_int64_t
_H5Block_write_data (
	H5PartFile *f,
	const char *name,
	const void *data,
	const hid_t type
	);

h5part_int64_t
_H5Block_read_data (
	H5PartFile *f,
	const char *name,
	void *data,
	hid_t type
	);

#endif
