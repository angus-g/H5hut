#include <stdlib.h>

#include <hdf5.h>
#include "h5_types.h"
#include "h5_core.h"
#include "h5_private.h"


h5_err_t
_h5_alloc_idmap (
	struct idmap	*map,
	h5_size_t	size
	) {
	map->items = realloc ( map->items, size * sizeof ( map->items[0] ) );
	if ( map->items == NULL ) {
		return HANDLE_H5_NOMEM_ERR;
	}
	map->size = size;
	return H5_SUCCESS;
}


h5_err_t
_h5_insert_idmap (
	struct idmap *map,
	h5_id_t global_id,
	h5_id_t local_id
	) {

	if ( map->num_items == map->size )
		return HANDLE_H5_OVERFLOW_ERR( "g2lmap", map->size );

	h5_id_t i = _h5_search_idmap ( map, global_id );
	if ( i >= 0 )			/* global id already in use ? */
		return -1;

	i = -(i+1);

	memmove ( 
		&map->items[i+1],
		&map->items[i],
		map->num_items - i );
	map->items[i].global_id = global_id;
	map->items[i].local_id = local_id;
	map->num_items++;

	return H5_SUCCESS;
}

/*!

  \ingroup h5_core

  binary search in simple map

  \return index in array if found, othwise \c -(result+1) is the index
  where \c value must be inserted.

 */
h5_id_t
_h5_search_idmap (
	struct idmap *map,
	h5_id_t value
	) {

	register int low = 0;
	register int high = map->num_items - 1;
	while (low <= high) {
		register int mid = (low + high) / 2;
		register h5_id_t diff = map->items[mid].global_id - value;
           	if ( diff > 0 )
               		high = mid - 1;
           	else if ( diff < 0 )
               		low = mid + 1;
           	else
               		return mid; // found
       	}
       	return -(low+1);  // not found
}

h5_id_t
h5t_map_vertex_id_global2local (
	h5_file *f,
	h5_id_t global_id
	) {
	struct h5t_fdata *t = &f->t;

	h5_id_t local_id = _h5_search_idmap ( &t->map_vertex_g2l, global_id );
	if ( local_id < 0 ) 
		return HANDLE_H5T_GID_NOT_EXIST_ERR ( "vertex", global_id );
	return local_id;
}
