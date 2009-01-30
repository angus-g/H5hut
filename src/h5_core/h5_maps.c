#include <stdlib.h>

#include <hdf5.h>
#include "h5_types.h"
#include "h5_core.h"
#include "h5_core_private.h"

h5_err_t
_h5_alloc_smap (
	h5_file_t * const f,
	struct smap	*map,
	const h5_size_t	size
	) {
	int new = ( map->items == NULL );
	size_t size_in_bytes = size * sizeof ( map->items[0] );
	TRY( map->items = _h5_alloc ( f, map->items, size_in_bytes ) );
	map->size = size;
	if ( new ) map->num_items = 0;
	return H5_SUCCESS;
}

h5_err_t
_h5_alloc_idmap (
	h5_file_t * const f,
	struct idmap	*map,
	const h5_size_t	size
	) {
	int new = ( map->items == NULL );
	size_t size_in_bytes = size * sizeof ( map->items[0] );
	TRY( map->items = _h5_alloc ( f, map->items, size_in_bytes ) );
	map->size = size;
	if ( new ) map->num_items = 0;
	return H5_SUCCESS;
}


h5_err_t
_h5_insert_idmap (
	h5_file_t * const f,
	struct idmap *map,
	h5_id_t global_id,
	h5_id_t local_id
	) {

	if ( map->num_items == map->size )
		return HANDLE_H5_OVERFLOW_ERR( f, "g2lmap", map->size );

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

  binary search in id map. 

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

int
_cmp_idmap (
	const void *id1,
	const void *id2
	) {
	
	return *(h5_id_t*)id1 - *(h5_id_t*)id2;
}

h5_err_t
_h5_sort_idmap (
	struct idmap *map
	) {
	qsort ( map->items, map->num_items, sizeof(map->items[0]), _cmp_idmap );
	return H5_SUCCESS;
}
