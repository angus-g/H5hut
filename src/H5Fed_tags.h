#ifndef __H5FED_TAGS_H
#define __H5FED_TAGS_H
h5_err_t	H5FedAddMTagset ( h5_file_t * const f,
				  char * name,
				  h5_id_t type );
h5_err_t	H5FedRemoveMTagset ( h5_file_t *const f,
				     char name[] );
h5_size_t	H5FedGetMTagsets ( h5_file_t *const f,
				   char **names[] );
h5_id_t		H5FedGetTypeOfMTagset ( h5_file_t *const f,
					char name[] );
h5_err_t	H5FedSetMTag ( h5_file_t *const f,
			       char name[], h5_id_t id,
			       const size_t dims, void *val );
h5_err_t	H5FedSetMTagToVertex ( h5_file_t *const f,
				       char name[], h5_id_t id,
				       const size_t dims, void *val );
h5_err_t	H5FedSetMTagToEdge ( h5_file_t *const f,
				     char name[], h5_id_t id,
				     const size_t dims, void *val );
h5_err_t	H5FedSetMTagToTriangle ( h5_file_t *const f,
					 char name[], h5_id_t id,
					 const size_t dims, void *val );
h5_err_t	H5FedSetMTagToTet ( h5_file_t *const f,
				    char name[], h5_id_t id,
				    const size_t dims, void *val );

h5_err_t	H5FedGetMTag ( h5_file_t *const f,
			       const char name[], const h5_id_t id, 
			       size_t *dims, void *val );
h5_err_t	H5FedRemoveMTag ( h5_file_t *const f,
				  const char name[], const h5_id_t id );

/*
  Get descriptor for a tagset
  Get tag value by descriptor
  get size of value
  Get tagset names for specific entity
*/
#endif