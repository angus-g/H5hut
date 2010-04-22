#ifndef __H5T_RETRIEVE_H
#define __H5T_RETRIEVE_H

h5_err_t h5t_begin_traverse_vertices ( h5_file_t * const f );
h5_id_t h5t_traverse_vertices ( h5_file_t * const f, h5_float64_t P[3] );
h5_err_t h5t_end_traverse_vertices ( h5_file_t * f );

h5_err_t h5t_begin_traverse_edges ( h5_file_t * const f );
h5_id_t h5t_traverse_edges ( h5_file_t * const f, h5_id_t * const vids );
h5_err_t h5t_end_traverse_edges ( h5_file_t * f );

h5_err_t h5t_begin_traverse_triangles ( h5_file_t * const f );
h5_id_t h5t_traverse_triangles ( h5_file_t * const f, h5_id_t * const vids );
h5_err_t h5t_end_traverse_triangles ( h5_file_t * const f );

h5_err_t h5t_begin_traverse_elems ( h5_file_t * const f );
h5_id_t h5t_traverse_elems ( h5_file_t * const f, h5_id_t * const vids );
h5_err_t h5t_end_traverse_elems ( h5_file_t * const f );
#endif
