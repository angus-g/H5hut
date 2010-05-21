#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>	/* va_arg - System dependent ?! */
#include <string.h>
#include <errno.h>
#include "h5_core.h"
#include "h5_core_private.h"


h5_err_t
h5tpriv_error_local_elem_nexist (
	h5_file_t * const f,
	h5_id_t local_vids[]
	) {
	h5t_fdata_t* t = f->t;
	char s[1024];

	int num_chars_printed = snprintf (s, sizeof(s), "%lld,", local_vids[0]);
	int i;
	int num_vertices = t->ref_element->num_faces[0];
	for (i = 1; i < num_vertices; i++) {
		num_chars_printed += snprintf (
			s+num_chars_printed, sizeof(s)-num_chars_printed,
			"%lld,", local_vids[i]);
		if ((sizeof(s) - num_chars_printed) < 32) {
			// buffer to small
			return h5_error_internal (f, __FILE__, __func__, __LINE__);
		}
	}

	return h5_error (
		f,
		H5_ERR_NOENTRY,
		"Element with local vertex IDs "
		"(%s) doesn't exist!",
		s );
}
	
