#include <assert.h>
#include <stdlib.h>

#include "h5_core.h"

/*!
  Compare two floating point numbers using integers. See
  http://www.cygnus-software.com/papers/comparingfloats/comparingfloats.htm
  for a detailed explanation.
*/
h5_int64_t
h5priv_fcmp (
	h5_float64_t A,
	h5_float64_t B,
	h5_int32_t maxUlps ) {

	// Make sure maxUlps is non-negative and small enough that the
	// default NAN won't compare as equal to anything.
	assert (maxUlps > 0 && maxUlps < 4 * 1024 * 1024);
	assert (sizeof (long long) == sizeof (h5_int64_t) );

	// Make [ab]Int lexicographically ordered as a twos-complement int
	h5_int64_t aInt = *(h5_int64_t*)&A;
	if (aInt < 0)
		aInt = 0x8000000000000000LL - aInt;

	h5_int64_t bInt = *(h5_int64_t*)&B;
	if (bInt < 0)
		bInt = 0x8000000000000000LL - bInt;

	h5_int64_t intDiff = aInt - bInt;
	if (llabs(intDiff) <= maxUlps)
		return 0;
	return intDiff;
}
