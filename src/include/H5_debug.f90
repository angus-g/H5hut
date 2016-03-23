!
!  Copyright (c) 2006-2016, The Regents of the University of California,
!  through Lawrence Berkeley National Laboratory (subject to receipt of any
!  required approvals from the U.S. Dept. of Energy) and the Paul Scherrer
!  Institut (Switzerland).  All rights reserved.!
!
!  License: see file COPYING in top level of source distribution.
!
  INTERFACE

     !> \addtogroup h5_debug_f
     !! @{
     
     !>
     !! Set debug mask. The debug mask is an or'ed value of
     !!
     !! - \c H5_DEBUG_API:	    C-API calls
     !! - \c H5_DEBUG_CORE_API:   core API calls. The core API is used by the C- and Fortran API.
     !! - \c H5_DEBUG_PRIV_API:   private API calls
     !! - \c H5_DEBUG_PRIV_FUNC:  static functions
     !! - \c H5_DEBUG_HDF5:	    HDF5 wrapper calls
     !! - \c H5_DEBUG_MPI:	    MPI wrapper calls
     !! - \c H5_DEBUG_MALLOC:	    memory allocation
     !! - \c H5_DEBUG_ALL:	    enable all
     !!
     !! \return \c H5_SUCCESS
     !!
     !! \see h5_get_debug_mask()

     SUBROUTINE h5_set_debug_mask ( mask )
       INTEGER*8, INTENT(IN) :: mask   !< [in] debug mask
     END SUBROUTINE h5_set_debug_mask

     !>
     !! Get debug mask.
     !!
     !! \return   debug mask
     !!
     !! \see h5_set_debug_mask()

     INTEGER*8 FUNCTION h5_get_debug_mask ()
     END FUNCTION h5_get_debug_mask

     !> @}
  END INTERFACE
