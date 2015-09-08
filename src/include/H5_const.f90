!
!  Copyright (c) 2006-2015, The Regents of the University of California,
!  through Lawrence Berkeley National Laboratory (subject to receipt of any
!  required approvals from the U.S. Dept. of Energy) and the Paul Scherrer
!  Institut (Switzerland).  All rights reserved.!
!
!  License: see file COPYING in top level of source distribution.
!
  INTEGER*8, PARAMETER :: H5_O_RDWR =              Z'00000001'
  INTEGER*8, PARAMETER :: H5_O_RDONLY =            Z'00000002'
  INTEGER*8, PARAMETER :: H5_O_WRONLY =            Z'00000004'
  INTEGER*8, PARAMETER :: H5_O_APPEND =            Z'00000008'

  INTEGER*8, PARAMETER :: H5_VFD_MPIPOSIX =        Z'00000010'
  INTEGER*8, PARAMETER :: H5_VFD_MPIIO_IND =       Z'00000020'
  INTEGER*8, PARAMETER :: H5_VFD_CORE =            Z'00000040'

  INTEGER*8, PARAMETER :: H5_PROP_DEFAULT =        0

  INTEGER*8, PARAMETER :: H5_STRING_T =            1
  INTEGER*8, PARAMETER :: H5_INT16_T =             2
  INTEGER*8, PARAMETER :: H5_INT32_T =             3
  INTEGER*8, PARAMETER :: H5_INT64_T =             4
  INTEGER*8, PARAMETER :: H5_FLOAT32_T =           5
  INTEGER*8, PARAMETER :: H5_FLOAT64_T =           6

  INTEGER*8, PARAMETER :: H5_MAX_NAME_LEN =        64

  INTEGER*8, PARAMETER :: H5_SUCCESS =             0
  INTEGER*8, PARAMETER :: H5_OK =                  H5_SUCCESS
  INTEGER*8, PARAMETER :: H5_NOK =                 -1
  INTEGER*8, PARAMETER :: H5_FAILURE =             -2
  INTEGER*8, PARAMETER :: H5_ERR_BADF =            -9
  INTEGER*8, PARAMETER :: H5_ERR_NOMEM =           -12
  INTEGER*8, PARAMETER :: H5_ERR_INVAL =           -22
  INTEGER*8, PARAMETER :: H5_ERR_BADFD =           -77
  
  INTEGER*8, PARAMETER :: H5_ERR_LAYOUT =          -100
  INTEGER*8, PARAMETER :: H5_ERR_NOENTRY =         -101

  INTEGER*8, PARAMETER :: H5_ERR_MPI =             -201
  INTEGER*8, PARAMETER :: H5_ERR_HDF5 =            -202
  INTEGER*8, PARAMETER :: H5_ERR_H5 =              -203
  INTEGER*8, PARAMETER :: H5_ERR_H5PART =          -204
  INTEGER*8, PARAMETER :: H5_ERR_H5BLOCK =         -205
  INTEGER*8, PARAMETER :: H5_ERR_H5FED =           -206
  
  INTEGER*8, PARAMETER :: H5_ERR_INTERNAL =        -253
  INTEGER*8, PARAMETER :: H5_ERR_NOT_IMPLEMENTED = -254

  INTEGER*8, PARAMETER :: H5_VERBOSE_NONE =        0
  INTEGER*8, PARAMETER :: H5_VERBOSE_ERROR =       1
  INTEGER*8, PARAMETER :: H5_VERBOSE_WARN =        2
  INTEGER*8, PARAMETER :: H5_VERBOSE_INFO =        3

  INTEGER*8, PARAMETER :: H5_VERBOSE_DEFAULT =     H5_VERBOSE_ERROR

  INTEGER*8, PARAMETER :: H5_DEBUG_USER =          4
  INTEGER*8, PARAMETER :: H5_DEBUG_API =           8
  INTEGER*8, PARAMETER :: H5_DEBUG_CORE_API =      16
  INTEGER*8, PARAMETER :: H5_DEBUG_PRIV_API =      32
  INTEGER*8, PARAMETER :: H5_DEBUG_PRIV_FUNC =     64
  INTEGER*8, PARAMETER :: H5_DEBUG_HDF5 =          128
  INTEGER*8, PARAMETER :: H5_DEBUG_MPI =           256
  INTEGER*8, PARAMETER :: H5_DEBUG_MALLOC =        512
  INTEGER*8, PARAMETER :: H5_DEBUG_CLIB =          1024
  
  INTEGER*8, PARAMETER :: H5_DEBUG_ALL =           2048-4

