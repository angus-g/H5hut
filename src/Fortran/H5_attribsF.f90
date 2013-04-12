!   __ _ _              _   _        _ _           _            
!  / _(_) | ___    __ _| |_| |_ _ __(_) |__  _   _| |_ ___  ___ 
! | |_| | |/ _ \  / _` | __| __| '__| | '_ \| | | | __/ _ \/ __|
! |  _| | |  __/ | (_| | |_| |_| |  | | |_) | |_| | ||  __/\__ \
! |_| |_|_|\___|  \__,_|\__|\__|_|  |_|_.__/ \__,_|\__\___||___/
!   __ _ _   _  ___ _ __ _   _ 
!  / _` | | | |/ _ \ '__| | | |
! | (_| | |_| |  __/ |  | |_| |
!  \__, |\__,_|\___|_|   \__, |
!     |_|                |___/

!>
!! \ingroup h5hut_attrib_f
!! See \ref H5GetNumFileAttribs
!! \return number of attributes or error code
!<
INTEGER*8 FUNCTION h5_getnfileattribs (filehandle)
    INTEGER*8, INTENT(IN) :: filehandle         !< file handle
END FUNCTION
  
!>
!! \ingroup h5hut_attrib_f
!! See \ref H5GetFileAttribInfo
!! \return 0 on success or error code
!<
INTEGER*8 FUNCTION h5_getfileattribinfo (filehandle, idx, attrib_name, attrib_nelem)
    INTEGER*8,INTENT(IN) :: filehandle          !< file handle
    INTEGER*8,INTENT(IN) :: idx                 !< index of attribute being queried
    CHARACTER(LEN=*), INTENT(OUT):: attrib_name !< name of attribute
    INTEGER*8,INTENT(OUT):: attrib_type         !< type of attribute
    INTEGER*8,INTENT(OUT):: attrib_nelem        !< number of elements in the attrib array
END FUNCTION

!   __ _ _              _   _        _ _           _            
!  / _(_) | ___    __ _| |_| |_ _ __(_) |__  _   _| |_ ___  ___ 
! | |_| | |/ _ \  / _` | __| __| '__| | '_ \| | | | __/ _ \/ __|
! |  _| | |  __/ | (_| | |_| |_| |  | | |_) | |_| | ||  __/\__ \
! |_| |_|_|\___|  \__,_|\__|\__|_|  |_|_.__/ \__,_|\__\___||___/
!  _    __          _        _             
! (_)  / /__    ___| |_ _ __(_)_ __   __ _ 
! | | / / _ \  / __| __| '__| | '_ \ / _` |
! | |/ / (_) | \__ \ |_| |  | | | | | (_| |
! |_/_/ \___/  |___/\__|_|  |_|_| |_|\__, |
!                                    |___/

!>
!! \ingroup h5hut_attrib_f
!! See \ref H5WriteFileAttribString
!! \return 0 on success or error code
!<
INTEGER*8 FUNCTION h5_writefileattrib_string (filehandle, attrib_name, attrib_value)
    INTEGER*8, INTENT(IN) :: filehandle         !< file handle
    CHARACTER(LEN=*), INTENT(IN) :: attrib_name !< name of attribute to write
    CHARACTER(LEN=*), INTENT(IN) :: attrib_value!< attribute data to be written
END FUNCTION
  
!>
!! \ingroup h5hut_attrib_f
!! \return 0 on success or error code
!<
INTEGER*8 FUNCTION h5_readfileattrib_string (filehandle, attrib_name, attrib_value)
    INTEGER*8, INTENT(IN) :: filehandle         !< file handle
    CHARACTER(LEN=*), INTENT(IN) :: attrib_name !< name of the attribute to read
    CHARACTER(LEN=*), INTENT(IN) :: attrib_value!< attribute data will be read into this array
END FUNCTION

!   __ _ _              _   _        _ _           _            
!  / _(_) | ___    __ _| |_| |_ _ __(_) |__  _   _| |_ ___  ___ 
! | |_| | |/ _ \  / _` | __| __| '__| | '_ \| | | | __/ _ \/ __|
! |  _| | |  __/ | (_| | |_| |_| |  | | |_) | |_| | ||  __/\__ \
! |_| |_|_|\___|  \__,_|\__|\__|_|  |_|_.__/ \__,_|\__\___||___/
!  _    __                     _ 
! (_)  / /__    _ __ ___  __ _| |
! | | / / _ \  | '__/ _ \/ _` | |
! | |/ / (_) | | | |  __/ (_| | |
! |_/_/ \___/  |_|  \___|\__,_|_|

!>
!! \ingroup h5hut_attrib_f
!! See \ref H5WriteFileAttribFloat64
!! \return 0 on success or error code
!<
INTEGER*8 FUNCTION h5_writefileattrib_r8 (filehandle, attrib_name, attrib_value, attrib_nelem)
    INTEGER*8,INTENT(IN) :: filehandle          !< file handle
    CHARACTER(LEN=*), INTENT(IN) :: attrib_name !< name of attribute to read
    REAL*8,   INTENT(OUT):: attrib_value(*)     !< attribute data to be written
    INTEGER*8, INTENT(IN) :: attrib_nelem       !< number of elements in data array
END FUNCTION

!>
!! \ingroup h5hut_attrib_f
!! See \ref H5ReadFileAttribFloat64
!! \return 0 on success or error code
!<
INTEGER*8 FUNCTION h5_readfileattrib_r8 (filehandle, attrib_name, attrib_value)
    INTEGER*8,INTENT(IN) :: filehandle          !< file handle
    CHARACTER(LEN=*), INTENT(IN) :: attrib_name !< name of attribute to read
    REAL*8,   INTENT(OUT):: attrib_value(*)     !< attribute data will be read into this array
END FUNCTION

!>
!! \ingroup h5hut_attrib_f
!! See \ref H5WriteFileAttribFloat32
!! \return 0 on success or error code
!<
INTEGER*8 FUNCTION h5_writefileattrib_r4 (filehandle, attrib_name, attrib_value, attrib_nelem)
    INTEGER*8,INTENT(IN) :: filehandle          !< file handle
    CHARACTER(LEN=*), INTENT(IN) :: attrib_name !< name of attribute to read
    REAL*4,   INTENT(OUT):: attrib_value(*)     !< attribute data to be written
    INTEGER*8, INTENT(IN) :: attrib_nelem       !< number of elements in data array
END FUNCTION

!>
!! \ingroup h5hut_attrib_f
!! See \ref H5ReadFileAttribFloat32
!! \return 0 on success or error code
!<
INTEGER*8 FUNCTION h5_readfileattrib_r4 ( filehandle, attrib_name, attrib_value )
    INTEGER*8,INTENT(IN) :: filehandle          !< file handle
    CHARACTER(LEN=*), INTENT(IN) :: attrib_name !< name of attribute to read
    REAL*4,   INTENT(OUT):: attrib_value(*)     !< attribute data will be read into this array
END FUNCTION

!   __ _ _              _   _        _ _           _            
!  / _(_) | ___    __ _| |_| |_ _ __(_) |__  _   _| |_ ___  ___ 
! | |_| | |/ _ \  / _` | __| __| '__| | '_ \| | | | __/ _ \/ __|
! |  _| | |  __/ | (_| | |_| |_| |  | | |_) | |_| | ||  __/\__ \
! |_| |_|_|\___|  \__,_|\__|\__|_|  |_|_.__/ \__,_|\__\___||___/
!  _    __      _       _                       
! (_)  / /__   (_)_ __ | |_ ___  __ _  ___ _ __ 
! | | / / _ \  | | '_ \| __/ _ \/ _` |/ _ \ '__|
! | |/ / (_) | | | | | | ||  __/ (_| |  __/ |   
! |_/_/ \___/  |_|_| |_|\__\___|\__, |\___|_|   
!                               |___/

!>
!! \ingroup h5hut_attrib_f
!! See \ref H5WriteFileAttribInt64
!! \return 0 on success or error code
!<
INTEGER*8 FUNCTION h5_writefileattrib_i8 (filehandle, attrib_name, attrib_value, attrib_nelem)
    INTEGER*8,INTENT(IN) :: filehandle          !< file handle
    CHARACTER(LEN=*), INTENT(IN) :: attrib_name !< name of attribute to write
    INTEGER*8,INTENT(OUT):: attrib_value(*)     !< attribute data to be written
    INTEGER*8, INTENT(IN) :: attrib_nelem       !< number of elements in data array
END FUNCTION

!>
!! \ingroup h5hut_attrib_f
!! See \ref H5ReadFileAttribInt64
!! \return 0 on success or error code
!<
INTEGER*8 FUNCTION h5_readfileattrib_i8 (filehandle, attrib_name, attrib_value)
    INTEGER*8,INTENT(IN) :: filehandle          !< file handle
    CHARACTER(LEN=*), INTENT(IN) :: attrib_name !< name of attribute to read
    INTEGER*8,INTENT(OUT):: attrib_value(*)     !< attribute data will be read into this array
END FUNCTION

!>
!! \ingroup h5hut_attrib_f
!! See \ref H5WriteFileAttribInt32
!! \return 0 on success or error code
!<
INTEGER*8 FUNCTION h5_writefileattrib_i4 (filehandle, attrib_name, attrib_value, attrib_nelem)
    INTEGER*8,INTENT(IN) :: filehandle          !< file handle
    CHARACTER(LEN=*), INTENT(IN) :: attrib_name !< name of attribute to write
    INTEGER*8,INTENT(OUT):: attrib_type         !< type of attribute
    INTEGER*4,INTENT(OUT):: attrib_value(*)     !< attribute data to be written
    INTEGER*8, INTENT(IN) :: attrib_nelem       !< number of elements in data array
END FUNCTION

!>
!! \ingroup h5hut_attrib_f
!! See \ref H5ReadFileAttribInt32
!! \return 0 on success or error code
!<
INTEGER*8 FUNCTION h5_readfileattrib_i4 (filehandle, attrib_name, attrib_value)
    INTEGER*8,INTENT(IN) :: filehandle          !< file handle
    CHARACTER(LEN=*), INTENT(IN) :: attrib_name !< name of attribute to read
    INTEGER*4,INTENT(OUT):: attrib_value(*)     !< attribute data will be read into this array
END FUNCTION

!      _                     _   _        _ _           _            
!  ___| |_ ___ _ __     __ _| |_| |_ _ __(_) |__  _   _| |_ ___  ___ 
! / __| __/ _ \ '_ \   / _` | __| __| '__| | '_ \| | | | __/ _ \/ __|
! \__ \ ||  __/ |_) | | (_| | |_| |_| |  | | |_) | |_| | ||  __/\__ \
! |___/\__\___| .__/   \__,_|\__|\__|_|  |_|_.__/ \__,_|\__\___||___/
!             |_|
!   __ _ _   _  ___ _ __ _   _ 
!  / _` | | | |/ _ \ '__| | | |
! | (_| | |_| |  __/ |  | |_| |
!  \__, |\__,_|\___|_|   \__, |
!     |_|                |___/

!>
!! \ingroup h5hut_attrib_f
!! See \ref H5GetNumFileAttribs
!! \return number of attributes or error code
!<
INTEGER*8 FUNCTION h5_getnstepattribs (filehandle)
    INTEGER*8, INTENT(IN) :: filehandle         !< file handle
END FUNCTION
  
!>
!! \ingroup h5hut_attrib_f
!! See \ref H5GetFileAttribInfo
!! \return 0 on success or error code
!<
INTEGER*8 FUNCTION h5_getstepattribinfo (filehandle, idx, attrib_name, attrib_nelem)
    INTEGER*8,INTENT(IN) :: filehandle          !< file handle
    INTEGER*8,INTENT(IN) :: idx                 !< index of attribute being queried
    CHARACTER(LEN=*), INTENT(OUT):: attrib_name !< name of attribute
    INTEGER*8,INTENT(OUT):: attrib_nelem        !< number of elements in the attrib array
END FUNCTION

!      _                     _   _        _ _           _            
!  ___| |_ ___ _ __     __ _| |_| |_ _ __(_) |__  _   _| |_ ___  ___ 
! / __| __/ _ \ '_ \   / _` | __| __| '__| | '_ \| | | | __/ _ \/ __|
! \__ \ ||  __/ |_) | | (_| | |_| |_| |  | | |_) | |_| | ||  __/\__ \
! |___/\__\___| .__/   \__,_|\__|\__|_|  |_|_.__/ \__,_|\__\___||___/
!             |_|
!  _    __          _        _             
! (_)  / /__    ___| |_ _ __(_)_ __   __ _ 
! | | / / _ \  / __| __| '__| | '_ \ / _` |
! | |/ / (_) | \__ \ |_| |  | | | | | (_| |
! |_/_/ \___/  |___/\__|_|  |_|_| |_|\__, |
!                                    |___/

!>
!! \ingroup h5hut_attrib_f
!! See \ref H5WriteStepAttribString
!! \return 0 on success or error code
!<
INTEGER*8 FUNCTION h5_writestepattrib_string (filehandle, attrib_name, attrib_value)
    INTEGER*8, INTENT(IN) :: filehandle         !< file handle
    CHARACTER(LEN=*), INTENT(IN) :: attrib_name !< name of attribute to write
    CHARACTER(LEN=*), INTENT(IN) :: attrib_value!< attribute data to be written
END FUNCTION
  
!>
!! \ingroup h5hut_attrib_f
!! See \ref H5ReadStepAttribString
!! \return 0 on success or error code
!<
INTEGER*8 FUNCTION h5_readstepattrib_string (filehandle, attrib_name, attrib_value)
    INTEGER*8, INTENT(IN) :: filehandle         !< file handle
    CHARACTER(LEN=*), INTENT(IN) :: attrib_name !< name of the attribute to read
    CHARACTER(LEN=*), INTENT(IN) :: attrib_value!< attribute data will be read into this array
END FUNCTION

!      _                     _   _        _ _           _            
!  ___| |_ ___ _ __     __ _| |_| |_ _ __(_) |__  _   _| |_ ___  ___ 
! / __| __/ _ \ '_ \   / _` | __| __| '__| | '_ \| | | | __/ _ \/ __|
! \__ \ ||  __/ |_) | | (_| | |_| |_| |  | | |_) | |_| | ||  __/\__ \
! |___/\__\___| .__/   \__,_|\__|\__|_|  |_|_.__/ \__,_|\__\___||___/
!             |_|
!  _    __                     _ 
! (_)  / /__    _ __ ___  __ _| |
! | | / / _ \  | '__/ _ \/ _` | |
! | |/ / (_) | | | |  __/ (_| | |
! |_/_/ \___/  |_|  \___|\__,_|_|

!>
!! \ingroup h5hut_attrib_f
!! See \ref H5WriteStepAttribFloat64
!! \return 0 on success or error code
!<
INTEGER*8 FUNCTION h5_writestepattrib_r8 (filehandle, attrib_name, attrib_value, attrib_nelem)
    INTEGER*8,INTENT(IN) :: filehandle          !< file handle
    CHARACTER(LEN=*), INTENT(IN) :: attrib_name !< name of attribute to read
    REAL*8,   INTENT(OUT):: attrib_value(*)     !< attribute data to be written
    INTEGER*8, INTENT(IN) :: attrib_nelem       !< number of elements in data array
END FUNCTION

!>
!! \ingroup h5hut_attrib_f
!! See \ref H5ReadStepAttribFloat64
!! \return 0 on success or error code
!<
INTEGER*8 FUNCTION h5_readstepattrib_r8 (filehandle, attrib_name, attrib_value)
    INTEGER*8,INTENT(IN) :: filehandle          !< file handle
    CHARACTER(LEN=*), INTENT(IN) :: attrib_name !< name of attribute to read
    REAL*8,   INTENT(OUT):: attrib_value(*)     !< attribute data will be read into this array
END FUNCTION

!>
!! \ingroup h5hut_attrib_f
!! See \ref H5WriteStepAttribFloat32
!! \return 0 on success or error code
!<
INTEGER*8 FUNCTION h5_writestepattrib_r4 (filehandle, attrib_name, attrib_value, attrib_nelem)
    INTEGER*8,INTENT(IN) :: filehandle          !< file handle
    CHARACTER(LEN=*), INTENT(IN) :: attrib_name !< name of attribute to read
    REAL*4,   INTENT(OUT):: attrib_value(*)     !< attribute data to be written
    INTEGER*8, INTENT(IN) :: attrib_nelem       !< number of elements in data array
END FUNCTION

!>
!! \ingroup h5hut_attrib_f
!! See \ref H5ReadStepAttribFloat32
!! \return 0 on success or error code
!<
INTEGER*8 FUNCTION h5_readstepattrib_r4 ( filehandle, attrib_name, attrib_value )
    INTEGER*8,INTENT(IN) :: filehandle          !< file handle
    CHARACTER(LEN=*), INTENT(IN) :: attrib_name !< name of attribute to read
    REAL*4,   INTENT(OUT):: attrib_value(*)     !< attribute data will be read into this array
END FUNCTION

!      _                     _   _        _ _           _            
!  ___| |_ ___ _ __     __ _| |_| |_ _ __(_) |__  _   _| |_ ___  ___ 
! / __| __/ _ \ '_ \   / _` | __| __| '__| | '_ \| | | | __/ _ \/ __|
! \__ \ ||  __/ |_) | | (_| | |_| |_| |  | | |_) | |_| | ||  __/\__ \
! |___/\__\___| .__/   \__,_|\__|\__|_|  |_|_.__/ \__,_|\__\___||___/
!             |_|
!  _    __      _       _                       
! (_)  / /__   (_)_ __ | |_ ___  __ _  ___ _ __ 
! | | / / _ \  | | '_ \| __/ _ \/ _` |/ _ \ '__|
! | |/ / (_) | | | | | | ||  __/ (_| |  __/ |   
! |_/_/ \___/  |_|_| |_|\__\___|\__, |\___|_|   
!                               |___/

!>
!! \ingroup h5hut_attrib_f
!! See \ref H5WriteStepAttribInt64
!! \return 0 on success or error code
!<
INTEGER*8 FUNCTION h5_writestepattrib_i8 (filehandle, attrib_name, attrib_value, attrib_nelem)
    INTEGER*8,INTENT(IN) :: filehandle          !< file handle
    CHARACTER(LEN=*), INTENT(IN) :: attrib_name !< name of attribute to write
    INTEGER*8,INTENT(OUT):: attrib_value(*)     !< attribute data to be written
    INTEGER*8, INTENT(IN) :: attrib_nelem       !< number of elements in data array
END FUNCTION

!>
!! \ingroup h5hut_attrib_f
!! See \ref H5ReadStepAttribInt64
!! \return 0 on success or error code
!<
INTEGER*8 FUNCTION h5_readstepattrib_i8 (filehandle, attrib_name, attrib_value)
    INTEGER*8,INTENT(IN) :: filehandle          !< file handle
    CHARACTER(LEN=*), INTENT(IN) :: attrib_name !< name of attribute to read
    INTEGER*8,INTENT(OUT):: attrib_value(*)     !< attribute data will be read into this array
END FUNCTION

!>
!! \ingroup h5hut_attrib_f
!! See \ref H5WriteStepAttribInt32
!! \return 0 on success or error code
!<
INTEGER*8 FUNCTION h5_writestepattrib_i4 (filehandle, attrib_name, attrib_value, attrib_nelem)
    INTEGER*8,INTENT(IN) :: filehandle          !< file handle
    CHARACTER(LEN=*), INTENT(IN) :: attrib_name !< name of attribute to write
    INTEGER*4,INTENT(OUT):: attrib_value(*)     !< attribute data to be written
    INTEGER*8, INTENT(IN) :: attrib_nelem       !< number of elements in data array
END FUNCTION

!>
!! \ingroup h5hut_attrib_f
!! See \ref H5ReadStepAttribInt32
!! \return 0 on success or error code
!<
INTEGER*8 FUNCTION h5_readstepattrib_i4 (filehandle, attrib_name, attrib_value)
    INTEGER*8,INTENT(IN) :: filehandle          !< file handle
    CHARACTER(LEN=*), INTENT(IN) :: attrib_name !< name of attribute to read
    INTEGER*4,INTENT(OUT):: attrib_value(*)     !< attribute data will be read into this array
END FUNCTION
