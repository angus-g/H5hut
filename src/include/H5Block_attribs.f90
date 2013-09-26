!
!  Copyright (c) 2006-2013, The Regents of the University of California,
!  through Lawrence Berkeley National Laboratory (subject to receipt of any
!  required approvals from the U.S. Dept. of Energy) and the Paul Scherrer
!  Institut (Switzerland).  All rights reserved.!
!
!  License: see file COPYING in top level of source distribution.
!
  INTERFACE

     !   __ _      _     _         _   _        _ _           _            
     !  / _(_) ___| | __| |   __ _| |_| |_ _ __(_) |__  _   _| |_ ___  ___ 
     ! | |_| |/ _ \ |/ _` |  / _` | __| __| '__| | '_ \| | | | __/ _ \/ __|
     ! |  _| |  __/ | (_| | | (_| | |_| |_| |  | | |_) | |_| | ||  __/\__ \
     ! |_| |_|\___|_|\__,_|  \__,_|\__|\__|_|  |_|_.__/ \__,_|\__\___||___/

     !
     !   __ _ _   _  ___ _ __ _   _ 
     !  / _` | | | |/ _ \ '__| | | |
     ! | (_| | |_| |  __/ |  | |_| |
     !  \__, |\__,_|\___|_|   \__, |
     !     |_|                |___/ 

     !>
     !! \ingroup h5block_attrib_f
     !! See \ref H5BlockGetNumFieldAttribs
     !! \return number of attributes or error code
     !<
     INTEGER*8 FUNCTION h5bl_getnfieldattribs (filehandle, field_name)
       INTEGER*8, INTENT(IN) :: filehandle         !< file handle
       CHARACTER(LEN=*), INTENT(IN) :: field_name  !< name of field
     END FUNCTION h5bl_getnfieldattribs

     !>
     !! \ingroup h5block_attrib_f
     !! See \ref H5BlockGetFieldAttribInfo
     !! \return 0 on success or error code
     !<
     INTEGER*8 FUNCTION h5bl_getfieldattribinfo (filehandle, field_name, idx, attrib_name, attrib_nelems)
       INTEGER*8,INTENT(IN) :: filehandle          !< file handle
       CHARACTER(LEN=*), INTENT(IN) :: field_name  !< name of field
       INTEGER*8,INTENT(IN) :: idx                 !< index of attribute being queried
       CHARACTER(LEN=*), INTENT(OUT):: attrib_name !< name of attribute
       INTEGER*8,INTENT(OUT):: attrib_nelems       !< number of elements in the attrib array
     END FUNCTION h5bl_getfieldattribinfo

     !  _    __          _        _             
     ! (_)  / /__    ___| |_ _ __(_)_ __   __ _ 
     ! | | / / _ \  / __| __| '__| | '_ \ / _` |
     ! | |/ / (_) | \__ \ |_| |  | | | | | (_| |
     ! |_/_/ \___/  |___/\__|_|  |_|_| |_|\__, |
     !                                    |___/

     !>
     !! \ingroup h5block_attrib_f
     !! See \ref H5BlockWriteFieldAttribString
     !! \return 0 on success or error code
     !<
     INTEGER*8 FUNCTION h5bl_writefieldattrib_string (filehandle, field_name, attrib_name, attrib_value, attrib_nelems)
       INTEGER*8, INTENT(IN) :: filehandle         !< file handle
       CHARACTER(LEN=*), INTENT(IN) :: field_name  !< name of field
       CHARACTER(LEN=*), INTENT(IN) :: attrib_name !< name of attribute
       CHARACTER(LEN=*), INTENT(IN) :: attrib_value!< attribute data to be written
       INTEGER*8, INTENT(IN) :: attrib_nelems      !< number of elements in data array
     END FUNCTION h5bl_writefieldattrib_string

     !>
     !! \ingroup h5block_attrib_f
     !! \return 0 on success or error code
     !<
     INTEGER*8 FUNCTION h5bl_readfieldattrib_string (filehandle, field_name, attrib_name, attrib_value)
       INTEGER*8, INTENT(IN) :: filehandle         !< file handle
       CHARACTER(LEN=*), INTENT(IN) :: field_name  !< name of field
       CHARACTER(LEN=*), INTENT(IN) :: attrib_name !< name of attribute
       CHARACTER(LEN=*), INTENT(IN) :: attrib_value!< attribute data will be read into this array
     END FUNCTION h5bl_readfieldattrib_string

     !  _    __                     _ 
     ! (_)  / /__    _ __ ___  __ _| |
     ! | | / / _ \  | '__/ _ \/ _` | |
     ! | |/ / (_) | | | |  __/ (_| | |
     ! |_/_/ \___/  |_|  \___|\__,_|_|

     !>
     !! \ingroup h5block_attrib_f
     !!
     !! See \ref H5BlockWriteFieldAttribFloat64
     !! \return 0 on success or error code
     !<
     INTEGER*8 FUNCTION h5bl_writefieldattrib_r8 (filehandle, field_name, attrib_name, attrib_value, attrib_nelems)
       INTEGER*8,INTENT(IN) :: filehandle          !< file handle
       CHARACTER(LEN=*), INTENT(IN) :: field_name  !< name of field
       CHARACTER(LEN=*), INTENT(IN) :: attrib_name !< name of attribute to write
       REAL*8,   INTENT(OUT):: attrib_value(*)     !< attribute data to be written
       INTEGER*8, INTENT(IN) :: attrib_nelems      !< number of elements in data array
     END FUNCTION h5bl_writefieldattrib_r8

     !>
     !! \ingroup h5block_attrib_f
     !! See \ref H5BlockReadFieldAttribFloat64
     !! \return 0 on success or error code
     !<
     INTEGER*8 FUNCTION h5bl_readfieldattrib_r8 ( filehandle, field_name, attrib_name, attrib_value )
       INTEGER*8,INTENT(IN) :: filehandle          !< file handle
       CHARACTER(LEN=*), INTENT(IN) :: field_name  !< name of field
       CHARACTER(LEN=*), INTENT(IN) :: attrib_name !< name of attribute to read
       REAL*8,   INTENT(OUT):: attrib_value(*)     !< attribute data will be read into this array
     END FUNCTION h5bl_readfieldattrib_r8

     !>
     !! \ingroup h5block_attrib_f
     !! See \ref H5BlockWriteFieldAttribFloat32
     !! \return 0 on success or error code
     !<
     INTEGER*8 FUNCTION h5bl_writefieldattrib_r4 (filehandle, field_name, attrib_name, attrib_value, attrib_nelems)
       INTEGER*8,INTENT(IN) :: filehandle          !< file handle
       CHARACTER(LEN=*), INTENT(IN) :: field_name  !< name of field
       CHARACTER(LEN=*), INTENT(IN) :: attrib_name !< name of attribute to write
       REAL*4,   INTENT(OUT):: attrib_value(*)     !< attribute datato be written
       INTEGER*8, INTENT(IN) :: attrib_nelems      !< number of elements in data array
     END FUNCTION h5bl_writefieldattrib_r4

     !>
     !! \ingroup h5block_attrib_f
     !! See \ref H5BlockReadFieldAttribFloat32
     !! \return 0 on success or error code
     !<
     INTEGER*8 FUNCTION h5bl_readfieldattrib_r4 (filehandle, field_name, attrib_name, attrib_value)
       INTEGER*8,INTENT(IN) :: filehandle          !< file handle
       CHARACTER(LEN=*), INTENT(IN) :: field_name  !< name of field
       CHARACTER(LEN=*), INTENT(IN) :: attrib_name !< name of attribute to read
       REAL*4,   INTENT(OUT):: attrib_value(*)     !< attribute data will be read into this array
     END FUNCTION h5bl_readfieldattrib_r4

     !  _    __      _       _                       
     ! (_)  / /__   (_)_ __ | |_ ___  __ _  ___ _ __ 
     ! | | / / _ \  | | '_ \| __/ _ \/ _` |/ _ \ '__|
     ! | |/ / (_) | | | | | | ||  __/ (_| |  __/ |   
     ! |_/_/ \___/  |_|_| |_|\__\___|\__, |\___|_|   
     !                               |___/

     !>
     !! \ingroup h5block_attrib_f
     !! See \ref H5BlockWriteFieldAttribInt64
     !! \return 0 on success or error code
     !<
     INTEGER*8 FUNCTION h5bl_writefieldattrib_i8 (filehandle, field_name, attrib_name, attrib_value, attrib_nelems)
       INTEGER*8,INTENT(IN) :: filehandle          !< file handle
       CHARACTER(LEN=*), INTENT(IN) :: field_name  !< name of field
       CHARACTER(LEN=*), INTENT(IN) :: attrib_name !< name of attribute to write
       INTEGER*8,INTENT(OUT):: attrib_value(*)     !< attribute data to be written
       INTEGER*8, INTENT(IN) :: attrib_nelems      !< number of elements in data array
     END FUNCTION h5bl_writefieldattrib_i8

     !!>
     !! \ingroup h5block_attrib_f
     !! See \ref H5BlockReadFieldAttribInt64
     !! \return 0 on success or error code
     !<
     INTEGER*8 FUNCTION h5bl_readfieldattrib_i8 (filehandle, field_name, attrib_name, attrib_value)
       INTEGER*8,INTENT(IN) :: filehandle          !< file handle
       CHARACTER(LEN=*), INTENT(IN) :: field_name  !< name of field
       CHARACTER(LEN=*), INTENT(IN) :: attrib_name !< name of attribute to read
       INTEGER*8,INTENT(OUT):: attrib_value(*)     !< attribute data will be read into this array
     END FUNCTION h5bl_readfieldattrib_i8

     !>
     !! \ingroup h5block_attrib_f
     !! See \ref H5BlockWriteFieldAttribInt32
     !! \return 0 on success or error code
     !<
     INTEGER*8 FUNCTION h5bl_writefieldattrib_i4 (filehandle, field_name, attrib_name, attrib_value, attrib_nelems)
       INTEGER*8,INTENT(IN) :: filehandle          !< file handle
       CHARACTER(LEN=*), INTENT(IN) :: field_name  !< name of field
       CHARACTER(LEN=*), INTENT(IN) :: attrib_name !< name of attribute to write
       INTEGER*4,INTENT(OUT):: attrib_value(*)     !< attribute data to be written
       INTEGER*8, INTENT(IN) :: attrib_nelems      !< number of elements in data array
     END FUNCTION h5bl_writefieldattrib_i4

     !>
     !! \ingroup h5block_attrib_f
     !! See \ref H5BlockReadFieldAttribInt32
     !! \return 0 on success or error code
     !<
     INTEGER*8 FUNCTION h5bl_readfieldattrib_i4 (filehandle, field_name, attrib_name, attrib_value)
       INTEGER*8,INTENT(IN) :: filehandle          !< file handle
       CHARACTER(LEN=*), INTENT(IN) :: field_name  !< name of field
       CHARACTER(LEN=*), INTENT(IN) :: attrib_name !< name of attribute to read
       INTEGER*4,INTENT(OUT):: attrib_value(*)     !< attribute data will be read into this array
     END FUNCTION h5bl_readfieldattrib_i4

     !   __ _      _     _                        _             
     !  / _(_) ___| | __| |  ___ _ __   __ _  ___(_)_ __   __ _ 
     ! | |_| |/ _ \ |/ _` | / __| '_ \ / _` |/ __| | '_ \ / _` |
     ! |  _| |  __/ | (_| | \__ \ |_) | (_| | (__| | | | | (_| |
     ! |_| |_|\___|_|\__,_| |___/ .__/ \__,_|\___|_|_| |_|\__, |
     !                          |_|                       |___/ 

     !>
     !! \ingroup h5block_attrib_f
     !! See \ref H5Block3dGetFieldSpacing
     !! \return 0 on success or error code
     !<
     INTEGER*8 FUNCTION h5bl_3d_get_field_spacing (filehandle, name, x, y, z)
       INTEGER*8, INTENT(IN) :: filehandle
       CHARACTER(LEN=*), INTENT(IN) :: name
       REAL*8, INTENT(OUT) :: x
       REAL*8, INTENT(OUT) :: y
       REAL*8, INTENT(OUT) :: z
     END FUNCTION h5bl_3d_get_field_spacing

     !>
     !! \ingroup h5block_attrib_f
     !! See \ref H5Block3dSetFieldSpacing
     !! \return 0 on success or error code
     !<
     INTEGER*8 FUNCTION h5bl_3d_set_field_spacing (filehandle, name, x, y, z)
       INTEGER*8, INTENT(IN) :: filehandle
       CHARACTER(LEN=*), INTENT(IN) :: name
       REAL*8, INTENT(IN) :: x
       REAL*8, INTENT(IN) :: y
       REAL*8, INTENT(IN) :: z
     END FUNCTION h5bl_3d_set_field_spacing

     !   __ _      _     _              _       _       
     !  / _(_) ___| | __| |   ___  _ __(_) __ _(_)_ __  
     ! | |_| |/ _ \ |/ _` |  / _ \| '__| |/ _` | | '_ \ 
     ! |  _| |  __/ | (_| | | (_) | |  | | (_| | | | | |
     ! |_| |_|\___|_|\__,_|  \___/|_|  |_|\__, |_|_| |_|
     !                                    |___/

     !>
     !! \ingroup h5block_attrib_f
     !! See \ref H5Block3dGetFieldOrigin
     !! \return 0 on success or error code
     !<
     INTEGER*8 FUNCTION h5bl_3d_get_field_origin (filehandle, name, x, y, z)
       INTEGER*8, INTENT(IN) :: filehandle
       CHARACTER(LEN=*), INTENT(IN) :: name
       REAL*8, INTENT(OUT) :: x
       REAL*8, INTENT(OUT) :: y
       REAL*8, INTENT(OUT) :: z
     END FUNCTION h5bl_3d_get_field_origin

     !>
     !! \ingroup h5block_attrib_f
     !! See \ref H5Block3dSetFieldOrigin
     !! \return 0 on success or error code
     !<
     INTEGER*8 FUNCTION h5bl_3d_set_field_origin (filehandle, name, x, y, z)
       INTEGER*8, INTENT(IN) :: filehandle
       CHARACTER(LEN=*), INTENT(IN) :: name
       REAL*8, INTENT(IN) :: x
       REAL*8, INTENT(IN) :: y
       REAL*8, INTENT(IN) :: z
     END FUNCTION h5bl_3d_set_field_origin

     !> @}
  END INTERFACE
