!!!!!!!! Setting up the Data Model !!!!!!!!

!> \ingroup h5part_model_f
!! See \ref H5PartSetNumParticles
!! \return 0 on success or error code
!<
INTEGER*8 FUNCTION h5pt_setnpoints ( filehandle, npoints )
    INTEGER*8, INTENT(IN) :: filehandle !< the handle returned during file open
    INTEGER*8, INTENT(IN) :: npoints    !< the number of particles on *this* processor
END FUNCTION

!> \ingroup h5part_model_f
!! See \ref H5PartSetNumParticlesStrided
!! \return 0 on success or error code
!<
INTEGER*8 FUNCTION h5pt_setnpoints_strided ( filehandle, npoints, stride )
    INTEGER*8, INTENT(IN) :: filehandle !< the handle returned during file open
    INTEGER*8, INTENT(IN) :: npoints    !< the number of particles on *this* processor
    INTEGER*8, INTENT(IN) :: stride     !< the stride value (e.g. the number of fields in the particle data array)
END FUNCTION

!> \ingroup h5part_model_f
!! See \ref H5PartGetNumDatasets
!! \return the number of datasets or error code
!<
INTEGER*8 FUNCTION h5pt_getndatasets (filehandle)
    INTEGER*8, INTENT(IN) :: filehandle         !< the handle returned during file open
END FUNCTION

!> \ingroup h5part_model_f
!! See \ref H5PartGetNumParticles
!! \return the number of particles or error code
!<
INTEGER*8 FUNCTION h5pt_getnpoints (filehandle)
    INTEGER*8, INTENT(IN) :: filehandle         !< the handle returned during file open
END FUNCTION

!> \ingroup h5part_model_f
!! See \ref H5PartGetDatasetName
!! \return 0 on success or error code
!<
INTEGER*8 FUNCTION h5pt_getdatasetname (filehandle,index,name)
    INTEGER*8, INTENT(IN) :: filehandle         !< the handle returned during file open
    INTEGER*8, INTENT(IN) :: index              !< index of dataset to query (starting from 0)
    CHARACTER(LEN=*), INTENT(OUT) :: name       !< buffer to read the dataset name into 
END FUNCTION

!> \ingroup h5part_model_f
!! See \ref H5PartSetView
!! \return 0 on success or error code
!<
INTEGER*8 FUNCTION h5pt_setview (filehandle,start,end)
    INTEGER*8, INTENT(IN) :: filehandle !< the handle returned during file open
    INTEGER*8, INTENT(IN) :: start      !< offset of the first particle in the view
    INTEGER*8, INTENT(IN) :: end        !< offset of the last particle in the view (inclusive)
END FUNCTION

!> \ingroup h5part_model_f
!! See \ref H5PartSetViewIndices
!! \return 0 on success or error code
!<
INTEGER*8 FUNCTION h5pt_setview_indices (filehandle,indices,nelem)
    INTEGER*8, INTENT(IN) :: filehandle !< the handle returned during file open
    INTEGER*8, INTENT(IN) :: indices(*) !< list of indicies to select in this view
    INTEGER*8, INTENT(IN) :: nelem      !< number of particles in the list
END FUNCTION

!> \ingroup h5part_model_f
!! See \ref H5PartResetView
!! \return 0 on success or error code
!<
INTEGER*8 FUNCTION h5pt_resetview (filehandle)
    INTEGER*8, INTENT(IN) :: filehandle !< the handle returned during file open
END FUNCTION

!> \ingroup h5part_model_f
!! See \ref H5PartResetView
!! \return 1 if true, 0 if false, or error code
!<
INTEGER*8 FUNCTION h5pt_hasview (filehandle)
    INTEGER*8, INTENT(IN) :: filehandle !< the handle returned during file open
END FUNCTION

!> \ingroup h5part_model_f
!! See \ref H5PartGetView
!! \return 0 on success or error code
!<
INTEGER*8 FUNCTION h5pt_getview (filehandle,start,end)
    INTEGER*8, INTENT(IN) :: filehandle !< the handle returned during file open
    INTEGER*8, INTENT(OUT) :: start     !< buffer to store the offset of the first particle in the view
    INTEGER*8, INTENT(OUT) :: end       !< buffer to store the offset of the last particle in the view (inclusive)
END FUNCTION


!!!!!!!! Reading and Writing Datasets !!!!!!!!

!> \ingroup h5part_data_f
!! See \ref H5PartWriteDataFloat64
!! \return 0 on success or error code
!<
INTEGER*8 FUNCTION h5pt_writedata_r8 ( filehandle, name, data )
    INTEGER*8, INTENT(IN) :: filehandle         !< the handle returned during file open
    CHARACTER(LEN=*), INTENT(IN) :: name        !< the name of the dataset
    REAL*8, INTENT(IN) :: data(*)               !< the array of float64 data to write
END FUNCTION

!> \ingroup h5part_data_f
!! See \ref H5PartWriteDataFloat32
!! \return 0 on success or error code
!<
INTEGER*8 FUNCTION h5pt_writedata_r4 ( filehandle, name, data )
    INTEGER*8, INTENT(IN) :: filehandle         !< the handle returned during file open
    CHARACTER(LEN=*), INTENT(IN) :: name        !< the name of the dataset
    REAL, INTENT(IN) :: data(*)                 !< the array of float32 data to write
END FUNCTION

!> \ingroup h5part_data_f
!! See \ref H5PartWriteDataInt64
!! \return 0 on success or error code
!<
INTEGER*8 FUNCTION h5pt_writedata_i8 ( filehandle, name, data )
    INTEGER*8, INTENT(IN) :: filehandle         !< the handle returned during file open
    CHARACTER(LEN=*), INTENT(IN) :: name        !< the name of the dataset
    INTEGER*8, INTENT(IN) :: data(*)            !< the array of int64 data to write
END FUNCTION

!> \ingroup h5part_data_f
!! See \ref H5PartWriteDataInt32
!! \return 0 on success or error code
!<
INTEGER*8 FUNCTION h5pt_writedata_i4 ( filehandle, name, data )
    INTEGER*8, INTENT(IN) :: filehandle         !< the handle returned during file open
    CHARACTER(LEN=*), INTENT(IN) :: name        !< the name of the dataset
    INTEGER, INTENT(IN) :: data(*)              !< the array of int32 data to write
END FUNCTION


!> \ingroup h5part_data_f
!! See \ref H5PartReadDataFloat64
!! \return 0 on success or error code
!<
INTEGER*8 FUNCTION h5pt_readdata_r8 (filehandle,name,data)
    INTEGER*8, INTENT(IN) :: filehandle         !< the handle returned during file open
    CHARACTER(LEN=*), INTENT(IN) :: name        !< the name of the dataset
    REAL*8, INTENT(OUT) :: data(*)              !< array to read float64 data into
END FUNCTION

!> \ingroup h5part_data_f
!! See \ref H5PartReadDataFloat32
!! \return 0 on success or error code
!<
INTEGER*8 FUNCTION h5pt_readdata_r4 (filehandle,name,data)
    INTEGER*8, INTENT(IN) :: filehandle         !< the handle returned during file open
    CHARACTER(LEN=*), INTENT(IN) :: name        !< the name of the dataset
    REAL, INTENT(OUT) :: data(*)                !< array to read float32 data into
END FUNCTION

!> \ingroup h5part_data_f
!! See \ref H5PartReadDataInt64
!! \return 0 on success or error code
!<
INTEGER*8 FUNCTION h5pt_readdata_i8 (filehandle,name,data)
    INTEGER*8, INTENT(IN) :: filehandle         !< the handle returned during file open
    CHARACTER(LEN=*), INTENT(IN) :: name        !< the name of the dataset
    INTEGER*8, INTENT(OUT) :: data(*)           !< array to read int64 data into
END FUNCTION

!> \ingroup h5part_data_f
!! See \ref H5PartReadDataInt32
!! \return 0 on success or error code
!<
INTEGER*8 FUNCTION h5pt_readdata_i4 (filehandle,name,data)
    INTEGER*8, INTENT(IN) :: filehandle         !< the handle returned during file open
    CHARACTER(LEN=*), INTENT(IN) :: name        !< the name of the dataset
    INTEGER, INTENT(OUT) :: data(*)             !< array to read int32 data into
END FUNCTION

