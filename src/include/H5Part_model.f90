!
!  Copyright (c) 2006-2013, The Regents of the University of California,
!  through Lawrence Berkeley National Laboratory (subject to receipt of any
!  required approvals from the U.S. Dept. of Energy) and the Paul Scherrer
!  Institut (Switzerland).  All rights reserved.!
!
!  License: see file COPYING in top level of source distribution.
!
  INTERFACE
     !>
     !! \ingroup h5part_model_f
     !!
     !! See \ref H5PartSetNumParticles
     !! \return 0 on success or error code
     !<
     INTEGER*8 FUNCTION h5pt_setnpoints ( filehandle, npoints )
       INTEGER*8, INTENT(IN) :: filehandle !< the handle returned during file open
       INTEGER*8, INTENT(IN) :: npoints    !< the number of particles on *this* processor
     END FUNCTION h5pt_setnpoints

     !>
     !! \ingroup h5part_model_f
     !!
     !! See \ref H5PartSetNumParticlesStrided
     !! \return 0 on success or error code
     !<
     INTEGER*8 FUNCTION h5pt_setnpoints_strided ( filehandle, npoints, stride )
       INTEGER*8, INTENT(IN) :: filehandle !< the handle returned during file open
       INTEGER*8, INTENT(IN) :: npoints    !< the number of particles on *this* processor
       INTEGER*8, INTENT(IN) :: stride     !< the stride value (e.g. the number of fields in the particle data array)
     END FUNCTION h5pt_setnpoints_strided


     !>
     !! \ingroup h5part_model_f
     !!
     !! See \ref H5PartSetView
     !! \return 0 on success or error code
     !<
     INTEGER*8 FUNCTION h5pt_setview (filehandle,start,end)
       INTEGER*8, INTENT(IN) :: filehandle !< the handle returned during file open
       INTEGER*8, INTENT(IN) :: start      !< offset of the first particle in the view
       INTEGER*8, INTENT(IN) :: end        !< offset of the last particle in the view (inclusive)
     END FUNCTION h5pt_setview

     !>
     !! \ingroup h5part_model_f
     !!
     !! See \ref H5PartSetViewIndices
     !! \return 0 on success or error code
     !<
     INTEGER*8 FUNCTION h5pt_setview_indices (filehandle,indices,nelem)
       INTEGER*8, INTENT(IN) :: filehandle !< the handle returned during file open
       INTEGER*8, INTENT(IN) :: indices(*) !< list of indicies to select in this view
       INTEGER*8, INTENT(IN) :: nelem      !< number of particles in the list
     END FUNCTION h5pt_setview_indices

     !>
     !! \ingroup h5part_model_f
     !!
     !! See \ref H5PartSetCanonicalView
     !! \return 0 on success or error code
     !<
     INTEGER*8 FUNCTION h5pt_setcanonicalview (filehandle)
       INTEGER*8, INTENT(IN) :: filehandle !< the handle returned during file open
     END FUNCTION h5pt_setcanonicalview

     !>
     !! \ingroup h5part_model_f
     !!
     !! See \ref H5PartResetView
     !! \return 0 on success or error code
     !<
     INTEGER*8 FUNCTION h5pt_resetview (filehandle)
       INTEGER*8, INTENT(IN) :: filehandle !< the handle returned during file open
     END FUNCTION h5pt_resetview

     !>
     !! \ingroup h5part_model_f
     !!
     !! See \ref H5PartResetView
     !! \return 1 if true, 0 if false, or error code
     !<
     INTEGER*8 FUNCTION h5pt_hasview (filehandle)
       INTEGER*8, INTENT(IN) :: filehandle !< the handle returned during file open
     END FUNCTION h5pt_hasview

     !>
     !! \ingroup h5part_model_f
     !!
     !! See \ref H5PartGetView
     !! \return 0 on success or error code
     !<
     INTEGER*8 FUNCTION h5pt_getview (filehandle,start,end)
       INTEGER*8, INTENT(IN) :: filehandle !< the handle returned during file open
       INTEGER*8, INTENT(OUT) :: start     !< buffer to store the offset of the first particle in the view
       INTEGER*8, INTENT(OUT) :: end       !< buffer to store the offset of the last particle in the view (inclusive)
     END FUNCTION h5pt_getview

     !>
     !! \ingroup h5part_model_f
     !!
     !! See \ref H5PartGetNumDatasets
     !! \return the number of datasets or error code
     !<
     INTEGER*8 FUNCTION h5pt_getndatasets (filehandle)
       INTEGER*8, INTENT(IN) :: filehandle         !< the handle returned during file open
     END FUNCTION h5pt_getndatasets

     !>
     !! \ingroup h5part_model_f
     !!
     !! See \ref H5PartGetNumParticles
     !! \return the number of particles or error code
     !<
     INTEGER*8 FUNCTION h5pt_getnpoints (filehandle)
       INTEGER*8, INTENT(IN) :: filehandle         !< the handle returned during file open
     END FUNCTION h5pt_getnpoints

     !>
     !! \ingroup h5part_model_f
     !!
     !! See \ref H5PartGetDatasetName
     !! \return 0 on success or error code
     !<
     INTEGER*8 FUNCTION h5pt_getdatasetname (filehandle,index,name)
       INTEGER*8, INTENT(IN) :: filehandle         !< the handle returned during file open
       INTEGER*8, INTENT(IN) :: index              !< index of dataset to query (starting from 0)
       CHARACTER(LEN=*), INTENT(OUT) :: name       !< buffer to read the dataset name into 
     END FUNCTION h5pt_getdatasetname

     !>
     !! \ingroup h5part_model_f
     !!
     !! See \ref H5PartGetDatasetInfo
     !! \return 0 on success or error code
     !<
     INTEGER*8 FUNCTION h5pt_getdatasetinfo (filehandle, idx, name, type, num_elems)
       INTEGER*8,INTENT(IN) :: filehandle          !< file handle
       INTEGER*8,INTENT(IN) :: idx                 !< index of dataset being queried
       CHARACTER(LEN=*), INTENT(OUT):: name        !< name of datset
       INTEGER*8,INTENT(OUT):: type                !< type of datset
       INTEGER*8,INTENT(OUT):: num_elems           !< number of elements in the dataset
     END FUNCTION h5pt_getdatasetinfo

     !>
     !! \ingroup h5part_model_f
     !!
     !! See \ref H5PartSetChunkSize
     !! \return 0 on success or -2 on error
     !<
     INTEGER*8 FUNCTION h5pt_getnpoints (filehandle)
       INTEGER*8, INTENT(IN) :: filehandle         !< the handle returned during file open
     END FUNCTION h5pt_getnpoints

  END INTERFACE
