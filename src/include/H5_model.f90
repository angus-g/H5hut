  INTERFACE
     !>
     !! \ingroup h5hut_model_f
     !! See \ref H5HasStep
     !! \return 0 on success or H5_FAILURE
     !<
     LOGICAL FUNCTION h5_hasstep (filehandle,step)
       INTEGER*8, INTENT(IN) :: filehandle !< the handle returned during file open
       INTEGER*8, INTENT(IN) :: step       !< a timestep value >= 1
     END FUNCTION h5_hasstep

     !>
     !! \ingroup h5hut_model_f
     !! See \ref H5SetStep
     !! \return 0 on success or error code
     !<
     INTEGER*8 FUNCTION h5_setstep (filehandle,step)
       INTEGER*8, INTENT(IN) :: filehandle !< the handle returned during file open
       INTEGER*8, INTENT(IN) :: step       !< a timestep value >= 1
     END FUNCTION h5_setstep

     !>
     !! \ingroup h5hut_model_f
     !! See \ref H5GetStep
     !! \return the the current step or \c H5_FAILURE
     !<
     INTEGER*8 FUNCTION h5_getstep (filehandle)      
       INTEGER*8, INTENT(IN) :: filehandle         !< the handle returned during file open
     END FUNCTION h5_getstep

     !>
     !! \ingroup h5hut_model_f
     !! See \ref H5GetNumSteps
     !! \return the number of steps or error code
     !<
     INTEGER*8 FUNCTION h5_getnsteps (filehandle)      
       INTEGER*8, INTENT(IN) :: filehandle         !< the handle returned during file open
     END FUNCTION h5_getnsteps
  END INTERFACE
