c ==============
c
c Sample Fortran program that uses HDF5 bindings
c
c ==============

      program H5testF
      implicit none
      include 'H5Part.inc'
      INTEGER*8 file
      INTEGER*8 nstep,ndata
      INTEGER*8 npoints
      INTEGER*8 step
      INTEGER*8 err
      INTEGER*8 I,J
      REAL*8,ALLOCATABLE:: X(:),Y(:),Z(:),PX(:),PY(:),PZ(:)
      INTEGER*8,ALLOCATABLE:: ID(:)
      REAL*8 REALTIME
      file = h5pt_openw("testfilef.h5")
      print *,"Opened file testfilef.h5 for writing"
      npoints = 1024
      nstep = 10
      ALLOCATE(X(npoints),Y(npoints),Z(npoints))
      ALLOCATE(PX(npoints),PY(npoints),PZ(npoints))
      ALLOCATE(ID(npoints))
      print *,"    Npoints=",npoints," nsteps=",nstep
      print *,"    writing X,Y,Z,PX,PY,PZ,ID"
      print *,"    ... initialize the data arrays"
      do I=1,npoints
         X(I)=0.0
         Y(I)=1.0+I
         Z(I)=100.0+I*2.0
         ID(I)=I
      enddo
      print *,"Tell h5pt how many particles are stored in the file"
c     set the number of points
      err = h5pt_setnpoints(file,npoints)
      print *,"write an attribute string"
c     write an annotation to the file
      err=h5pt_writefileattrib_string(file,"Annotation","Testing 1 2 3")
      print *,"and now write the steps"
      do I=1,nstep
c        Set the step number
         print *,"Write Step ",I
         err = h5pt_setstep(file,I)
c        Now start writing the data arrays for this step
         err = h5pt_writedata_r8(file,"x",X)
         err = h5pt_writedata_r8(file,"y",Y)
         err = h5pt_writedata_r8(file,"z",Z)
         err = h5pt_writedata_r8(file,"px",PX)
         err = h5pt_writedata_r8(file,"py",PY)
         err = h5pt_writedata_r8(file,"pz",PZ)
         err = h5pt_writedata_i8(file,"id",ID)
         do J=1,npoints
            ID(J)=ID(J)+10
         enddo
c        And write a simple floatingpoint attribute associated with this timestep
         REALTIME = I*0.1
         err=h5pt_writestepattrib_r8(file,"RealTime",REALTIME,1)
      enddo
      print *,"Done writing, now close the file"
      err = h5pt_close(file)

c **************** Clean out some variables ***************
      nstep=0
c      npoints=0
      do I=1,npoints
         X(I)=-1.0
         Y(I)=-1.0
         Z(I)=-1.0
         ID(I)=0
      enddo

c *****************Now Reopen for Reading ******************


      print *,"Open file for reading"
      file = h5pt_openr("testfilef.h5")
      print *,"  Opened testfilef.h5"
      nstep = h5pt_getnsteps(file)
      print *,"  Nsteps = ",nstep
      err = h5pt_setstep(file,1_8)
      print *,"now get the number of datasets"
      ndata = h5pt_getndatasets(file)
      print *,"  Ndata=",ndata
      npoints = h5pt_getnpoints(file)
      print *,"  NP=",npoints


      
      do step=1,nstep
         print *,"Read step ",step
c        set the current step
         err = h5pt_setstep(file,step)
         err=h5pt_readdata_i8(file,"id",ID)
c        read the Z data from the current step
c         err = h5prt_readdata(file,step,X,Y,Z,PX,PY,PZ,ID)
         do J=1,10
            print *,"    ID(",J,")==",ID(J)
         enddo
      enddo
      
      err = h5pt_close(file)
      print *,"done"
      
      end

