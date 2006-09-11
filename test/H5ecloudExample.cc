#ifdef GTHDF5
template<class T,unsigned int Dim> void DataSink<T,Dim>::savePhaseSpaceData()
{
   T *x=(double*)malloc(beam_m->getLocalNum()*sizeof(double));
   T *y=(double*)malloc(beam_m->getLocalNum()*sizeof(double));
   T *z=(double*)malloc(beam_m->getLocalNum()*sizeof(double));
   T *px=(double*)malloc(beam_m->getLocalNum()*sizeof(double));
   T *py=(double*)malloc(beam_m->getLocalNum()*sizeof(double));
   T *pz=(double*)malloc(beam_m->getLocalNum()*sizeof(double));
   h5part_int64_t int *id=(h5part_int64_t int*)malloc(beam_m->getLocalNum()*sizeof(h5part_int64_t));

   double actPos           = beam_m->getActSPos();
   double structLenght     = beam_m->getMaxZ();
   Vector_t org            = beam_m->getOrigin();
   Vector_t maxX           = beam_m->getRmax();
   Vector_t minX           = beam_m->getRmin();
   Vector_t maxP           = beam_m->getPmax();
   Vector_t minP           = beam_m->getPmin();
   unsigned long protons   = beam_m->getNumberOfProtons();
   unsigned long electrons = beam_m->getNumberOfElectrons();
   Vector_t centroid       = beam_m->getCentroid();
   unsigned nTot           = beam_m->getTotalNum();
     for (h5part_int64_t i=0; i<beam_m->getLocalNum();i++) {
       x[i] =  beam_m->R[i](0);
       y[i] =  beam_m->R[i](1);
       z[i] =  beam_m->R[i](2);
       px[i] =  beam_m->P[i](0);
       py[i] =  beam_m->P[i](1);
       pz[i] =  beam_m->P[i](2);
       if (i< (electrons-1))
           id[i] =  beam_m->ID[i];            else
           id[i] =  -1*(long int)beam_m->ID[i];
   }
     H5PartSetStep(file_m,idx_m);  /* must set the current timestep in file */
   H5PartSetNumParticles(file_m,beam_m->getLocalNum()); /* then set number of particles to store */
     /* now write different tuples of data into this timestep of the file */
   H5PartWriteDataFloat64(file_m,"x",x);
   H5PartWriteDataFloat64(file_m,"y",y);
   H5PartWriteDataFloat64(file_m,"z",z);
   H5PartWriteDataFloat64(file_m,"px",px);
   H5PartWriteDataFloat64(file_m,"py",py);
   H5PartWriteDataFloat64(file_m,"pz",pz);
   H5PartWriteDataInt64(file_m,"id",id);
   h5part_int64_t step = idx_m;
   H5PartWriteStepAttrib(file_m,"Step",H5T_NATIVE_INT64,&step,1);


   /* write scalar data i.e the header */
   H5PartWriteAttrib(file_m,"Spos",H5T_NATIVE_DOUBLE,&actPos,1);
   H5PartWriteAttrib(file_m,"structLen",H5T_NATIVE_DOUBLE,&structLenght,1);

   H5PartWriteAttrib(file_m,"org",H5T_NATIVE_DOUBLE,&org,3);
   H5PartWriteAttrib(file_m,"maxX",H5T_NATIVE_DOUBLE,&maxX,3);
   H5PartWriteAttrib(file_m,"minX",H5T_NATIVE_DOUBLE,&minX,3);
   H5PartWriteAttrib(file_m,"maxP",H5T_NATIVE_DOUBLE,&maxP,3);
   H5PartWriteAttrib(file_m,"minP",H5T_NATIVE_DOUBLE,&minP,3);
   H5PartWriteAttrib(file_m,"centroid",H5T_NATIVE_DOUBLE,&centroid,3);
   delete x;
   delete y;
   delete z;
   delete id;
   idx_m++;
}
