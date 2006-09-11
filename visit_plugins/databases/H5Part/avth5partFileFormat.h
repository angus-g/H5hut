// ************************************************************************* //
//                            avth5partFileFormat.h                           //
// ************************************************************************* //

#ifndef AVT_h5part_FILE_FORMAT_H
#define AVT_h5part_FILE_FORMAT_H

#include <avtMTMDFileFormat.h>
#include <H5Part.hh>

#include <vector>
#include <string>




// ****************************************************************************
//  Class: avth5partFileFormat
//
//  Purpose:
//      Reads in h5part files as a plugin to VisIt.
//
//  Programmer: cristina -- generated by xml2avt
//  Creation:   Mon Feb 27 13:53:31 PST 2006
//
// ****************************************************************************

class avth5partFileFormat : public avtMTMDFileFormat
{
  public:
                       avth5partFileFormat(const char *);
    virtual           ~avth5partFileFormat() {;};

    //
    // This is used to return unconvention data -- ranging from material
    // information to information about block connectivity.
    //
    // virtual void      *GetAuxiliaryData(const char *var, const char *type,
    //                                     int timestep, int domain,void *args, 
    //                                     DestructorFunction &);
    //

    //
    // If you know the times and cycle numbers, overload this function.
    // Otherwise, VisIt will make up some reasonable ones for you.
    //
    // virtual void        GetCycles(std::vector<int> &);
    // virtual void        GetTimes(std::vector<double> &);
    //

    virtual int            GetNTimesteps(void);

    virtual const char    *GetType(void)   { return "h5part"; };
    virtual void           FreeUpResources(void); 

    virtual vtkDataSet    *GetMesh(int, int, const char *);
    virtual vtkDataArray  *GetVar(int, int, const char *);
    virtual vtkDataArray  *GetVectorVar(int, int, const char *);

  protected:
    // DATA MEMBERS

    virtual void           PopulateDatabaseMetaData(avtDatabaseMetaData *, int);
    std::string fname; //filename
    std::vector<float>  points; //point coordinates
    std::vector<std::vector<float> > pointvars; //point variables
    std::vector<std::string> pointvarnames; //point variables' names
    std::vector<float> minExtents; //min extents
    std::vector<float> maxExtents; //max extents


};


#endif
