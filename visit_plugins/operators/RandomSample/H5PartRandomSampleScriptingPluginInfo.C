// ************************************************************************* //
//                        H5PartRandomSampleScriptingPluginInfo.C
// ************************************************************************* //
#include <H5PartRandomSamplePluginInfo.h>
#include <PyH5PartRandomSampleAttributes.h>

#if defined(__APPLE__)
#define GetScriptingInfo H5PartRandomSample_GetScriptingInfo
#endif

// ****************************************************************************
//  Function:  GetScriptingInfo
//
//  Purpose:
//    Return a new ScriptingPluginInfo for the H5PartRandomSample plot.
//
//  Programmer: cristina -- generated by xml2info
//  Creation:   Thu Mar 16 10:26:55 PDT 2006
//
// ****************************************************************************
extern "C" ScriptingOperatorPluginInfo* GetScriptingInfo()
{
    return new H5PartRandomSampleScriptingPluginInfo;
}

// ****************************************************************************
// Method: H5PartRandomSampleScriptingPluginInfo::InitializePlugin
//
// Purpose: 
//   Calls the initialization function for the plugin.
//
// Arguments:
//   subj    : A pointer to the plugin's state object.
//   data    : A pointer to data to be used by the observer function.
//
//  Programmer: cristina -- generated by xml2info
//  Creation:   Thu Mar 16 10:26:55 PDT 2006
//
// Modifications:
//   
// ****************************************************************************

void
H5PartRandomSampleScriptingPluginInfo::InitializePlugin(AttributeSubject *subj,
    void *data)
{
    PyH5PartRandomSampleAttributes_StartUp((H5PartRandomSampleAttributes *)subj, data);
}

// ****************************************************************************
// Method: H5PartRandomSampleScriptingPluginInfo::GetMethodTable
//
// Purpose: 
//   Returns a pointer to the plugin's Python method table. These methods are
//   added to the top-level visit module's methods.
//
// Arguments:
//   nMethods : Returns the number of methods in the method table.
//
// Returns:    A pointer to the method table.
//
//  Programmer: cristina -- generated by xml2info
//  Creation:   Thu Mar 16 10:26:55 PDT 2006
//
// Modifications:
//   
// ****************************************************************************

void *
H5PartRandomSampleScriptingPluginInfo::GetMethodTable(int *nMethods)
{
    return PyH5PartRandomSampleAttributes_GetMethodTable(nMethods);
}

// ****************************************************************************
// Method: H5PartRandomSampleScriptingPluginInfo::TypesMatch
//
// Purpose: 
//   Returns whether or not the input PyObject is H5PartRandomSample plot attributes.
//
// Arguments:
//   pyobject : A PyObject cast to void*.
//
//  Programmer: cristina -- generated by xml2info
//  Creation:   Thu Mar 16 10:26:55 PDT 2006
//
// Modifications:
//   
// ****************************************************************************

bool
H5PartRandomSampleScriptingPluginInfo::TypesMatch(void *pyobject)
{
    return PyH5PartRandomSampleAttributes_Check((PyObject *)pyobject);
}

// ****************************************************************************
// Method: H5PartRandomSampleScriptingPluginInfo::GetLogString
//
// Purpose: 
//   Gets a string representation of the current attributes.
//
// Arguments:
//   val : Whether or not to log state information.
//
//  Programmer: cristina -- generated by xml2info
//  Creation:   Thu Mar 16 10:26:55 PDT 2006
//
// Modifications:
//   
// ****************************************************************************

char *
H5PartRandomSampleScriptingPluginInfo::GetLogString()
{
    std::string s(PyH5PartRandomSampleAttributes_GetLogString());
    char *v = new char[s.size() + 1];
    strcpy(v, s.c_str());
    return v;
}

// ****************************************************************************
// Method: H5PartRandomSampleScriptingPluginInfo::SetDefaults
//
// Purpose: 
//   Used to set the default values for a plugin's state object.
//
// Arguments:
//   atts : The new state.
//
//  Programmer: cristina -- generated by xml2info
//  Creation:   Thu Mar 16 10:26:55 PDT 2006
//
// Modifications:
//   
// ****************************************************************************

void
H5PartRandomSampleScriptingPluginInfo::SetDefaults(const AttributeSubject *atts)
{
    PyH5PartRandomSampleAttributes_SetDefaults((const H5PartRandomSampleAttributes *)atts);
}
