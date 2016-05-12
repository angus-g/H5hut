#TODO: use the rewrite to swap depending whether mpi is there are not..

import numpy

try:
   import H5hut_mpi as H5hut_rewrite
   from H5hut_mpi import *
except:
   import H5hut as H5hut_rewrite
   from H5hut import *


__h5hut_table__ = {}

# for now assume last parameter is the data field..
funcx = """
def {0}(*args, **kwargs): 
    import numpy 
    if type(args[-1]) is numpy.ndarray:
        #print "writing type", args[-1].dtype
        __h5hut_table__['{0}'][args[-1].dtype](*args, **kwargs)
    elif ((type(args[-1]) is list or type(args[-1]) is tuple) and len(args[-1]) > 0):
        #print "writing type", type(args[-1][0]).dtype
        __h5hut_table__['{0}'][numpy.dtype(type(args[-1][0]))](*args, **kwargs)
    else:
        print 'last argument is not a numpy data array'
"""

def __update_types__():
    import numpy
    import sys
    import inspect

    __h5hut_api__ = dir(H5hut_rewrite)

    __h5hut_types__ = [ "Int32", "Int64", "Float32", "Float64" ]
    __numpy_types__ = [ numpy.dtype(numpy.int32), 
                        numpy.dtype(numpy.int64), 
                        numpy.dtype(numpy.float32), 
                        numpy.dtype(numpy.float64) ]

    for __i__ in __h5hut_api__:
        for __j__ in __h5hut_types__:
            if __j__ in __i__:
                key = __i__[:-len(__j__)]

                numpy_key = __numpy_types__[__h5hut_types__.index(__j__)]
                func = H5hut_rewrite.__dict__[__i__]

                if key not in __h5hut_table__:
                    __h5hut_table__[key] = {}

                __h5hut_table__[key][numpy_key] = func

                del H5hut_rewrite.__dict__[__i__]
                del sys.modules["H5hut"].__dict__[__i__]

    for __new_func__ in __h5hut_table__.keys():
        types = __h5hut_table__[__new_func__]
        #args = inspect.getargspec(types[0])
        #print __new_func__
        result = funcx.format(__new_func__) 
        exec(result)

        #H5hut_rewrite.__dict__[__new_func__] = globals()[__new_func__]
        sys.modules["H5hut"].__dict__[__new_func__] = locals()[__new_func__]

__update_types__()
        




