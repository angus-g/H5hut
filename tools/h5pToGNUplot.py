#!/usr/bin/python

import string, sys
sys.path.append('../src/')
import H5Part
from optparse import OptionParser


parser = OptionParser()

print_all=0

parser.add_option("-1", "--1var", dest="var_1",
                  help="(REQUIRED) Takes first variable parameter")

parser.add_option("-2", "--2var", dest="var_2",
                  help="(REQUIRED) Takes second variable parameter")

parser.add_option("-i", "--input", dest="input_file",
                  help="(REQUIRED) Takes input file name")

parser.add_option("-t", "--timestep", dest="timestep", type="int",
                  help="(REQUIRED) Sets the timestep (Value -1 will result in dumping values of all timesteps.)")

parser.add_option("-o", "--output", dest="output_file",
                  help="(OPTIONAL) Takes output file name (without this flag, the program will print to stdout)")

parser.add_option("-n", "--number", dest="nparticle", type="int",
                  help="(OPTIONAL) Sets number of output points")

parser.add_option("-s", "--start", dest="start", type="int",
                  help="(OPTIONAL) Sets the starting particle index")


(options, args) = parser.parse_args()

if options.timestep == -1:
    print_all = 1

MAX_LEN = 100
stop = 0
start_indx = 0

h5file = H5Part.H5PartOpenFile(options.input_file, H5Part.H5PART_READ)

if H5Part.H5PartFileIsValid(h5file)==0 :
    print "unable to open input file"
    parser.print_help()
    sys.exit(1)

j = options.timestep
ntime_step = j+1

if print_all == 1:
    j=0
    ntime_step = H5Part.H5PartGetNumSteps(h5file)

for j in range(j, ntime_step):
    H5Part.H5PartSetStep(h5file,j)
    num_dataset = H5Part.H5PartGetNumDatasets(h5file)
    for i in range(0, num_dataset):
        data_name, data_type, nparticle = H5Part.H5PartGetDatasetInfo(h5file, i, MAX_LEN)
	if data_name == options.var_1:
            type_1=data_type
        if data_name == options.var_2:
            type_2=data_type
# # # # # # # # # # # # # # # #  I MUST FIND A WAY... hid_t & H5T_NATIVE_INT64 etc undefined problem... Currently treated as int...

#temp = H5Part.hid_tArray(2)
#temp[0] = H5T_NATIVE_INT64
#print temp[0]

if type_1 == 201326626: # H5Part.H5T_NATIVE_INT64:
    value_1 = H5Part.longArray(nparticle)
    H5Part.H5PartReadDataInt64(h5file, options.var_1, value_1)
elif type_1 == 201326635: # H5Part.H5T_NATIVE_DOUBLE:
    value_1 = H5Part.doubleArray(nparticle)
    H5Part.H5PartReadDataFloat64(h5file, options.var_1, value_1)
else:
    print "Dataset Type is UNKNOWN for %s. (Check the variable name.)\n" % options.var_1
    sys.exit(1)

if type_2 == 201326626: # H5Part.H5T_NATIVE_INT64:
    value_2 = H5Part.longArray(nparticle)
    H5Part.H5PartReadDataInt64(h5file, options.var_2, value_2)
elif type_2 == 201326635: # H5Part.H5T_NATIVE_DOUBLE:
    value_2 = H5Part.doubleArray(nparticle)
    H5Part.H5PartReadDataFloat64(h5file, options.var_2, value_2)
else:
    print "Dataset Type is UNKNOWN for %s. (Check the variable name.)\n" % options.var_2
    sys.exit(1)


if options.start != None:
    start_indx = options.start

if options.nparticle != None:
    stop = options.nparticle + start_indx
else:
    stop = nparticle

if stop > nparticle:
    stop = nparticle

if options.output_file != None:
    outFile = open(options.output_file,"w")

    for x in range(start_indx, stop):
        if type_1 == 201326626: # H5Part.H5T_NATIVE_INT64:
            outFile.write("%ld" % value_1[x])
        elif type_1 == 201326635: # H5Part.H5T_NATIVE_DOUBLE:
            outFile.write("%lf" % value_1[x])

        if type_2 == 201326626: # H5Part.H5T_NATIVE_INT64:
            outFile.write("\t%ld" % value_2[x])
            outFile.write("\n")
        elif type_2 == 201326635: # H5Part.H5T_NATIVE_DOUBLE:
            outFile.write("\t%lf" % value_2[x])
            outFile.write("\n")

    outFile.write("\n")
    outFile.close()

else:
    for y in range(start_indx, stop):
        if type_1 == 201326626: # H5Part.H5T_NATIVE_INT64:
                print "%ld" % value_1[y],
        elif type_1 == 201326635: # H5Part.H5T_NATIVE_DOUBLE:
                print "%lf" % value_1[y],

        if type_2 == 201326626: # H5Part.H5T_NATIVE_INT64:
                print "\t%ld" % value_2[y]
        elif type_2 == 201326635: # H5Part.H5T_NATIVE_DOUBLE:
                print "\t%lf" % value_2[y]

    print "\n"


H5Part.H5PartCloseFile(h5file)

# if __name__ == "__main__":
#     print "I'M RUNNING AS A MAIN PROGRAM!!!"
