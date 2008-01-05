g++ -c -g optparse.cpp
g++ -c -g H5merge.cpp -I$HOME/include -DUSE_BOOST
g++ -g -o H5merge0.1 H5merge.o optparse.o -L$HOME/lib -lH5Part -lhdf5 -lz   -lm 
