libtoolize -f && \
aclocal && \
autoconf && \
autoheader && \
automake --add-missing && \
CC='mpicc' F9X='mpif90' CXX='mpiCC' ./configure --with-rlog=$HOME/extlib/rlog-1.3.7 --with-boost='' --with-hdf5=$HOME/extlib/hdf5-1.6.5 &&\
make clean && \
make all


