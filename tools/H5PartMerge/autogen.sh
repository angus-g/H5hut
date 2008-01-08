#! /bin/sh

aclocal
autoconf
automake -a -c
./configure --prefix=$HOME --with-h5part=$HOME --enable-boost
make
#make install

