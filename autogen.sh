#!/bin/sh
# Run this to generate all the initial makefiles, etc.

echo "+ making misc files ..."
touch NEWS README AUTHORS ChangeLog
echo
echo
echo
echo "+ running aclocal ..."
aclocal $ACLOCAL_FLAGS || {
  echo
  echo "aclocal failed - check that all needed development files are present on system"
  exit 1
}
echo
echo
echo
echo "+ running autoheader ... "
autoheader || {
  echo
  echo "autoheader failed"
  exit 1
}
echo
echo
echo
echo "+ running libtoolize ... "
libtoolize --force || {
  echo
  echo "libtoolize failed"
  exit 1
}
echo
echo
echo
echo "+ running autoconf ... "
autoconf || {
  echo
  echo "autoconf failed"
  exit 1
}
echo
echo
echo
echo "+ running automake ... "
automake -a -c --foreign || {
  echo
  echo "automake failed"
  exit 1
}
echo
echo
echo

