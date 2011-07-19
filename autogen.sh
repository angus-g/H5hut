#!/bin/sh
# Run this to generate all the initial makefiles, etc.

LIBTOOLIZE=`which libtoolize`
if [ "$LIBTOOLIZE" = "" ]; then
	LIBTOOLIZE=`which glibtoolize`
fi
if [ "$LIBTOOLIZE" = "" ]; then
	echo "libtoolize not found" 1>&2
	exit 1
fi

echo "+ making misc files ..."
touch NEWS README AUTHORS ChangeLog
echo
echo "+ running aclocal ..."
aclocal $ACLOCAL_FLAGS || {
  echo
  echo "aclocal failed - check that all needed development files are present on system"
  exit 1
}
echo
echo "+ running autoheader ... "
autoheader || {
  echo
  echo "autoheader failed"
  exit 1
}
echo
echo "+ running libtoolize ... "
$LIBTOOLIZE --force || {
  echo
  echo "libtoolize failed"
  exit 1
}
echo
echo "+ running autoconf ... "
autoconf || {
  echo
  echo "autoconf failed"
  exit 1
}
echo
echo "+ running automake ... "
automake -a -c --foreign || {
  echo
  echo "automake failed"
  exit 1
}
