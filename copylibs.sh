#!/usr/bin/env sh
#
# Copy shared libraries to ./lib 
#

ldd zhserver.bin | awk -F'=>' '{print $2}' | awk -F'(' '{print $1}' | egrep --invert-match  "libc.so|libpthread|libm|libresolv" | xargs cp -v -t ./lib
rm -rf lib/librt*
