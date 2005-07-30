#!/bin/bash
echo "Running aclocal..." && aclocal -I ./m4 $ACLOCAL_FLAGS &&
echo "Running autoheader..." && autoheader &&
echo "Running autoconf..." && autoconf &&
echo "Creating changelog..." && tla changelog >ChangeLog &&
echo "Running automake..." && automake &&
rm -rf autom4te.cache

