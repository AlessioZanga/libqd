#!/bin/bash
PACKAGE_NAME=qd
MAJOR_VERSION=2
MINOR_VERSION=1
PATCH_LEVEL=${1:-`tla logs | sed -n -e '$s/patch-//p'`}
VERSION=$MAJOR_VERSION.$MINOR_VERSION.$PATCH_LEVEL

echo "Creating $PACKAGE_NAME-$VERSION distribution..."

DIR=/var/tmp/$PACKAGE_NAME-$$
ORIG_DIR=`pwd`

mkdir -p $DIR &&
cp -a . $DIR &&
cd $DIR &&
mv configure.ac configure.old &&
sed "/^define(\[QD_PATCH_VERSION\]/s/devel/$PATCH_LEVEL/" configure.old >configure.ac && 
rm -f configure.old &&
config/autogen.sh &&
./configure &&
tla changelog >ChangeLog &&
make distcheck &&
cp $PACKAGE_NAME-$VERSION.tar.gz $ORIG_DIR &&
rm -rf $DIR

