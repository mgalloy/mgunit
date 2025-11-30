#!/bin/sh

PREFIX=$HOME/software/mgunit

rm -rf build
mkdir build
cd build

cmake -DCMAKE_INSTALL_PREFIX=$PREFIX \
      -DIDL_ROOT_DIR:PATH=/opt/share/idl8.6/idl86 \
      -DIDLdoc_DIR=~/projects/idldoc/src \
      ..
