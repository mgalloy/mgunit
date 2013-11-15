#!/bin/sh

PREFIX=$HOME/software/mgunit

rm -rf build
mkdir build
cd build

cmake -DCMAKE_INSTALL_PREFIX=$PREFIX \
      -DIDLdoc_DIR=~/projects/idldoc/src \
      ..
