#!/bin/sh

git submodule update --init --recursive

cd mgcmake; git pull origin master

cd idldoc/lib/mgcmake; git pull origin master
cd idldoc/lib; git pull origin master
cd idldoc; git pull origin master

cd lib/mgcmake; git pull origin master
cd lib; git pull origin master
