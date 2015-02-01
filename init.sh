#!/bin/sh

git submodule update --init --recursive

cd mgcmake; git pull origin master

cd lib/mgcmake; git pull origin master
cd lib; git pull origin master
