#!/bin/bash

brew install wget --with-libressl


BASEDIR=$(dirname "$0")
cd $BASEDIR

DATASETS_DIR="data"
mkdir -p $DATASETS_DIR

cd $DATASETS_DIR

# Get data set
wget https://www.dropbox.com/s/aecihu4d566su4q/data.zip

unzip data.zip
rm data.zip

