#!/bin/bash

brew install gcc5

pip install xgboost

brew install gcc

git clone --recursive https://github.com/dmlc/xgboost

cd xgboost; cp make/config.mk ./config.mk; make -j4

