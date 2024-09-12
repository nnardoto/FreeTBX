#!/bin/bash

#=========================================================
# fdf library download
#=========================================================
#git clone --depth=1 https://gitlab.com/nnardoto/libfdf
#cd libfdf
#make
##=========================================================
#
#cd ..

#=========================================================
# termtools library download
#=========================================================
git clone --depth=1 https://github.com/nnardoto/TermTools
cd TermTools
make
cp library/*.mod ../../modules/
cp library/*.o   ../../objects/

