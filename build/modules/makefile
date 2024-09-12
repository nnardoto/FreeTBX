INCLUDE = $(shell pkg-config --cflags fortran_stdlib) $(shell pkg-config --cflags libfdf) -I../dependencies/TermTools/library/ 
SOURCE  = ../source/

Finish: TBModel.mod TBModel_Configuration.mod TBModel_BandCalculation.mod TBModel_PathCalculation.mod

TBModel.mod: ${SOURCE}/TBModel.f90
	gfortran -fsyntax-only $< ${INCLUDE}

TBModel_Configuration.mod: ${SOURCE}/TBModel_Configuration.f90 TBModel.mod
	gfortran -fsyntax-only $< ${INCLUDE} 

TBModel_BandCalculation.mod: ${SOURCE}/TBModel_BandCalculation.f90 TBModel.mod
	gfortran -fsyntax-only $< ${INCLUDE} 

TBModel_PathCalculation.mod:  ${SOURCE}/TBModel_PathCalculation.f90
	gfortran -fsyntax-only $< ${INCLUDE} 




