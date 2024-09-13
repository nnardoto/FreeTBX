LIBS    = $(shell pkg-config --libs fortran_stdlib) $(shell pkg-config --libs libfdf) 
INCLUDE = $(shell pkg-config --cflags fortran_stdlib) $(shell pkg-config --cflags libfdf)  

MOD_DIR = ../modules
SRC_DIR = ../source/

TO_BUILD = TBModel_Configuration.o TBModel_PathCalculation.o TBModel_BandCalculation.o TBModel.o 


all: ${TO_BUILD} 

TBModel.o: ${SRC_DIR}/TBModel.f90 
	gfortran -c $< -J ${MOD_DIR} -I${MOD_DIR} -o $@ ${INCLUDE}	

TBModel_Configuration.o: ${SRC_DIR}/TBModel_Configuration.f90
	gfortran -c $< -J ${MOD_DIR} -I${MOD_DIR} -o $@ ${INCLUDE}

TBModel_BandCalculation.o: ${SRC_DIR}/TBModel_BandCalculation.f90
	gfortran -c $< -J ${MOD_DIR} -I${MOD_DIR} -o $@ ${INCLUDE}

TBModel_PathCalculation.o: ${SRC_DIR}/TBModel_PathCalculation.f90
	gfortran -c $< -J ${MOD_DIR} -I${MOD_DIR} -o $@ ${INCLUDE}

clean:
	rm ${TO_BUILD}
