LIBS    = $(shell pkg-config --libs   fortran_stdlib) $(shell pkg-config --libs   libfdf) 
INCLUDE = $(shell pkg-config --cflags fortran_stdlib) $(shell pkg-config --cflags libfdf) 

OBJ_DIR = ./objects
MOD_DIR = ./modules
SRC_DIR = ./source


FreeTBX.x: ${SRC_DIR}/FreeTBX.f90
	make -C dependencies/TermTools
	make -C modules all
	make -C objects all
	gfortran $< ${OBJ_DIR}/*.o -I${MOD_DIR} -o $@ ${INCLUDE} ${LIBS} -fopenmp 

clean:
	make -C ./objects clean
	make -C ./modules clean
