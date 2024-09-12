LIBS    = $(shell pkg-config --cflags fortran_stdlib) $(shell pkg-config --cflags libfdf) -I./objects/
INCLUDE = $(shell pkg-config --libs fortran_stdlib) $(shell pkg-config --libs libfdf) -I./dependencies/TermTools/library/

OBJ_DIR = objects
MOD_DIR = modules


${OBJ_DIR}/TBModel.o: ${OBJ_DIR}/TBModel@Configuration.o ${OBJ_DIR}/TBModel@PathCalculation.o ${OBJ_DIR}/TBModel@BandCalculation.o
	echo "Uhuuul!"	

${OBJ_DIR}/TBModel@Configuration.o: TBModel@Configuration.f90
	gfortran -c $< -J ${MOD_DIR} -I${MOD_DIR} -o $@

${OBJ_DIR}/TBModel@BandCalculation.o: TBModel@BandCalculation.f90
	gfortran -c $< -J ${MOD_DIR} -I${MOD_DIR} -o $@

${OBJ_DIR}/TBModel@PathCalculation.o: TBModel@PathCalculation.f90
	gfortran -c $< -J ${MOD_DIR} -I${MOD_DIR} -o $@

${MOD_DIR}/TBModel.mod:
	gfortran -fsyntax-only TBModel.f90 -J${MOD_DIR} ${INCLUDE} ${LIBS}
