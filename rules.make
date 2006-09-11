# Makefile Rules (used by both GNUMakefile and Makefile)
# now "all" rule is defined in the main makefile
# all: H5PartTest H5PartTest.o H5Part.o

vtkhdf.o: vtkhdf.cc
	$(CXX) $(CFLAGS) -c vtkhdf.cc

H5PartTest: H5PartTest.o H5Part.o
	$(CXX) -o H5PartTest  H5Part.o H5PartTest.o $(LDFLAGS)

Bench: Bench.c H5Part.o
	$(CC) $(CFLAGS) -o Bench Bench.c H5Part.o $(LDFLAGS)

H5PartTest.o: H5PartTest.cc H5Part.hh
	$(CXX) $(CFLAGS) -DREGRESSIONTEST  -c H5PartTest.cc

H5PartTestParallel.o: H5PartTestParallel.cc H5Part.hh
	$(CXX) $(CFLAGS) -c H5PartTestParallel.cc

H5PartTestParallel: H5PartTestParallel.o H5Part.o
	$(CXX) -o H5PartTestParallel H5Part.o H5PartTestParallel.o $(LDFLAGS)

H5PartAndreasTest.o: H5PartAndreasTest.cc H5Part.hh
	$(CXX) $(CFLAGS) -c H5PartAndreasTest.cc

H5PartAndreasTest: H5PartAndreasTest.o H5Part.o
	$(CXX) -o H5PartAndreasTest H5Part.o H5PartAndreasTest.o $(LDFLAGS)

H5Part.o: H5Part.c H5Part.h
	$(CC) $(CFLAGS) -c H5Part.c

H5PartF.o: H5PartF.c Underscore.h H5Part.h
	$(CC) $(CFLAGS) -w -c H5PartF.c

H5testF.o: H5testF.f H5Part.inc
	$(F90) $(CFLAGS) -c H5testF.f

H5testF: H5testF.o H5Part.o H5PartF.o
	$(F90) $(CFLAGS) -o H5testF H5testF.o H5PartF.o H5Part.o $(LDFLAGS) -lC

vtkxml.o: vtkxml.cc
	$(CXX) $(CFLAGS) -c vtkxml.cc

Underscore.h: TestUnderscore.f TestUnderscoreC.c
	rm -f TestUnderscore.o TestUnderscoreC.o TestUnderscore
	$(CC) -c TestUnderscoreC.c
	$(F90) -c TestUnderscore.f
	$(F90) -o TestUnderscore TestUnderscore.o TestUnderscoreC.o -lC
	./TestUnderscore >& Underscore.h

clean:
	rm -rf *~ *.o H5PartTest
