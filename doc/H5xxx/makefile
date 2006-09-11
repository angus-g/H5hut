#################################################                             
##BEGIN OF makefile "makefile" - using the gnu-compiler g++                    
##This file is used by the Make command.                                       
###################################################                           

FILE=H5xxx

all:	$(FILE).tex 
	latex $(FILE).tex
#bibtex $(FILE)
	dvips -j0 $(FILE).dvi -o $(FILE).ps 
#	dvips $(FILE).dvi -o $(FILE).ps -x 1200 -O 10mm,15mm
clean:		
	rm *~ *.bbl *.aux *.blg *.log *.dvi *.ps 
