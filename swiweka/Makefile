# Makefile for 
# David Pastor Escuredo 2007

CC = gcc # C compile
CPP = g++ # C++ compiler

CFLAGS = -Wall -fPIC

#######################################################################
# Define your platform-specific options here
#######################################################################

swi_dir = /usr/local/lib/pl-5.6.34

swiweka_dir = /home/david/km-rdf/swiweka

lib_suf = .so
obj_suf = .o
#######################################################################

#Headers
INCLUDE= 		-I$(swi_dir)/include/ \
			-I/usr/include/qt4/Qt/ \
			-I/usr/include/qt4/ 

			
#Library

LIB = /usr/lib/libQtCore.so 

#Sources
SWI_WEKA = swiwekadb.cpp

#Objects

OBJ_WEKA = swiweka.o

#DLIB

WEKA = swiweka.so

#Targets
all : $(WEKA)

$(OBJ_WEKA) :
	$(CPP) -c $(CFLAGS) $(INCLUDE) $(SWI_WEKA) -o $(OBJ_WEKA)

$(WEKA): $(OBJ_WEKA) Makefile
	$(CPP) -shared $(LIB) $(OBJ_WEKA) -o $(WEKA)

clean :
	rm *.o *.so



