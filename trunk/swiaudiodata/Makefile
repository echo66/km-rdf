# Makefile for 
# David Pastor Escuredo 2007

CC = gcc # C compile
CPP = g++ # C++ compiler

CFLAGS = -Wall -fPIC

#######################################################################
# Define your platform-specific options here
#######################################################################

swi_dir = /usr/local/lib/pl-5.6.34

swiaudiodata_dir = /home/david/km-rdf/swiaudiodata
swilib_dir = /home/david/km-rdf/swilib

lib_suf = .so
obj_suf = .o
#######################################################################

#Headers
INCLUDE= 		-I$(swi_dir)/include/ \
			-I/usr/include/qt4/Qt/ \
			-I/usr/include/qt4/ \
			-I$(swilib_dir)/
			
#Libraries

LIB = $(swilib_dir)/swimo.so \
	$(swilib_dir)/swiaudioblob.so

#Sources

SWI_MOPL = mopl.cpp
SWI_FRAME = framepl.cpp

#Objects

OBJ_MOPL = mopl.o
OBJ_FRAME = framepl.o

#DLIB

AUDIODATA = swiaudiodata.so

#Targets
all : $(AUDIODATA)

$(OBJ_MOPL) :
	$(CPP) -c $(CFLAGS) $(INCLUDE) $(SWI_MOPL) -o $(OBJ_MOPL)

$(OBJ_FRAME) :
	$(CPP) -c $(CFLAGS) $(INCLUDE) $(SWI_FRAME) -o $(OBJ_FRAME)

$(AUDIODATA) : $(OBJ_FRAME) $(OBJ_MOPL) Makefile
	$(CPP) -shared $(LIB) $(OBJ_MOPL) $(OBJ_FRAME) -o $(AUDIODATA) 

clean :
	rm *.o *.so



