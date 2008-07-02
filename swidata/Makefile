# Makefile for swidata
# David Pastor Escuredo 2008

CC = gcc # C compile
CPP = g++ # C++ compiler

CFLAGS = -Wall -fPIC

#############################################################
# Define your platform-specific options here
#############################################################

swi_dir = /usr/local/lib/pl-5.6.50

swiaudiodata_dir = .
swilib_dir = ../swilib

lib_suf = .so
obj_suf = .o
#######################################################################

#Headers
INCLUDE= 		-I$(swi_dir)/include/ \
			-I/usr/include/qt4/Qt/ \
			-I/usr/include/qt4/ \
			-I$(swilib_dir)/
			
#Libraries

LIB = 	$(swilib_dir)/swiaudioblob.so \
	$(swilib_dir)/blobid.so

#Sources

SWI_ABPL = audioblobpl.cpp
SWI_BID = blobidpl.cpp
SWI_BIDOP = blobidOperations.cpp

#Objects

OBJ_ABPL = abpl.o
OBJ_BID = blobid.o
OBJ_BIDO = blobidop.o

#DLIB

AUDIODATA = swidata.so

#Targets
all : $(AUDIODATA)

$(OBJ_ABPL) :
	$(CPP) -c $(CFLAGS) $(INCLUDE) $(SWI_ABPL) -o $(OBJ_ABPL)

$(OBJ_BID) :
	$(CPP) -c $(CFLAGS) $(INCLUDE) $(SWI_BID) -o $(OBJ_BID)

$(OBJ_BIDO) :
	$(CPP) -c $(CFLAGS) $(INCLUDE) $(SWI_BIDOP) -o $(OBJ_BIDO)

$(AUDIODATA) : $(OBJ_ABPL) $(OBJ_BID) $(OBJ_BIDO) Makefile
	$(CPP) -shared $(LIB) $(OBJ_ABPL) $(OBJ_BID) $(OBJ_BIDO) -o $(AUDIODATA) 

clean :
	rm *.o *.so



