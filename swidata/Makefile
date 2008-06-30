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

LIB = $(swilib_dir)/swimo.so \
	$(swilib_dir)/swiaudioblob.so \
	$(swilib_dir)/blobid.so

#Sources

SWI_ABPL = audioblobpl.cpp
SWI_BID = blobidpl.cpp

#Objects

OBJ_ABPL = abpl.o
OBJ_BID = blobid.o

#DLIB

AUDIODATA = swidata.so

#Targets
all : $(AUDIODATA)

$(OBJ_FRAME) :
	$(CPP) -c $(CFLAGS) $(INCLUDE) -o $(OBJ_FRAME)

$(OBJ_ABPL) :
	$(CPP) -c $(CFLAGS) $(INCLUDE) $(SWI_ABPL) -o $(OBJ_ABPL)

$(OBJ_BID) :
	$(CPP) -c $(CFLAGS) $(INCLUDE) $(SWI_BID) -o $(OBJ_BID)

$(AUDIODATA) : $(OBJ_ABPL) $(OBJ_BID) Makefile
	$(CPP) -shared $(LIB) $(OBJ_ABPL) $(OBJ_BID) -o $(AUDIODATA) 

clean :
	rm *.o *.so



