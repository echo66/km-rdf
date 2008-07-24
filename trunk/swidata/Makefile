# Makefile for swidata
# David Pastor Escuredo 2008
# Works fine for Linux

CC = gcc # C compile
CPP = g++ # C++ compiler

CFLAGS = -Wall -fPIC

#############################################################
# Define your platform-specific options here
#############################################################

swiaudiodata_dir = .
swilib_dir = ../swilib

#######################################################################

#Headers
INCLUDE= 		-I$(swi_dir)/include/ \
			-I/usr/include/qt4/Qt/ \
			-I/usr/include/qt4/ \
			-I$(swilib_dir)/
			
#Libraries

LIB = 	$(swilib_dir)/swiaudioblob.$(lib_suf) \
	$(swilib_dir)/blobid.$(lib_suf)

#Sources

SWI_ABPL = audioblobpl.cpp
SWI_BID = blobidDB.cpp
SWI_BIDOP = blobidOperations.cpp

#Objects

OBJ_ABPL = abpl.$(obj_suf)
OBJ_BID = blobid.$(obj_suf)
OBJ_BIDO = blobidop.$(obj_suf)

#DLIB

AUDIODATA = swidata.$(lib_suf)

#Targets
all : $(AUDIODATA)

$(OBJ_ABPL) :
	$(CPP) -c $(CFLAGS) $(INCLUDE) $(SWI_ABPL) -o $(OBJ_ABPL)

$(OBJ_BID) :
	$(CPP) -c $(CFLAGS) $(INCLUDE) $(SWI_BID) -o $(OBJ_BID)

$(OBJ_BIDO) :
	$(CPP) -c $(CFLAGS) $(INCLUDE) $(SWI_BIDOP) -o $(OBJ_BIDO)

$(AUDIODATA) : $(OBJ_ABPL) $(OBJ_BID) $(OBJ_BIDO) Makefile
	$(CPP) $(dynlib_com) $(LIB) $(OBJ_ABPL) $(OBJ_BID) $(OBJ_BIDO) -o $(AUDIODATA) 

clean :
	rm *.$(obj_suf) *.$(lib_suf)



