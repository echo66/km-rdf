# Makefile for swilib 
# David Pastor Escuredo 2007
# Works fine for Linux

CC = gcc # C compile
CPP = g++ # C++ compiler

CFLAGS = -Wall -fPIC

#######################################################################
# Define your platform-specific options here
#######################################################################

swilib_dir = .

#######################################################################

#Headers
INCLUDE= 		-I$(swi_dir)/include/ \
			$(qt_include) \
			-I$(swilib_dir)/
			
#Library

LIB = $(qt_lib)

#Sources
SWI_AUDIOBLOB = swiaudioblob.cpp
B_ID = blobid.cpp

#Objects

OBJ_AUDIOBLOB = swiaudioblob.o
OBJ_BI = blobid.o

#DLIB

AUDIOBLOB = swiaudioblob.$(lib_suf)
BLOBID = blobid.$(lib_suf)

#Targets
all : $(AUDIOBLOB) $(BLOBID)

$(OBJ_AUDIOBLOB) :
	$(CPP) -c $(CFLAGS) $(INCLUDE) $(SWI_AUDIOBLOB) -o $(OBJ_AUDIOBLOB)

$(OBJ_BI) :
	$(CPP) -c $(CFLAGS) $(INCLUDE) $(B_ID) -o $(OBJ_BI)

$(AUDIOBLOB) : $(OBJ_AUDIOBLOB) Makefile
	$(CPP) $(dynlib_com) $(LIB) $(OBJ_AUDIOBLOB) -o $(AUDIOBLOB) 

$(BLOBID) : $(OBJ_BI) Makefile
	$(CPP) $(dynlib_com) $(LIB) $(OBJ_AUDIOBLOB) $(OBJ_BI) -o $(BLOBID)

clean :
	rm *.$(obj_suf) *.$(lib_suf)



