# Makefile for 
# David Pastor Escuredo 2007

CC = gcc # C compile
CPP = g++ # C++ compiler

CFLAGS = -Wall -fPIC

#######################################################################
# Define your platform-specific options here
#######################################################################

swi_dir = /usr/local/lib/pl-5.6.50

swilib_dir = .

lib_suf = .so
mac_lib_suf = .dylib
obj_suf = o
#######################################################################

#Headers
INCLUDE= 		-I$(swi_dir)/include/ \
			-I/usr/include/qt4/Qt/ \
			-I/usr/include/qt4/ \
			-I$(swilib_dir)/
			
#Library

LIB = /usr/lib/libQtCore.so 

#Sources
SWI_AUDIOBLOB = swiaudioblob.cpp
B_ID = blobid.cpp

#Objects

OBJ_AUDIOBLOB = swiaudioblob.$(obj_suf)
OBJ_BI = blobid.$(obj_suf)

#DLIB

AUDIOBLOB = swiaudioblob.so
BLOBID = blobid.so

#Targets
all : $(AUDIOBLOB) $(BLOBID)

$(OBJ_AUDIOBLOB) :
	$(CPP) -c $(CFLAGS) $(INCLUDE) $(SWI_AUDIOBLOB) -o $(OBJ_AUDIOBLOB)

$(OBJ_BI) :
	$(CPP) -c $(CFLAGS) $(INCLUDE) $(B_ID) -o $(OBJ_BI)

$(AUDIOBLOB) : $(OBJ_AUDIOBLOB) Makefile
	$(CPP) -shared $(OBJ_AUDIOBLOB) -o $(AUDIOBLOB) 

$(BLOBID) : $(OBJ_BI) Makefile
	$(CPP) -shared $(LIB) $(OBJ_BI) -o $(BLOBID)

clean :
	rm *.o *.so



