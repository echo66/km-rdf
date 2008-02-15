# Makefile for 
# David Pastor Escuredo 2007

CC = gcc # C compile
CPP = g++ # C++ compiler

CFLAGS = -Wall -fPIC

#######################################################################
# Define your platform-specific options here
#######################################################################

swi_dir = /usr/local/lib/pl-5.6.50

swilib_dir = /home/david/km-rdf/swilib

lib_suf = .so
obj_suf = .o
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
SWI_MO = swimo.cpp
B_ID = blobid.cpp

#Objects

OBJ_AUDIOBLOB = swiaudioblob.o
OBJ_MO = swimo.o
OBJ_BI = blobid.o

#DLIB

MO = swimo.so
AUDIOBLOB = swiaudioblob.so
BLOBID = blobid.so

#Targets
all : $(AUDIOBLOB) $(MO) $(BLOBID)

$(OBJ_MO) :
	$(CPP) -c $(CFLAGS) $(INCLUDE) $(SWI_MO) -o $(OBJ_MO)

$(OBJ_AUDIOBLOB) :
	$(CPP) -c $(CFLAGS) $(INCLUDE) $(SWI_AUDIOBLOB) -o $(OBJ_AUDIOBLOB)

$(OBJ_BI) :
	$(CPP) -c $(CFLAGS) $(INCLUDE) $(B_ID) -o $(OBJ_BI)

$(MO) : $(OBJ_MO) Makefile
	$(CPP) -shared $(OBJ_MO) -o $(MO)

$(AUDIOBLOB) : $(OBJ_AUDIOBLOB) Makefile
	$(CPP) -shared $(OBJ_AUDIOBLOB) -o $(AUDIOBLOB) 

$(BLOBID) : $(OBJ_BI) Makefile
	$(CPP) -shared $(LIB) $(OBJ_BI) -o $(BLOBID)

clean :
	rm *.o *.so



