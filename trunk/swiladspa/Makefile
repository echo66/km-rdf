# Makefile for swiladspa
# David Pastor Escuredo 2008

CC = gcc # C compile
CPP = g++ # C++ compiler

CFLAGS = -Wall -fPIC

#######################################################################
# Define your SWI-Prolog path here
#######################################################################

swi_dir = /usr/local/lib/pl-5.6.50

#######################################################################

swiladspa_dir = .
swilib_dir = ../swilib

#######################################################################

#Libraries

LIBS_SWIL = $(swilib_dir)/swimo.so \
		$(swilib_dir)/swiaudioblob.so \
		$(swilib_dir)/blobid.so

#Headers
INCLUDE_SWIL = -I$(swi_dir)/include \
	-I$(swiladspa_dir) \
	-I/usr/include/qt4/Qt \
	-I/usr/include/qt4 \
	-I$(swilib_dir) 

#Sources

L_DB = swiladspa.cpp
L_LOAD = LADSPALoader.cpp
L_HOST = LADSPAHost.cpp
L_PLUG = LADSPAPlugin.cpp

#Objects

OBJ_LDB = swiladspa.o
OBJ_LLOAD = loader.o
OBJ_LHOST = host.o
OBJ_LPLUG = plug.o

#DLIB

SWI_L = swiladspa.so

#Targets
all : $(SWI_L)

$(OBJ_LDB) : 
	$(CPP) -c $(CFLAGS) $(INCLUDE_SWIL) $(L_DB) -o $(OBJ_LDB)

$(OBJ_LPLUG) : 
	$(CPP) -c $(CFLAGS) $(INCLUDE_SWIL) $(L_PLUG) -o $(OBJ_PLUG)

$(OBJ_LLOAD) : 
	$(CPP) -c $(CFLAGS) $(INCLUDE_SWIL) $(L_LOAD) -o $(OBJ_LLOAD)

$(OBJ_LHOST) : 
	$(CPP) -c $(CFLAGS) $(INCLUDE_SWIL) $(L_HOST) -o $(OBJ_LHOST)

$(SWI_L) : $(OBJ_LDB) $(OBJ_LLOAD) $(OBJ_LHOST) $(OBJ_PLUG) Makefile
	$(CPP) -shared $(LIBS_SWIL) $(OBJ_LLOAD) $(OBJ_LDB) $(OBJ_LHOST) $(OBJ_PLUG) -o $(SWI_L) 

clean :
	rm -f *.o *.so 



