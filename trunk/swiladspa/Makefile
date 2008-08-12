# Makefile for swiladspa
# David Pastor Escuredo 2008

CC = gcc # C compile
CPP = g++ # C++ compiler

CFLAGS = -Wall -fPIC

#######################################################################
# Define your SWI-Prolog path here
#######################################################################

swiladspa_dir = .
swilib_dir = ../swilib

#######################################################################

#Libraries

LIBS_SWIL = 	$(swilib_dir)/swiaudioblob.$(lib_suf) \
		$(swilib_dir)/blobid.$(lib_suf) \
		$(qt_lib)

#Headers
INCLUDE_SWIL = -I$(swi_dir)/include \
	-I$(swiladspa_dir) \
	-I$(swilib_dir) \
        $(qt_include)

#Sources

L_DB = swiladspa.cpp
L_LOAD = LADSPALoader.cpp
L_HOST = LADSPAHost.cpp
L_PLUG = LADSPAPlugin.cpp

#Objects

OBJ_LDB = swiladspa.$(obj_suf)
OBJ_LLOAD = loader.$(obj_suf)
OBJ_LHOST = host.$(obj_suf)
OBJ_LPLUG = plug.$(obj_suf)

#DLIB

SWI_L = swiladspa.$(lib_suf)

#Targets
all : $(SWI_L)

$(OBJ_LDB) : 
	$(CPP) -c $(CFLAGS) $(INCLUDE_SWIL) $(L_DB) -o $(OBJ_LDB)

$(OBJ_LPLUG) : 
	$(CPP) -c $(CFLAGS) $(INCLUDE_SWIL) $(L_PLUG) -o $(OBJ_LPLUG)

$(OBJ_LLOAD) : 
	$(CPP) -c $(CFLAGS) $(INCLUDE_SWIL) $(L_LOAD) -o $(OBJ_LLOAD)

$(OBJ_LHOST) : 
	$(CPP) -c $(CFLAGS) $(INCLUDE_SWIL) $(L_HOST) -o $(OBJ_LHOST)

$(SWI_L) : $(OBJ_LDB) $(OBJ_LLOAD) $(OBJ_LHOST) $(OBJ_LPLUG) Makefile
	$(CPP) $(dynlib_com) $(LIBS_SWIL) $(OBJ_LLOAD) $(OBJ_LDB) $(OBJ_LHOST) $(OBJ_LPLUG) -o $(SWI_L) 

clean :
	rm -f *.$(obj_suf) *.$(lib_suf) 



