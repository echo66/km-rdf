# Makefile for swivamp
# David Pastor Escuredo 2008
# Works fine for Linux, not tested for MAC OS/X nor Windows

CC = gcc # C compile
CPP = g++ # C++ compiler

CFLAGS = -Wall -fPIC

#######################################################################
# Define your SWI-Prolog path here
#######################################################################


#######################################################################

swivamp_dir = .
vamp_dir = vamp-sdk
vamp_host_dir = vamp-sdk/hostext
swilib_dir = ../swilib

suf = so
SWIVAMP_LIB = $(swivamp_dir)/vampswi.$(lib_suf)


#######################################################################

#Libraries

LIBS_SWIVAMP = 	$(vamp_dir)/libvamp-hostsdk.$(lib_suf) \
		$(swilib_dir)/swiaudioblob.$(lib_suf) \
		$(swilib_dir)/blobid.$(lib_suf)

#Headers
INCLUDE_SWIVAMP = -I$(swi_dir)/include \
	-I$(vamp_dir) \
	$(qt_include) \
	-I$(vamp_host_dir) \
	-I$(swilib_dir) \
	-I$(swivamp_dir)

#Sources

VAMP_HOST = swivamphost.cpp
VAMP_LIB = swivamp.cpp
VAMP_PLUGIN = swivampplugin.cpp

#Objects

OBJ_SWIVAMP = swivamp.$(obj_suf)
OBJ_VAMPHOST = swivamphost.$(obj_suf)
OBJ_VAMPPLUGIN = swivampplugin.$(obj_suf)

#DLIB

SWI_VAMP = swivamp.$(lib_suf)

#Targets
all : $(SWI_VAMP)

$(OBJ_VAMPHOST) : 
	$(CPP) -c $(CFLAGS) $(INCLUDE_SWIVAMP) $(VAMP_HOST) -o $(OBJ_VAMPHOST)

$(OBJ_VAMPPLUGIN) : 
	$(CPP) -c $(CFLAGS) $(INCLUDE_SWIVAMP) $(VAMP_PLUGIN) -o $(OBJ_VAMPPLUGIN)

$(OBJ_SWIVAMP) : 
	$(CPP) -c $(CFLAGS) $(INCLUDE_SWIVAMP) $(VAMP_LIB) -o $(OBJ_SWIVAMP)

$(SWI_VAMP) : $(OBJ_SWIVAMP) $(OBJ_VAMPHOST) $(OBJ_VAMPPLUGIN) Makefile
	$(CPP) $(dynlib_com) $(LIBS_SWIVAMP) $(OBJ_SWIVAMP) $(OBJ_VAMPHOST) $(OBJ_VAMPPLUGIN) -o $(SWI_VAMP) 

clean :
	rm -f *.$(obj_suf) *.$(lib_suf) 



