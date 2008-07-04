# Makefile for swivamp: Interface to wrap Vamp plugins into SWI-Prolog
# David Pastor Escuredo 2007

CC = gcc # C compile
CPP = g++ # C++ compiler

CFLAGS = -Wall -fPIC

#######################################################################
# Define your SWI-Prolog path here
#######################################################################

swi_dir = /usr/local/lib/pl-5.6.50

#######################################################################

swivamp_dir = .
vamp_dir = vamp-sdk
vamp_host_dir = vamp-sdk/hostext
swilib_dir = ../swilib

SWIVAMP_LIB = $(swivamp_dir)/vampswi.so

#######################################################################

#Libraries

LIBS_SWIVAMP = 	$(vamp_dir)/libvamp-hostsdk.so \
		$(swilib_dir)/swiaudioblob.so \
		$(swilib_dir)/blobid.so

#Headers
INCLUDE_SWIVAMP = -I$(swi_dir)/include \
	-I/usr/include/qt4/Qt \
	-I/usr/include/qt4 \
	-I$(vamp_dir) \
	-I$(vamp_host_dir) \
	-I$(swilib_dir) \
	-I$(swivamp_dir)

#Sources

VAMP_HOST = swivamphost.cpp
VAMP_LIB = swivamp.cpp
VAMP_PLUGIN = swivampplugin.cpp

#Objects

OBJ_SWIVAMP = swivamp.o
OBJ_VAMPHOST = swivamphost.o
OBJ_VAMPPLUGIN = swivampplugin.o

#DLIB

SWI_VAMP = swivamp.so

#Targets
all : $(SWI_VAMP)

$(OBJ_VAMPHOST) : 
	$(CPP) -c $(CFLAGS) $(INCLUDE_SWIVAMP) $(VAMP_HOST) -o $(OBJ_VAMPHOST)

$(OBJ_VAMPPLUGIN) : 
	$(CPP) -c $(CFLAGS) $(INCLUDE_SWIVAMP) $(VAMP_PLUGIN) -o $(OBJ_VAMPPLUGIN)

$(OBJ_SWIVAMP) : 
	$(CPP) -c $(CFLAGS) $(INCLUDE_SWIVAMP) $(VAMP_LIB) -o $(OBJ_SWIVAMP)

$(SWI_VAMP) : $(OBJ_SWIVAMP) $(OBJ_VAMPHOST) $(OBJ_VAMPPLUGIN) Makefile
	$(CPP) -shared $(LIBS_SWIVAMP) $(OBJ_SWIVAMP) $(OBJ_VAMPHOST) $(OBJ_VAMPPLUGIN) -o $(SWI_VAMP) 

clean :
	rm -f *.o *.so 



