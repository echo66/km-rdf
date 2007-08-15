# Makefile for swivamp: Interface to wrap Vamp plugins into SWI-Prolog
# David Pastor Escuredo 2007

CC = gcc # C compile
CPP = g++ # C++ compiler

CFLAGS = -Wall -v

#######################################################################
# Define your platform-specific options here
#######################################################################

swi_dir = /usr/local/lib/pl-5.6.39

swivamp_dir = /home/moustaki/eclipse_workspace/km-rdf/swivamp/
vamp_dir = /home/moustaki/eclipse_workspace/km-rdf/swivamp/vamp-plugin-sdk-v1.1pre1/
#It is not necessary to set the vamp_dir as the library is included in swivamp 

SWIVAMP_LIB = $(swivamp_dir)/vampswi.so

#######################################################################

#Libraries

LIBS_SWIVAMP = -L$(vamp_dir)/vamp-sdk/
VAMPHOSTSDK_LIB = -lvamp-hostsdk

SNDFILE_LIB = -lsndfile
SNDFILE_PATH = /usr/lib

#Headers
INCLUDE_SWIVAMP = -I$(swi_dir)/include/ \
	-I$(vamp_dir)/vamp-sdk/ \
	-I$(vamp_dir)/vamp-sdk/hostext/ \
	-I$(vamp_dir)/

#Objects

OBJ_SWIVAMP = swivamp.o

#DLIB

SWI_VAMP = swivamp.so

#Targets
all : $(SWI_VAMP)

$(OBJ_SWIVAMP) : 
	$(CPP) -c $(CFLAGS) $(INCLUDE_SWIVAMP) swivamp.cpp -o $(OBJ_SWIVAMP)

#This procedure links vampswi.o to the vamp-sdk library for hosts
#$(VAMP_SWI) : $(OBJ_SWIVAMP) Makefile
#	$(CPP) -static -shared -o $(VAMP_SWI) $(OBJ_SWIVAMP) $(LIBS_SWIVAMP) $(VAMPHOSTSDK_LIB) 

$(SWI_VAMP) : $(OBJ_SWIVAMP) Makefile
	$(CPP) -shared $(vamp_dir)/vamp-sdk/libvamp-hostsdk.so $(SNDFILE_PATH)/libsndfile.so $(OBJ_SWIVAMP) -o $(SWI_VAMP) 

#The vamp library could be accessed also from the vamp_dir

clean :
	rm -f $(OBJ_SWIVAMP) 



