# Makefile for swivamp: Interface to wrap Vamp plugins into SWI-Prolog
# David Pastor Escuredo 2007

CC = gcc # C compile
CPP = g++ # C++ compiler

CFLAGS = -Wall -v

#######################################################################
# Define your platform-specific options here
#######################################################################

swi_dir = /usr/local/lib/pl-5.6.34

java_dir = /usr/lib/jvm/java-6-sun/jre/lib/i386

swiweka_dir = /home/david/swiweka
SWIWEKA_LIB = $(swiweka_dir)/libswiweka.so

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

OBJ_SWIWEKA = swiweka.o

#DLIB

SWI_WEKA = libswiweka.so

#Targets
all : $(SWI_WEKA)

$(OBJ_SWIVAMP) : 
	$(CPP) -c $(CFLAGS) $(INCLUDE_SWIVAMP) swivamp.cpp -o $(OBJ_SWIVAMP)

#This procedure links vampswi.o to the vamp-sdk library for hosts
#$(VAMP_SWI) : $(OBJ_SWIVAMP) Makefile
#	$(CPP) -static -shared -o $(VAMP_SWI) $(OBJ_SWIVAMP) $(LIBS_SWIVAMP) $(VAMPHOSTSDK_LIB) 

$(SWI_WEKA) : 
	$(CPP) -shared $(java_dir)/libjava.so -o $(SWI_WEKA) 

#The vamp library could be accessed also from the vamp_dir

clean :
	rm -f $(OBJ_SWIVAMP) 



