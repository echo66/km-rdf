# Makefile for 
# David Pastor Escuredo 2007

CC = gcc # C compile
CPP = g++ # C++ compiler

CFLAGS = -Wall -fPIC

#######################################################################
# Define your platform-specific options here
#######################################################################

swi_dir = /usr/local/lib/pl-5.6.46

swilib_dir = /home/david/swilib

lib_suf = .so
obj_suf = .o
#######################################################################

#Headers
INCLUDE= 		-I$(swi_dir)/include/ \
			-I/usr/include/qt4/Qt/ \
			-I/usr/include/qt4/ \
			-I$(swilib_dir)/
			
#Sources
SWI_AUDIOBLOB = swiaudioblob.cpp
SWI_MO = swimo.cpp

#Objects

OBJ_AUDIOBLOB = swiaudioblob.o
OBJ_MO = swimo.o

#DLIB

MO = swimo.so
AUDIOBLOB = swiaudioblob.so

#Targets
all : $(MO) $(AUDIOBLOB) $(AUDIODATA)

$(OBJ_MOPL) :
	$(CPP) -c $(CFLAGS) $(INCLUDE) $(SWI_MOPL) -o $(OBJ_MOPL)

$(OBJ_FRAME) :
	$(CPP) -c $(CFLAGS) $(INCLUDE) $(SWI_FRAME) -o $(OBJ_FRAME)

$(OBJ_MO) :
	$(CPP) -c $(CFLAGS) $(INCLUDE) $(SWI_MO) -o $(OBJ_MO)

$(OBJ_AUDIOBLOB) :
	$(CPP) -c $(CFLAGS) $(INCLUDE) $(SWI_AUDIOBLOB) -o $(OBJ_AUDIOBLOB)

$(MO) : $(OBJ_MO) Makefile
	$(CPP) -shared $(OBJ_MO) -o $(MO) 

$(AUDIOBLOB) : $(OBJ_AUDIOBLOB) Makefile
	$(CPP) -shared $(OBJ_AUDIOBLOB) -o $(AUDIOBLOB) 

clean :
	rm *.o *.so



