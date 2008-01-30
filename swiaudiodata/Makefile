# Makefile for 
# David Pastor Escuredo 2007

CC = gcc # C compile
CPP = g++ # C++ compiler

CFLAGS = -Wall -fPIC

#############################################################
# Define your platform-specific options here
#############################################################

swi_dir = /usr/local/lib/pl-5.6.34

swiaudiodata_dir = /home/david/km-rdf/swiaudiodata
swilib_dir = /home/david/km-rdf/swilib

lib_suf = .so
obj_suf = .o
#######################################################################

#Headers
INCLUDE= 		-I$(swi_dir)/include/ \
			-I/usr/include/qt4/Qt/ \
			-I/usr/include/qt4/ \
			-I$(swilib_dir)/
			
#Libraries

LIB = $(swilib_dir)/swimo.so \
	$(swilib_dir)/swiaudioblob.so \
	$(swilib_dir)/blobid.so

#Sources

SWI_MOPL = mopl.cpp
SWI_FRAME = framepl.cpp
SWI_ABPL = audioblobpl.cpp
SWI_BID = blobidpl.cpp
SWI_MIX = mixer.cpp

#Objects

OBJ_MOPL = mopl.o
OBJ_FRAME = framepl.o
OBJ_ABPL = abpl.o
OBJ_BID = blobid.o
OBJ_MIX = mixer.o

#DLIB

AUDIODATA = swiaudiodata.so

#Targets
all : $(AUDIODATA)

$(OBJ_MIX) :
	$(CPP) -c $(CFLAGS) $(INCLUDE) $(SWI_MIX) -o $(OBJ_MIX)

$(OBJ_MOPL) :
	$(CPP) -c $(CFLAGS) $(INCLUDE) $(SWI_MOPL) -o $(OBJ_MOPL)

$(OBJ_FRAME) :
	$(CPP) -c $(CFLAGS) $(INCLUDE) $(SWI_FRAME) -o $(OBJ_FRAME)

$(OBJ_ABPL) :
	$(CPP) -c $(CFLAGS) $(INCLUDE) $(SWI_ABPL) -o $(OBJ_ABPL)

$(OBJ_BID) :
	$(CPP) -c $(CFLAGS) $(INCLUDE) $(SWI_BID) -o $(OBJ_BID)

$(AUDIODATA) : $(OBJ_FRAME) $(OBJ_MOPL) $(OBJ_ABPL) $(OBJ_BID) $(OBJ_MIX) Makefile
	$(CPP) -shared $(LIB) $(OBJ_MOPL) $(OBJ_FRAME) $(OBJ_ABPL) $(OBJ_BID) $(OBJ_MIX) -o $(AUDIODATA) 

clean :
	rm *.o *.so



