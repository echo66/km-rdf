# Makefile for swiaudiosource
# David Pastor Escuredo 2007

CC = gcc # C compile
CPP = g++ # C++ compiler

CFLAGS = -Wall -fPIC

#######################################################################
# Define your platform-specific options here. For non-linux machines, some more arrangements should be done!
#######################################################################

swi_dir = /usr/local/lib/pl-5.6.34

swiaudio_dir = /home/david/km-rdf/swiaudiosource

swilib_dir = /home/david/km-rdf/swilib

SWAUDIOSOURCE_LIB = $(swiaudiosource_dir)/swiaudiosource.so

#######################################################################

#Libraries

LIB_MAD = /usr/lib/libmad.so \
	/usr/lib/libQtCore.so \
	$(swilib_dir)/swimo.so \
	$(swilib_dir)/swiaudioblob.so

LIB_OGGVORBIS = /usr/lib/libfishsound.so \
		/usr/lib/liboggz.so \
		$(swilib_dir)/swimo.so \
		$(swilib_dir)/swiaudioblob.so

LIB_SNDFILE = /usr/lib/libsndfile.so \
		$(swilib_dir)/swimo.so \
		$(swilib_dir)/swiaudioblob.so

#Headers
INCLUDE= 		-I$(swi_dir)/include/ \
			-I/usr/include/qt4/Qt/ \
			-I/usr/include/qt4/ \
			-I$(swilib_dir)/

INCLUDE_SWIMAD = $(INCLUDE)

INCLUDE_SWIOGGVORBIS =  $(INCLUDE) \
			-I/usr/include/fishsound/ \
			-I/usr/include/oggz/

INCLUDE_SWISNDFILE = $(INCLUDE) 
			
#Sources
SWI_MAD = swimad.cpp
SWI_OGGVORBIS = swioggvorbis.cpp
SWI_SNDFILE = swisndfile.cpp
SWI_UTILS = aspl.cpp

#Objects
OBJ_SWIMAD = swimad.o
OBJ_SWIOGGVORBIS = swioggvorbis.o
OBJ_SWISNDFILE = swisndfile.o
OBJ_UTILS = aspl.o

#DLIB

AUDIOSOURCE_SWI = swiaudiosource.so

#Targets
all : $(AUDIOSOURCE_SWI)

$(OBJ_UTILS) :
	$(CPP) -c $(CFLAGS) $(INCLUDE) $(SWI_UTILS) -o $(OBJ_UTILS)

$(OBJ_SWIOGGVORBIS) : 
	$(CPP) -c $(CFLAGS) $(INCLUDE_SWIOGGVORBIS) $(SWI_OGGVORBIS) -o $(OBJ_SWIOGGVORBIS)

$(OBJ_SWIMAD) :
	$(CPP) -c $(CFLAGS) $(INCLUDE_SWIMAD) $(SWI_MAD) -o $(OBJ_SWIMAD)

$(OBJ_SWISNDFILE) : 
	$(CPP) -c $(CFLAGS) $(INCLUDE_SWISNDFILE) $(SWI_SNDFILE) -o $(OBJ_SWISNDFILE)

$(AUDIOSOURCE_SWI) : $(OBJ_SWIOGGVORBIS) $(OBJ_SWISNDFILE) $(OBJ_SWIMAD) $(OBJ_UTILS) Makefile
	$(CPP) -shared $(LIB_MAD) $(LIB_OGGVORBIS) $(LIB_SNDFILE) $(OBJ_UTILS) $(OBJ_SWIOGGVORBIS) $(OBJ_SWISNDFILE) $(OBJ_SWIMAD)  -o $(AUDIOSOURCE_SWI) 


clean :
	rm *.o *.so



