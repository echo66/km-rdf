# Makefile for swiaudiosource
# David Pastor Escuredo 2007

CC = gcc # C compile
CPP = g++ # C++ compiler

CFLAGS = -Wall -fPIC

#######################################################################
# Define your platform-specific options here. For non-linux machines, some more arrangements should be done!
#######################################################################

swi_dir = /usr/local/lib/pl-5.6.50

swiaudio_dir = /home/david/km-rdf/swiaudiosource

swilib_dir = /home/david/km-rdf/swilib

SWAUDIOSOURCE_LIB = $(swiaudiosource_dir)/swiaudiosource.so

#######################################################################

#Libraries

LIB_MAD = /usr/lib/libmad.so \
	/usr/lib/libQtCore.so \
	$(swilib_dir)/swimo.so \
	$(swilib_dir)/swiaudioblob.so \
	$(swilib_dir)/blobid.so

LIB_OGGVORBIS = /usr/lib/libfishsound.so \
		/usr/lib/liboggz.so \
		$(swilib_dir)/swimo.so \
		$(swilib_dir)/swiaudioblob.so\
		$(swilib_dir)/blobid.so

LIB_SNDFILE = /usr/lib/libsndfile.so \
		$(swilib_dir)/swimo.so \
		$(swilib_dir)/swiaudioblob.so\
		$(swilib_dir)/blobid.so

LIB_FAAD = /usr/lib/libfaad.so \
		/usr/lib/libmp4ff.so \
		$(swilib_dir)/swimo.so \
		$(swilib_dir)/swiaudioblob.so\
		$(swilib_dir)/blobid.so

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

INCLUDE_SWIFAAD = $(INCLUDE) \
		-I$(swiaudio_dir)/
			
#Sources
SWI_MAD = swimad.cpp
SWI_OGGVORBIS = swioggvorbis.cpp
SWI_SNDFILE = swisndfile.cpp
SWI_UTILS = aspl.cpp
SWI_FAAD = swifaad.cpp
SWI_AACDECODE = faadDecode.cpp

#Objects
OBJ_SWIMAD = swimad.o
OBJ_SWIOGGVORBIS = swioggvorbis.o
OBJ_SWISNDFILE = swisndfile.o
OBJ_UTILS = aspl.o
OBJ_SWIFAAD = swifaad.o
OBJ_AACDECODE = swiaacdecode.o

#DLIB
FAAD_SWI = swifaad.so
AUDIOSOURCE_SWI = swiaudiosource.so

#Targets
all : $(AUDIOSOURCE_SWI)

$(OBJ_UTILS) :
	$(CPP) -c $(CFLAGS) $(INCLUDE) $(SWI_UTILS) -o $(OBJ_UTILS)

$(OBJ_SWIOGGVORBIS) : 
	$(CPP) -c $(CFLAGS) $(INCLUDE_SWIOGGVORBIS) $(SWI_OGGVORBIS) -o $(OBJ_SWIOGGVORBIS)

$(OBJ_SWIMAD) :
	$(CPP) -c $(CFLAGS) $(INCLUDE_SWIMAD) $(SWI_MAD) -o $(OBJ_SWIMAD)

$(OBJ_SWIFAAD) :
	$(CPP) -c $(CFLAGS) $(INCLUDE_SWIFAAD) $(SWI_FAAD) -o $(OBJ_SWIFAAD)

$(OBJ_SWISNDFILE) : 
	$(CPP) -c $(CFLAGS) $(INCLUDE_SWISNDFILE) $(SWI_SNDFILE) -o $(OBJ_SWISNDFILE)

$(OBJ_AACDECODE) :
	$(CPP) -c $(CFLAGS) $(INCLUDE_SWIFAAD) $(SWI_AACDECODE) -o $(OBJ_AACDECODE)

$(FAAD_SWI) : $(OBJ_AACDECODE) $(OBJ_SWIFAAD)
	$(CPP) --shared $(LIB_FAAD) $(OBJ_SWIFAAD) $(OBJ_AACDECODE) -o $(FAAD_SWI)

$(AUDIOSOURCE_SWI) : $(OBJ_SWIOGGVORBIS) $(OBJ_SWISNDFILE) $(OBJ_SWIMAD) $(OBJ_UTILS) $(OBJ_AACDECODE) $(OBJ_SWIFAAD) Makefile
	$(CPP) -shared $(LIB_MAD) $(LIB_OGGVORBIS) $(LIB_FAAD) $(LIB_SNDFILE) $(OBJ_UTILS) $(OBJ_SWIOGGVORBIS) $(OBJ_AACDECODE) $(OBJ_SWIFAAD) $(OBJ_SWISNDFILE) $(OBJ_SWIMAD) -o $(AUDIOSOURCE_SWI) 


clean :
	rm *.o *.so



