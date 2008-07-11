# Makefile for swiaudio 
# David Pastor Escuredo 2007
# Works fine for Linux, not tested for MAC OS/X nor Windows

CC = gcc # C compile
CPP = g++ # C++ compiler

CFLAGS = -Wall -fPIC

#######################################################################
# Define your platform-specific options here. For non-linux machines, some more arrangements should be done!
#######################################################################

swiaudio_dir = .

swilib_dir = ../swilib

SWAUDIOSOURCE_LIB = $(swiaudiosource_dir)/swiaudio.$(lib_suf)

#######################################################################

#Libraries

LIB_MAD = /usr/lib/libmad.$(lib_suf) \
	$(qt_lib) \
	$(swilib_dir)/blobid.$(lib_suf)

LIB_OGGVORBIS = /usr/lib/libfishsound.$(lib_suf) \
		/usr/lib/liboggz.$(lib_suf) \
		$(swilib_dir)/blobid.$(lib_suf)

LIB_SNDFILE = /usr/lib/libsndfile.$(lib_suf) \
		$(swilib_dir)/blobid.$(lib_suf)

LIB_FAAD = /usr/lib/libfaad.$(lib_suf) \
		/usr/lib/libmp4ff.$(lib_suf) \
		$(swilib_dir)/blobid.$(lib_suf)

#Headers
INCLUDE= 		-I$(swi_dir)/include/ \
			$(qt_include) \
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
OBJ_SWIMAD = swimad.$(obj_suf)
OBJ_SWIOGGVORBIS = swioggvorbis.$(obj_suf)
OBJ_SWISNDFILE = swisndfile.$(obj_suf)
OBJ_UTILS = aspl.$(obj_suf)
OBJ_SWIFAAD = swifaad.$(obj_suf)
OBJ_AACDECODE = swiaacdecode.$(obj_suf)

#DLIB
FAAD_SWI = swifaad.$(lib_suf)
AUDIOSOURCE_SWI = swiaudio.$(lib_suf)

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
	$(CPP) -$(dynlib_com) $(LIB_FAAD) $(OBJ_SWIFAAD) $(OBJ_AACDECODE) -o $(FAAD_SWI)

$(AUDIOSOURCE_SWI) : $(OBJ_SWIOGGVORBIS) $(OBJ_SWISNDFILE) $(OBJ_SWIMAD) $(OBJ_UTILS) $(OBJ_AACDECODE) $(OBJ_SWIFAAD) Makefile
	$(CPP) $(dynlib_com) $(LIB_MAD) $(LIB_OGGVORBIS) $(LIB_FAAD) $(LIB_SNDFILE) $(OBJ_UTILS) $(OBJ_SWIOGGVORBIS) $(OBJ_AACDECODE) $(OBJ_SWIFAAD) $(OBJ_SWISNDFILE) $(OBJ_SWIMAD) -o $(AUDIOSOURCE_SWI) 


clean :
	rm *.$(obj_suf) *.$(lib_suf)



