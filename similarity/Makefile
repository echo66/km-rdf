# Makefile for similarity module.
# David Pastor Escuredo 2007

CC = gcc # C compile
CPP = g++ # C++ compiler

CFLAGS = -Wall -fPIC

#######################################################################
# Define your platform-specific options here
#######################################################################


sim_dir = .
swilib_dir = ../swilib

SIM_LIB = $(sim_dir)/similarity.$(lib_suf)

#######################################################################

#Libraries

LIBS_SIM = $(swilib_dir)/blobid.$(lib_suf) \
	$(swilib_dir)/swiaudioblob.$(lib_suf) \
	$(qt_lib) 
		
#Headers
INCLUDE_SIM = -I$(swi_dir)/include/ \
	$(qt_include) \
	-I$(swilib_dir)/ \
	-I$(sim_dir)/

#Sources

SB_SIM = sbsimilarity.cpp

#Objects

OBJ_SBSIM = sbsimilarity.$(obj_suf)


#DLIB

SWI_SIM = similarity.$(lib_suf)

#Targets
all : $(SWI_SIM)

$(OBJ_SBSIM) : 
	$(CPP) -c $(CFLAGS) $(INCLUDE_SIM) $(SB_SIM) -o $(OBJ_SBSIM)

$(SWI_SIM) : $(OBJ_SBSIM) Makefile
	$(CPP) $(dynlib_com) $(LIBS_SIM) $(OBJ_SBSIM) -o $(SWI_SIM)

clean :
	rm -f *.$(obj_suf) *.$(lib_suf) 



