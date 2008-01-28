# Makefile for similarity module.
# David Pastor Escuredo 2007

CC = gcc # C compile
CPP = g++ # C++ compiler

CFLAGS = -Wall -fPIC

#######################################################################
# Define your platform-specific options here
#######################################################################

swi_dir = /usr/local/lib/pl-5.6.34

sim_dir = /home/david/km-rdf/similarity
swilib_dir = /home/david/km-rdf/swilib

SIM_LIB = $(sim_dir)/similarity.so

#######################################################################

#Libraries

LIBS_SIM = $(swilib_dir)/swimo.so \
	$(swilib_dir)/blobid.so \
	$(swilib_dir)/swiaudioblob.so 
		
#Headers
INCLUDE_SIM = -I$(swi_dir)/include/ \
	-I/usr/include/qt4/Qt/ \
	-I/usr/include/qt4/ \
	-I$(swilib_dir)/ \
	-I$(sim_dir)/

#Sources

SB_SIM = sbsimilarity.cpp

#Objects

OBJ_SBSIM = sbsimilarity.o


#DLIB

SWI_SIM = similarity.so

#Targets
all : $(SWI_SIM)

$(OBJ_SBSIM) : 
	$(CPP) -c $(CFLAGS) $(INCLUDE_SIM) $(SB_SIM) -o $(OBJ_SBSIM)

$(SWI_SIM) : $(OBJ_SBSIM) Makefile
	$(CPP) -shared $(LIBS_SIM) $(OBJ_SBSIM) -o $(SWI_SIM)

clean :
	rm -f *.o *.so 



