# Makefile for swivamp: Interface to wrap Vamp plugins into SWI-Prolog
# David Pastor Escuredo 2007

CC = gcc # C compile
CPP = g++ # C++ compiler

CFLAGS = -Wall -fPIC

#######################################################################
# Define your platform-specific options here
#######################################################################

swi_dir = /usr/local/lib/pl-5.6.34

swivamp_dir = /home/david/km-rdf/swivamp
vamp_dir = /home/david/vamp-plugin-sdk-v1.1pre1
#It is not necessary to set the vamp_dir as the library is included in swivamp 
swilib_dir = /home/david/km-rdf/swilib

SWIVAMP_LIB = $(swivamp_dir)/vampswi.so

#######################################################################

#Libraries

LIBS_SWIVAMP = $(swilib_dir)/swimo.so \
		$(vamp_dir)/vamp-sdk/libvamp-hostsdk.so \
		$(swilib_dir)/swiaudioblob.so

#Headers
INCLUDE_SWIVAMP = -I$(swi_dir)/include/ \
	-I/usr/include/qt4/Qt/ \
	-I/usr/include/qt4/ \
	-I$(vamp_dir)/vamp-sdk/ \
	-I$(vamp_dir)/vamp-sdk/hostext/ \
	-I$(vamp_dir)/ \
	-I$(swilib_dir)/

#Objects

OBJ_SWIVAMP = swivamp.o

#DLIB

SWI_VAMP = swivamp.so

#Targets
all : $(SWI_VAMP)

$(OBJ_SWIVAMP) : 
	$(CPP) -c $(CFLAGS) $(INCLUDE_SWIVAMP) swivamp.cpp -o $(OBJ_SWIVAMP)

$(SWI_VAMP) : $(OBJ_SWIVAMP) Makefile
	$(CPP) -shared $(LIBS_SWIVAMP) $(OBJ_SWIVAMP) -o $(SWI_VAMP) 

clean :
	rm -f *.o *.so 



