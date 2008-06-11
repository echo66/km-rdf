# Makefile

CC = gcc # C compile
CPP = g++ # C++ compiler

CFLAGS = -Wall -fPIC

#######################################################################

swi_dir = /usr/local/lib/pl-5.6.50

generator_dir= .
vamp_dir = ../../../vamp-plugin-sdk/vamp-sdk
vamp_host_dir = vamp-sdk/hostext

#######################################################################

#Libraries

LIB_VAMP = $(vamp_dir)/libvamp-hostsdk.so 
	

#Headers
INCLUDE = -I$(swi_dir)/include \
	-I/usr/include/qt4/Qt \
	-I/usr/include/qt4 \
	-I$(vamp_dir) \
	-I$(vamp_host_dir) \

#Sources

GENERATOR = template_generator.cpp

#Objects

GENER = template_generator.o

#DLIB

TGENERATOR = template_generator.so

#Targets
all : $(TGENERATOR)

$(GENER) : 
	$(CPP) -c $(CFLAGS) $(INCLUDE) $(GENERATOR) -o $(GENER)

$(TGENERATOR) : $(GENER) Makefile
	$(CPP) -shared $(LIB_VAMP) $(GENER) -o $(TGENERATOR) 

clean :
	rm -f *.o *.so 



