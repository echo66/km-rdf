# Makefile for C4DML and SoundBite interfaces to SWI-Prolog
# Copyright Yves Raimond (c) 2006


CC = gcc # C compile
CPP = g++ # C++ compiler

CFLAGS = -Wall -fPIC

#######################################################################
# Define your platform-specific options here
#######################################################################

swi_dir = /usr/local/lib/pl-5.6.46

c4dml_dir = /home/moustaki/work/workspace/c4dml

C4DML_LIB = $(c4dml_dir)/src/libc4dm.so

#######################################################################


#Libraries

LIBS_C4DML = 

#Headers
INCLUDE_SWI = -I$(swi_dir)/include/
INCLUDE_C4DML = -I$(c4dml_dir)/utils \
	-I$(c4dml_dir)/io \
	-I$(c4dml_dir)/features \
	-I$(c4dml_dir)/misc \
	$(INCLUDE_SWI)

#Objects

OBJ_C4DML = c4dmlswi.o

#DLIB

C4DML_SWI = c4dmlswi.so

#Targets
all : $(C4DML_SWI)

$(OBJ_C4DML) : 
	$(CC) -c $(CFLAGS) $(INCLUDE_C4DML) c4dmlswi.c -o $(OBJ_C4DML)

$(C4DML_SWI) : $(OBJ_C4DML) Makefile
	$(CC) -shared $(C4DML_LIB) $(OBJ_C4DML) -o $(C4DML_SWI)


clean :
	rm -f $(OBJ_C4DML)  $(C4DML_SWI)


