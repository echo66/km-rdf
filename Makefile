#SWI-DSP makefile
#David Pastor 2008

## It works fine in Linux and possible well in MAC OS/X 

############# YOUR SWI PATH ###############
export swi_dir:= /usr/local/lib/pl-5.6.50

#Your path to the libpl
#export libpl_path:= /home/david/master/lib

### You may need to set paths to your other libraries, mainly qt

############# PLATFORM DEPENDENCIES ############
export lib_suf:= so
#lib_suf = dynlib
#lib_suf = dll

export obj_suf:= o

export dynlib_com:= -shared
#export dynlib_com = -dynamiclib $(libpl_path)/libpl.$(lib_suf)

############# YOUR SWI PATH ###############
export swi_dir:= /usr/local/lib/pl-5.6.50

#export swi_lib_dir:-
# for mac os/x u need to link the libpl library
all: swidsp

swidata:
	cd swilib && $(MAKE)
	cd swidata && $(MAKE)

swiaudio:
	cd swilib && $(MAKE)
	cd swidata && $(MAKE)
	cd swiaudio && $(MAKE)

swivamp::
	cd swilib && $(MAKE)
	cd swidata && $(MAKE)
	cd swiaudio && $(MAKE)
	cd swivamp && $(MAKE)

swidsp::
	cd swilib && $(MAKE)
	cd swidata && $(MAKE)
	cd swiaudio && $(MAKE)
	cd swivamp && $(MAKE)
	cd similarity && $(MAKE)
	#install-swiladspa


clean :
	cd swilib && rm -f *.o *.$(lib_suf)
	cd swidata && rm -f *.o *.$(lib_suf)
	cd swiaudio && rm -f *.o *.$(lib_suf)
	cd swivamp && rm -f *.o *.$(lib_suf)
	cd similarity && rm -f *.o *.$(lib_suf)



