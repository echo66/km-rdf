#SWI-DSP makefile
#David Pastor 2008


##ONLY LINUX BUILD. This sets the libraries in local paths. May put everything somewhere else?

############# PLATFORM DEPENDENCIES ############
export lib_suf:= so
#lib_suf = dynlib
#lib_suf = dll

export obj_suf:= o

export dynlib_com:= -shared
#dynlib_com = -dynamiclib

############# YOUR SWI PATH ###############
export swi_dir:= /usr/local/lib/pl-5.6.50

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



