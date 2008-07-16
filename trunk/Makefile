#SWI-DSP makefile
#David Pastor 2008

## It works fine in Linux and possible well in MAC OS/X 

############# YOUR SWI PATH ###############
export swi_dir:= /usr/local/lib/pl-5.6.50

#Your path to the libpl (not necessary for Linux)
#export libpl_path:= /home/david/master/lib/libpl.so


### QT LIB ### try to remove it

export qt_lib:= /usr/lib/libQtCore.so
export qt_include:= -I/usr/include/qt4/Qt/ \
			-I/usr/include/qt4/

#export qt_include:= -I/usr/local/Trolltech/Qt-4.4.0/include/QtCore/ -I/usr/local/Trolltech/Qt-4.4.0/include/

############# PLATFORM DEPENDENCIES ############
export lib_suf:= so
#lib_suf = dynlib
#lib_suf = dll

export obj_suf:= o

export dynlib_com:= -shared
#export dynlib_com = -dynamiclib $(libpl_path)/libpl.dylib

###################################################
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



