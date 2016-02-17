#Module for audio files decoding

# Introduction #

This module provides audio signal representations and functions to deal with them. The audio elements are represented by SWI-Prolog functors: signal(SampleRate, ListOfData), frame(SampleRate, Start, ListOfData) and timestamp(Start, Duration).

This interface provides a decode/2 predicate which decodes audio files returning a "signal term" common for every file format. These signal terms can be manipulated with predicates defined in this module and serve for communication with other modules that analyse audio information (see km-rdf general doc)

The file extensions supported:

.wav, .aif, .aiff, .mp3, .ogg with vorbis encoding, .m4a and other AAC encoded formats (AAC decoder is only stable for the Linux built)

# Details #

This module relies on external open source libraries which communicates with Prolog by the foreign libraries imported by this module. These libraries are MAD, FISHSOUND, OGGZ, FAAD and SOUNDFILE.

The data is managed using the swidata module and the BLOBIDs mechanism (see wiki page).

# Install #

Download audiopl.tar.gz and built each of the libraries in it. Download and build the named decoding libraries and load the audio module from the SWI-Prolog prompt.

It works fine for Linux, you may find some problems to build with MAC and it has not been tested in Windows