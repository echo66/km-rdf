#Identifiers for large data objects

# Introduction #

DSP tasks involve the management of arbitrary large data objects such as the PCM of an audio signal channel decoded from an audio file or the chromagram bins of an audio frame.

These data objects are too difficult to handle using prolog lists which may even crash trying to capture a whole audio signal for example. Swi-Prolog provides special terms to capture binary representation of data in memory, the so-call BLOBS (PL\_blob\_t). These Blobs succeed in wrapping large data but they are really cumbersome to deal with them and give rise to some compatibility problems with other modules.

We develop within this project a mechanism to manage data objects under atomic identifiers. These identifiers are generated each time a vector of data is exported to prolog and stored in a runtime database. This mechanism is defined in swilib.

The automatic and preferred format for identifiers is data\_x which varies incrementally. This format is encouraged to be suitable to represent a bnode in semantic web expressions. Other ids can be assigned by the user though

# SWIDATA MODULE #

Thus, the logic interface to this mechanism allow us to treat each data object as an atom which is a name of the data object at least during the session in execution. Identifiers can be related to the data object along session by keeping a correspondence with the external file with the dumped data.

Module predicates specification can be found at [data.pl](http://code.google.com/p/km-rdf/source/browse/trunk/swidata/DOC/index.html?r=445). Some examples of this mechanism are:

data\_list('data\_4', L).
L = [4.5, 6.45, 9.87, 3.56, ...]

data\_out('data\_4', 'myfile').

concat\_blobs('data\_6', 'data3434', Concat).
Concat = data\_342143

blob\_frame('_data\_423', 20, 60, Frame).
Frame =__data\_3424_

# LIMITATIONS #

1. Each module MUST BE implemented to read these atoms and get the data from the runtime database

2. The database my collapse if the session involves too many operations during a long time

3. Data lists and data identifiers should be treated in the same way and be transparent for the case. It is not completely true in practise.

4. The binary files in disk where data is dumped are not written in any standard, surely portable format.