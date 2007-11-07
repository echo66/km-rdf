/**
	Prolog module to work with audiodata extracted from the audiosource module. This module has been separated from swilib and swiaudiosource
	to get them as loosely as possible for later interaction with another modules
	David Pastor 2007
*/

/**
	The predicates are defined in framepl.cpp and mopl.cpp.
		mopl.cpp adapts swilib/swimo to prolog to get data from MO:elements by using MO::GET::functions
		framepl.cpp is in charge of framing. As framing can be performed anywhere, we prefer to have it out of audiosource or any other module
*/

:- module(audiodata,[get_sample_rate/2,
			get_channels/2,
			get_frame/4]).

:- style_check(-discontiguous).
:- load_foreign_library(swiaudiodata).
 
/**
	get_channels(+Signal or Frame, -Channels)

*/

/**
	get_frame(+Signal, +Start, +End, -Frame). Signal is a MO:signal from which this predicate extracts a MO:frame for the specific start and
	end samples. This should be called iterativelly to get the frames for further processing.
	Check out aspl.cpp for the source code
*/
