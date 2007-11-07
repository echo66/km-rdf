/**
	Prolog module to work with audiodata extracted from the audiosource module
	David Pastor 2007
*/



:- module(audiodata,[get_sample_rate/2,
			get_channels/2,
			get_frame/4,
			get_frame_timestamp/2]).

:- style_check(-discontiguous).
:- load_foreign_library(swiaudiodata).
 
/**
	get_sample_rate(+Signal or Frame, -SampleRate). Check mopl.cpp

*/

/**
	get_channels(+Signal or Frame, -Channels). Check mopl.cpp

*/

/**
	get_frame(+Signal, +Start, +End, -Frame). Signal is a MO:signal from which this predicate extracts a MO:frame for the specific start and
	end samples. This should be called iterativelly to get the frames for further processing.
	Check out framepl.cpp for the source code
*/

/**
	get_timestamp(+Frame, -Timestamp). Gets the timestamp of a frame
	Check out framepl.cpp for the source code
*/
