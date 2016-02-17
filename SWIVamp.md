#SWI-Prolog module for audio features extraction using Vamp Plugins.

# Introduction #

This is a SWI-Prolog module built on the foreign interface and the CPP package for SWI-Prolog. It provides a communication layer with Vamp Plugins for querying and computations triggering under Prolog predicates. This module defines a stable Vamp Host that controls the plugin lifecycle as defined in the Vamp API. For more information about Vamp Plugins please visit the site www.vamp-plugins.org (Chris Cannam).

The module provides also some higher level predicates defining procedures for feature-extraction and a "Transform" predicate that wraps the feature extraction process for Digital Signal Processing worflows creation.

# Details #

This module forms part of SWI-DSP, a logic-based engine that allow DSP workflows creation.  The input data is provided by SWIAudio and the data is represented using the BLOBIDs mechanism described in this Wiki.

The instantiation patterns can be consulted in the generated source by loading a module and calling doc\_server/1.

We have defined a Prolog functor to represent Vamp::Feature objects:

feature(Type, timestamp(Start, Duration), Data)

where Data is a prolog list for Sparse Outputs and a BLOBIDs for dense outputs. The duration of the timestamp is still a bit "tricky" as there is not specific duration assigned by the plugin, so it still requires correct interpretation (See Vamp doc to check relationships between SampleTypes and timestamps).

These terms are retrieved forming complex lists which gathers the features extracted for an input data framing for each of the outputs queried.

# Installation #

To run swivamp from Prolog, you need to set and compile the following modules in your machine

-swilib: a C++ library to handle large data objects by using atomic identifiers: BLOBIDs
-swidata: a SWI-Prolog module to handle BLOBIDs
-swiaudio: a SWI-Prolog module thar provides "signal" terms decoding a wide range of audio files
-Download the vamp-sdk from the vamp plugin site and set the folder in your swivamp path
-Download vamp plugins libraries and set them in one of the preferred plugins paths

# Running #

This Prolog host is developed in two submodules: vamp and vamp\_transform. The first can be loaded from the Prolog prompt and allows interactive communication with the Vamp Plugins, so the user can control the plugin lifecycle and call any plugin method from the prompt.

The latter builds a transform/6 predicate on top of vamp.pl that hides the actual procedure followed to extract vamp features. The user can easily and efficiently retrieve outputs as a complex list of feature terms.

To check modules specification, call doc\_server/1 after loading the module.

# Status #

At this point, this version 1.0 is stable and works for existing plugins using the sdk 1.2. There is still some ongoing work to provide a better and clearer front-end of the transform predicate and structure of the plugin outputs